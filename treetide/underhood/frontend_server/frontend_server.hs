{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import           Protolude               hiding ( groupBy )

import           Control.Monad.Free
import qualified Data.ByteString.Base64        as Base64
import           Data.Default                   ( def )
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import           Network.HTTP.Client            ( Manager
                                                , newManager
                                                , defaultManagerSettings
                                                )
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Servant
import           Servant.Client                 ( ClientM
                                                , runClientM
                                                , mkClientEnv
                                                , BaseUrl(..)
                                                , Scheme(Http)
                                                )
import           Text.Groom                     ( groom )
import           Text.Show.Deriving             ( deriveShow1 )

import qualified Options.Applicative.Extended  as O

import qualified TreeTide.UnderHood.FrontendApi
                                               as Api
import           TreeTide.UnderHood.FrontendServer.Types
import qualified TreeTide.UnderHood.KytheApi   as K
import qualified TreeTide.UnderHood.KytheApi.Convert
                                               as K

newtype DisplayName = DisplayName Text
    deriving (Eq, Ord, Show)

data DirF a = DirF K.DirectoryRequest DisplayName [a]
    deriving (Eq, Ord, Functor)

deriveShow1 'DirF

data FileRef = FileRef K.KytheUri DisplayName
    deriving (Eq, Ord, Show)

type FileTree = Free DirF FileRef

newtype ParentPath = ParentPath K.Path
    deriving (Eq, Ord, Show)

queryDirs :: ClientM [[FileTree]]
queryDirs = do
    K.CorpusRootsReply crs <- getCorpusRoots
    mapM fetchCorpusTree crs
  where
    fetchCorpusTree (K.CorpusRoots c rs) = mapM
        (\r -> subtree
            (DisplayName $ maybe "<corpus>" K.unCorpus c)
            K.DirectoryRequest
                { K.corpus'DirReq = fromMaybe (K.Corpus "") c
                , K.root'DirReq = guard (not . T.null . K.unRoot $ r) >> Just r
                , K.path'DirReq = Nothing
                }
        )
        rs

subtree :: DisplayName -> K.DirectoryRequest -> ClientM FileTree
subtree dn req = do
    listing <- getDirectory req
    let (files, dirs) = L.partition
            ((== K.FILE) . K.kind'DirEntry)
            (unMaybeList . K.entry'DirRep $ listing)
        fs = map
            (\f ->
                Pure
                    . flip FileRef (DisplayName $ K.name'DirEntry f)
                    . K.kytheUriFromDirectoryRequest
                    $ appEndo (appendDirReqPath f) req
            )
            files
    ds <-
        mapM
                (\d ->
                    let newReq = appEndo (appendDirReqPath d) req
                    in  subtree (DisplayName $ K.name'DirEntry d) newReq
                )
            $ dirs
    return $! Free $! DirF req dn (ds ++ fs)
  where
    appendDirReqPath :: K.DirEntry -> Endo K.DirectoryRequest
    appendDirReqPath e = Endo $ \r ->
        let p = K.name'DirEntry e
        in  r
                { K.path'DirReq = Just . K.Path $! maybe
                                      p
                                      (\p0 -> p0 <> "/" <> p)
                                      (K.unPath <$> K.path'DirReq r)
                }

isGenerated :: K.KytheUri -> Bool
isGenerated uri =
    let r = K.unRoot <$> (K.crpRoot <=< K.kytheUriToParts) uri
                    -- TODO this is hard-coded, use a more general config.
    in  maybe False ("bazel-out" `T.isPrefixOf`) r

transcodeFileTree :: FileTree -> Api.Subtree
transcodeFileTree ft = case ft of
    Pure (FileRef kUri@(K.KytheUri rawUri) (DisplayName display)) ->
        let generated = isGenerated kUri
        in  Api.Subtree rawUri
                        display
                        (bool Api.File Api.GeneratedFile generated)
                        generated
                        []
    Free (DirF req (DisplayName display) as) -> Api.Subtree
        (K.unKytheUri (K.kytheUriFromDirectoryRequest req))
        display
        Api.Directory
        False  -- correct value filled in merge step
        (map transcodeFileTree as)

data MergeKey = MergeKey
    { mkDisplay :: Text
    , mkIsDir :: Bool
    }
    deriving (Eq, Ord, Show)

mergeTrees :: [Api.Subtree] -> Api.Subtree
mergeTrees ts = Api.Subtree { Api.kytheUri      = "kythe://"  -- virtual root will be discarded on UI anyway
                            , Api.display       = "/"
                            , Api.treeEntryKind = Api.Directory
                            , Api.onlyGenerated = False
                            , Api.children      = go ts
                            }
  where
    realGroupBy :: (Ord b) => (a -> b) -> [a] -> [NonEmpty a]
    realGroupBy f = NE.groupBy ((==) `on` f) . sortOn f
    --
    mergeKey t =
        MergeKey (Api.display t) (Api.treeEntryIsDir . Api.treeEntryKind $ t)
    --
    go :: [Api.Subtree] -> [Api.Subtree]
    go = (NE.toList . mergeGroup) <=< realGroupBy mergeKey
    --
    mergeGroup :: NonEmpty Api.Subtree -> NonEmpty Api.Subtree
    mergeGroup g@(s :| _) = if Api.treeEntryIsDir (Api.treeEntryKind s)
        then
            let cs = go . concatMap Api.children . NE.toList $ g
            in  s { Api.onlyGenerated = all Api.onlyGenerated cs
                  , Api.children      = cs
                  }
                    :| []
        else g

server :: Options -> Manager -> Server ServedApi
server opts manager' =
    serveFileTree :<|> serveSource :<|> serveDecors :<|> serveDoc :<|> serveXRef
  where
    clientEnv = mkClientEnv
        manager'
        (BaseUrl Http (toS $ oKytheHost opts) (oKythePort opts) "")
    --
    serveFileTree = Handler . ExceptT . liftIO $ do
        res <- runClientM queryDirs clientEnv
        let treeRes = bimap
                (const err503 { errBody = "Couldn't get tree." })
                        -- TODO merge or otherwise present trees
                        --(map (map transcodeFileTree))
                (mergeTrees . map transcodeFileTree . concat)
                res
        putStrLn (groom treeRes)
        return $! treeRes
    --
    serveSource :: Server Api.SourceApi
    serveSource mbRawTicket mbPreview =
        Handler . ExceptT . liftIO $ case mbRawTicket of
            Nothing ->
                return . Left $ err503 { errBody = "Missing query parameter." }
            Just rawTicket -> do
                let
                    req = def
                        { K.location'DecReq    =
                            K.Location
                                { K.ticket'L = Just (K.KytheUri rawTicket)
                                }
                        , K.source_text'DecReq = Just True
                        }
                print ("Getting sources" :: Text, req)
                res <- runClientM (getDecorations req) clientEnv
                putStrLn (groom res)
                return
                    $! bimap
                           (const err503 { errBody = "Couldn't get source." })
                           (\rep ->
                               maybe identity
                                     (\c -> T.unlines . take c . T.lines)
                                     mbPreview
                                   $ fromMaybe
                                        -- Note: lot more resiliency + logging should be added here,
                                        -- to use as fallback: lenient base64 decoding, and lenient
                                        -- text decoding. Does lenient decoding interfere with anchor locations? What is the right place in the pipeline to solve this? (iconv at source? etc)
                                         "(Failed to get source)"
                                         (   K.source_text'DecRep rep
                                         >>= ( -- for now use lenient text, but see comments above 
                                              fmap (T.decodeUtf8With T.lenientDecode)
                                             . rightToMaybe
                                             . Base64.decode
                                             . toS
                                             )
                                         )
                           )
                           res
    --
    serveDecors :: Server Api.DecorApi
    serveDecors mbRawTicket = Handler . ExceptT . liftIO $ case mbRawTicket of
        Nothing ->
            return . Left $ err503 { errBody = "Missing query parameter." }
        Just rawTicket -> do
            let
                req = def
                    { K.location'DecReq   =
                        K.Location { K.ticket'L = Just (K.KytheUri rawTicket) }
                    , K.references'DecReq = Just True
                    }
            print ("Getting decors" :: Text, req)
            res <- runClientM (getDecorations req) clientEnv
            putStrLn (groom res)
            return
                $! bimap
                       (const err503 { errBody = "Couldn't get internal refs." }
                       )
                       transcodeDecorations
                       res
    --
    serveDoc :: Server Api.DocApi
    serveDoc mbRawTicket = Handler . ExceptT . liftIO $ case mbRawTicket of
        Nothing ->
            return . Left $ err503 { errBody = "Missing query parameter." }
        Just rawTicket -> do
            let
                req = K.DocumentationRequest
                    { K.ticket'DocReq = [K.KytheUri rawTicket]
                    }
            res <- runClientM (getDocumentation req) clientEnv
            return
                $! bimap
                       (const err503 { errBody = "Couldn't get documentation." }
                       )
                       transcodeDoc
                       res
    --
    serveXRef :: Server Api.XRefApi
    serveXRef mbRawTicket = Handler . ExceptT . liftIO $ case mbRawTicket of
        Nothing ->
            return . Left $ err503 { errBody = "Missing query parameter." }
        Just rawTicket -> do
            let req = def { K.ticket'CRReq         = [K.KytheUri rawTicket]
                          , K.caller_kind'CRReq    = K.DIRECT_CALLERS
                          , K.reference_kind'CRReq = K.ALL_REFERENCES
                          }
            print ("Getting xref" :: Text, req)
            res <- runClientM (getCrossReferences req) clientEnv
            putStrLn (groom res)
            return
                $! bimap
                       (\e -> err503 { errBody = "Couldn't find xref " <> show e
                                     }
                       )
                       transcodeXRef
                       res

transcodeXRef :: K.CrossReferencesReply -> Api.XRefReply
transcodeXRef rep =
    let
        onlyElem = (head . M.elems) =<< K.cross_references'CRRep rep
        getSites =
            mapMaybe (anchorToSite <=< K.anchor'RA) . relatedAnchorFor onlyElem
        callContexts =
            mapMaybe relatedAnchorToCallContext
                . relatedAnchorFor onlyElem
                $ K.caller'CRS
    in
        Api.XRefReply { Api.refs         = getSites K.reference'CRS
                      , Api.refCount     = getCount K.references'Total
                      , Api.calls        = callContexts
                      , Api.callCount    = getCount K.callers'Total
                      , Api.definitions  = getSites K.definition'CRS
                      , Api.declarations = getSites K.declaration'CRS
                      }
  where
    getCount :: (K.Total -> Maybe Text) -> Int
    getCount selector =
        fromMaybe 0 . ((readMaybe . toS) <=< selector <=< K.total'CRRep) $ rep
    --
    relatedAnchorFor
        :: Maybe K.CrossReferenceSet
        -> (K.CrossReferenceSet -> Maybe [a])
        -> [a]
    relatedAnchorFor s selector = unMaybeList (s >>= selector)
    --
    anchorToDisplayFile :: K.Anchor -> Maybe Api.DisplayedFile
    anchorToDisplayFile a =
        Api.DisplayedFile
            <$> (K.unKytheUri <$> K.parent'A a)
            <*> (formatDisplayedPath <$> (K.kytheUriToParts <=< K.parent'A) a)
      where
        formatDisplayedPath :: K.CorpusRootPath -> Text
        formatDisplayedPath (K.CorpusRootPath c r p) =
            mconcat
                . catMaybes
                $ [ (<> "/") . K.unCorpus <$> c
                  , (<> "/") . K.unRoot <$> r
                  , K.unPath <$> p
                  ]
    --
    anchorToSnippet :: K.Anchor -> Maybe Api.Snippet
    anchorToSnippet a =
        Api.Snippet
            <$> K.snippet'A a
            <*> (span2range <$> K.snippet_span'A a)
            <*> (span2range <$> K.span'A a)
    --
    anchorToSite :: K.Anchor -> Maybe Api.Site
    anchorToSite a = Api.Site <$> anchorToDisplayFile a <*> anchorToSnippet a
    --
    relatedAnchorToCallContext :: K.RelatedAnchor -> Maybe Api.CallContext
    relatedAnchorToCallContext ra =
        Api.CallContext
            <$> (anchorToSite =<< K.anchor'RA ra)
            <*> (K.unKytheUri <$> K.ticket'RA ra)
            <*> pure (mapMaybe anchorToSnippet . unMaybeList . K.site'RA $ ra)

transcodeDoc :: K.DocumentationReply -> Api.DocReply
transcodeDoc rep =
    Api.DocReply
        $ (K.raw_text'Printable <=< K.text'DocRep <=< head <=< K.document'DocRep
          )
              rep

transcodeDecorations :: K.DecorationsReply -> Api.DecorReply
transcodeDecorations =
    Api.DecorReply . map f . fromMaybe [] . K.reference'DecRep
  where
    f r =
        let start  = K.start'Span . K.span'Ref $ r
            end    = K.end'Span . K.span'Ref $ r
            dStart = p2p start
            dEnd   = p2p end
        in  Api.Decor
                { Api.dTarget = K.unKytheUri (K.target_ticket'Ref r)
                , Api.dStart  = dStart
                , Api.dEnd    = dEnd
                , Api.dSpan   = (-)
                                <$> K.byte_offset'P end
                                <*> K.byte_offset'P start
                }
    -- TODO what are the ones missing the col offset? Are they important refs?

p2p :: K.Point -> Api.CmPoint
p2p (K.Point _ ln colOfs) =
    -- seems to be missing sometime, esp. beginning of snippet spans.
    let fixedColOfs = fromMaybe 0 colOfs in Api.CmPoint (ln - 1) fixedColOfs

span2range :: K.Span -> Api.CmRange
span2range (K.Span s e) = (Api.CmRange `on` p2p) s e

data Options = Options
    { oPort :: Int
    , oKytheHost :: Text
    , oKythePort :: Int
    }

options :: O.Parser Options
options =
    Options
        <$> O.defineOption "port" 8081 "Port to listen on."
        <*> O.defineOption "kythe_api_host"
                           "localhost"
                           "Host serving Kythe HTTP API."
        <*> O.defineOption "kythe_api_port" 8080 "Port to acces Kythe HTTP API."

app :: Options -> Manager -> Application
app opts mgr = serve (Proxy :: Proxy ServedApi) (server opts mgr)

main :: IO ()
main = do
    opts     <- O.parseOptionsIO "Frontend API server" options
    manager' <- newManager defaultManagerSettings
    putStrLn @Text "Up&running!" >> run (oPort opts) (app opts manager')

-- * Helpers

unMaybeList :: Maybe [a] -> [a]
unMaybeList = join . maybeToList

