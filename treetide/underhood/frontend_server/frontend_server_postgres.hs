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
{-# OPTIONS_GHC -Wwarn #-}

import           Protolude               hiding ( groupBy )

import Control.Monad.Except (liftEither)
import           Control.Monad.Free
--import qualified Data.ByteString.Base64        as Base64
--import           Data.Default                   ( def )
import qualified Data.List                     as L
import qualified Data.List.NonEmpty            as NE
import Data.Int (Int64)
--import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.Offset as TOfs
import qualified Database.PostgreSQL.Simple.Extended as PG
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Servant
--import           Text.Groom                     ( groom )
import           Text.Show.Deriving             ( deriveShow1 )

import qualified Options.Applicative.Extended  as O

import qualified TreeTide.UnderHood.FrontendApi
                                               as Api
import           TreeTide.UnderHood.FrontendServer.Types
import qualified TreeTide.UnderHood.KytheApi   as K
import qualified TreeTide.UnderHood.KytheApi.Convert
                                               as K

import qualified TreeTide.PgPool as PG

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

-- | Files in this root will be marked as generated.
isGeneratedRoot :: Text -> Bool
isGeneratedRoot r = "bazel-out" `T.isPrefixOf` r

-- | If a root is marked as overlaid, it won't appear explicitly on the UI, but
-- merged with other overlay roots (and the root-less corpus, if any).
isOverlayRoot :: Text -> Bool
isOverlayRoot r = "bazel-out" `T.isPrefixOf` r || T.null r

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

data FileRow = FileRow
    { frCrp :: Int64
    , frCorpus :: Maybe Text
    , frRoot :: Maybe Text
    , frPath :: Text
    }

data Broken = Broken
    { brokenFileRow :: FileRow
    , brokenParts :: [Text]
    }

rowToFileRow :: (Int64, Maybe Text, Maybe Text, Text) -> FileRow
rowToFileRow (a, b, c, d) = FileRow a b c d

hasFilePath :: FileRow -> Bool
hasFilePath = not . T.null . frPath

filesToSubtree :: [(Int64, Maybe Text, Maybe Text, Text)] -> Api.Subtree
filesToSubtree fs =
    let byCorpRoot = groupOn corpRoot . filter hasFilePath . map rowToFileRow $ fs
        sepTrees = map (makeTree . map breakOnSep) byCorpRoot
    in mergeTrees sepTrees
  where
    groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
    groupOn f = L.groupBy ((==) `on` f) . sortBy (comparing f)
    corpRoot x = (frCorpus x, frRoot x)
    breakOnSep :: FileRow -> Broken
    breakOnSep fr = Broken
        { brokenFileRow = fr
        , brokenParts = T.splitOn "/" (frPath fr)
        }
    makeTree :: [Broken] -> Api.Subtree
    makeTree = mergeTrees . map toSingleChain
      where
        toSingleChain b =
            mkTopLevel (\dirReq -> go dirReq (brokenParts b))
          where
            dirReqToUri = K.unKytheUri . K.kytheUriFromDirectoryRequest
            -- | This can be reused between kythe-server.
            appendDirReqPath :: Text -> Endo K.DirectoryRequest
            appendDirReqPath p = Endo $ \r ->
                r
                        { K.path'DirReq = Just . K.Path $! maybe
                                              p
                                              (\p0 -> p0 <> "/" <> p)
                                              (K.unPath <$> K.path'DirReq r)
                        }


            mkTopLevel dirReqToSub =
              let fr = brokenFileRow b
                  mbRoot = frRoot fr
                  renderedCorpus = fromMaybe "<corpus>" (frCorpus fr)
                  dirReq = K.DirectoryRequest
                    { K.corpus'DirReq = K.Corpus (fromMaybe "" (frCorpus fr))
                    , K.root'DirReq = guard (not . T.null . fromMaybe "" $ mbRoot) >> K.Root <$> mbRoot
                    , K.path'DirReq = Nothing
                    }
                  mbSub = dirReqToSub dirReq
              in Api.Subtree
                  { Api.kytheUri = dirReqToUri dirReq
                  , Api.display = case (frRoot . brokenFileRow $ b) of
                      Nothing -> renderedCorpus
                      Just r -> if isOverlayRoot r
                        then renderedCorpus
                        else renderedCorpus <> ":" <> r
                  , Api.treeEntryKind = Api.Directory
                  , Api.onlyGenerated = maybe False Api.onlyGenerated mbSub
                  , Api.children = maybeToList mbSub
                  }
            go :: K.DirectoryRequest -> [Text] -> Maybe Api.Subtree
            go _ [] = Nothing
            go dirReq [fileName] =
                let gen = maybe False isGeneratedRoot (frRoot . brokenFileRow $ b)
                    thisReq = appEndo (appendDirReqPath fileName) dirReq
                in Just $! Api.Subtree
                    { Api.kytheUri = dirReqToUri thisReq
                    , Api.display = fileName
                    , Api.treeEntryKind = bool Api.File Api.GeneratedFile gen
                    , Api.onlyGenerated = gen
                    , Api.children = []
                    }
            go dirReq (d:ps) =
              let thisReq = appEndo (appendDirReqPath d) dirReq
                  sub = go thisReq ps
              in Just $! Api.Subtree
                  { Api.kytheUri = dirReqToUri thisReq
                  , Api.display = d
                  , Api.treeEntryKind = Api.Directory
                  , Api.onlyGenerated = maybe False Api.onlyGenerated sub
                  , Api.children = maybeToList sub
                  }

data HashTicket = HashTicket
    { htCrp :: Int64
    , htSigl :: Int64
    }
    deriving (Eq, Ord, Show)

parseHashTicket :: Text -> Maybe HashTicket
parseHashTicket t = do
    let parts = T.splitOn ":" t
    guard (length parts == 3)
    let [_, tc, ts] = parts
    crp <- readMaybe (toS tc)
    sigl <- readMaybe (toS ts)
    Just $! HashTicket { htCrp = crp, htSigl = sigl }

renderHashTicket :: HashTicket -> Text
renderHashTicket ht = "hash:" <> show (htCrp ht) <> ":" <> show (htSigl ht)

noteMay :: e -> Maybe a -> Either e a
noteMay e Nothing = Left $! e
noteMay _ (Just a) = Right $! a

textToServerError :: ExceptT Text IO a -> ExceptT ServerError IO a
textToServerError = withExceptT (\e -> err503 { errBody = toS e })

server :: Options -> PG.PgPool -> Server ServedApi
server opts pool =
    serveFileTree :<|> serveSource :<|> serveDecors :<|> serveDoc :<|> serveXRef
  where

    kytheUriToCrp :: K.KytheUri -> ExceptT Text IO Int64
    kytheUriToCrp kytheUri = do
        case K.kytheUriToDirectoryRequest kytheUri of
          Nothing -> throwError "Couldn't decode kythe uri"
          Just dirReq -> do
            print dirReq
            res <- liftIO (PG.singleOnly <$> PG.queryPool pool "SELECT crp FROM crp WHERE corpus = ? AND root = ? AND path = ?"
                      (K.unCorpus . K.corpus'DirReq $ dirReq
                      -- can it ever be null for real?
                      ,maybe "" K.unRoot (K.root'DirReq dirReq)
                      ,K.unPath <$> (K.path'DirReq $ dirReq)
                      ))
            case res of
                Nothing -> throwError "Couldn't find crp hash for file."
                Just crp -> pure crp

    fetchCrpContent :: Int64 -> ExceptT Text IO Text
    fetchCrpContent crp = do
        mbBytes <- liftIO (PG.singleOnly <$> PG.queryPool pool "SELECT content FROM file WHERE crp = ?"
                 (PG.Only crp))
        case mbBytes of
            Nothing -> throwError "Couldn't find content for file."
            Just bytes -> pure $ T.decodeUtf8With T.lenientDecode bytes

    --
    serveFileTree = Handler $ do
        files <- liftIO $ PG.queryPool_ pool "SELECT crp, corpus, root, path FROM crp"
        pure (filesToSubtree files)

    serveSource :: Server Api.SourceApi
    serveSource mbRawTicket _mbPreview = Handler . textToServerError $ do
        rawTicket <- liftEither (noteMay "Missing query parameter" mbRawTicket)
        crp <- kytheUriToCrp (K.KytheUri rawTicket)
        fetchCrpContent crp

    serveDecors :: Server Api.DecorApi
    serveDecors mbRawTicket = Handler . textToServerError $ do
        rawTicket <- liftEither (noteMay "Missing query parameter" mbRawTicket)
        -- TODO(robinp): cache line offsets to DB, instead of fetching source
        -- here again?
        crp <- kytheUriToCrp (K.KytheUri rawTicket)
        content <- fetchCrpContent crp
        let tab = TOfs.createOffsetTable (toS content)
        res <- liftIO (PG.queryPool pool "SELECT a.bs, a.be, e.tcrp, e.tsigl FROM anchor a JOIN anchor_edge e USING(crp,sigl) WHERE bs IS NOT NULL AND be IS NOT NULL AND crp = ?" (PG.Only crp))
        -- TODO(robinp): terrible O(n^2) behavior, could just get all distinct
        -- positions in one linear scan.
        let (bads, goods) = partitionEithers (map (rowToDecor tab) res)
        if null bads
            then pure Api.DecorReply { Api.decors = goods }
            else throwError $ "Error converting positions: " <> show bads
      where
        rowToDecor tab (bs::Int, be::Int, tcrp::Int64, tsigl::Int64) = do
          start <- mkPoint tab bs
          end <- mkPoint tab be
          Right $! Api.Decor
              { Api.dTarget = "hash:" <> show tcrp <> ":" <> show tsigl
              , Api.dStart = start
              , Api.dEnd = end
              , Api.dSpan = Nothing
              }
        mkPoint tab bytePos =
            case TOfs.byteOffsetToLineCol tab bytePos of
                Just (TOfs.LineCol l c) -> Right $! Api.CmPoint { Api.line = l, Api.ch = c }
                Nothing -> Left bytePos

    serveDoc :: Server Api.DocApi
    serveDoc mbRawTicket = Handler . ExceptT . liftIO $ case mbRawTicket of
        Nothing ->
            return . Left $ err503 { errBody = "Missing query parameter." }
        Just _rawTicket -> panic "implement docs"

    serveXRef :: Server Api.XRefApi
    serveXRef mbRawTicket = Handler . textToServerError $ do
        rawTicket <- liftEither (noteMay "Missing query parameter" mbRawTicket)
        ht <- liftEither . noteMay "Not a hash ticket" . parseHashTicket $ rawTicket
        res <- liftIO (PG.queryPool pool "SELECT a.crp, a.sigl, a.bs, a.be, e.ekind, crp.corpus, crp.root, crp.path FROM anchor a JOIN anchor_edge e USING(crp,sigl) JOIN crp USING (crp) WHERE bs IS NOT NULL AND be IS NOT NULL AND tcrp = ? AND tsigl = ?" ((htCrp ht, htSigl ht)))
        pure Api.XRefReply
            { Api.refs = map toSite res
            , Api.refCount = length res
            , Api.calls = []
            , Api.callCount = 0
            , Api.definitions = []
            , Api.declarations = []
            }
      where
        toSite (crp::Int64, sigl::Int64, bs::Int, be::Int, ekind::Int, corpus::Text, root::Text, path::Text) =
          let ht = HashTicket { htCrp = crp, htSigl = sigl }
          in Api.Site {
            Api.sContainingFile = Api.DisplayedFile
                { Api.dfFileTicket = renderHashTicket ht
                , Api.dfDisplayName = corpus <> ":" <> root <> "/" <> path
                }
            , Api.sSnippet = Api.Snippet
                { Api.snippetText = "foobar"
                , Api.snippetFullSpan = aSpan
                , Api.snippetOccurrenceSpan = aSpan
                }
          }
        aSpan = Api.CmRange
            { Api.from = Api.CmPoint { Api.line = 1, Api.ch = 1 }
            , Api.to = Api.CmPoint { Api.line = 1, Api.ch = 5 }
            }


data Options = Options
    { oPort :: Int
    , oPgConnString :: Text
    }

options :: O.Parser Options
options =
    Options
        <$> O.defineOption "port" 8081 "Port to listen on."
        <*> O.defineOption "postgres_conn_string"
                           ""
                           "Postgresql connection string, for example postgresql:///kydb?host=/some/dir"

app :: Options -> PG.PgPool -> Application
app opts pool = serve (Proxy :: Proxy ServedApi) (server opts pool)

main :: IO ()
main = do
    opts     <- O.parseOptionsIO "Frontend API server (postgres)" options
    pool <- PG.createPgPool PG.PgPoolOptions { PG.pgpConnString = toS (oPgConnString opts) }
    putStrLn @Text "Up&running!" >> run (oPort opts) (app opts pool)

