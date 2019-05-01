{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module TreeTide.UnderHood.KytheApi.Convert
    ( kytheUriToParts
    , CorpusRootPath(..)
    , kytheUriToDirectoryRequest
    , kytheUriFromDirectoryRequest
    )
where

import           Protolude

import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Network.URI
import           Network.HTTP.Types.URI         ( parseQueryText )

import           TreeTide.UnderHood.KytheApi

kytheUriToDirectoryRequest :: KytheUri -> Maybe DirectoryRequest
kytheUriToDirectoryRequest k = do
    CorpusRootPath {..} <- kytheUriToParts k
    let corpus' = fromMaybe (Corpus "") crpCorpus
    return $! DirectoryRequest { corpus'DirReq = corpus'
                               , root'DirReq   = crpRoot
                               , path'DirReq   = crpPath
                               }

data CorpusRootPath = CorpusRootPath
    { crpCorpus :: Maybe Corpus
    , crpRoot :: Maybe Root
    , crpPath :: Maybe Path
    }

-- TODO see https://kythe.io/docs/kythe-uri-spec.html, parse more.
kytheUriToParts :: KytheUri -> Maybe CorpusRootPath
kytheUriToParts (KytheUri raw) = do
    uri <- parseURI . toS . fixKytheUri $ raw
    guard (uriScheme uri == "kythe:")
    let mbCorpus = Corpus . toS . uriRegName <$> uriAuthority uri
        kvs      = parseQueryText . toS . uriQuery $ uri
        mbPath   = Path <$> join (L.lookup "path" kvs)
        mbRoot   = Root <$> join (L.lookup "root" kvs)
    return $! CorpusRootPath mbCorpus mbRoot mbPath

kytheUriFromDirectoryRequest :: DirectoryRequest -> KytheUri
kytheUriFromDirectoryRequest DirectoryRequest {..} =
    KytheUri $ "kythe://" <> unCorpus corpus'DirReq <> mconcat
        (catMaybes
            [ param "root" (unRoot <$> root'DirReq)
            , param "path" (unPath <$> path'DirReq)
            ]
        )
  where
    param :: Text -> Maybe Text -> Maybe Text
    param p =
        fmap
            ( (("?" <> p <> "=") <>)
            . toS
            . escapeURIString isUnescapedInURI
            . toS
            )

-- | Kythe URI follows RFC and uses ? as top-level separators, while
-- contemporary URI libs expect & as non-first separator.
fixKytheUri :: Text -> Text
fixKytheUri t = case T.splitOn "?" t of
    (hd : rest) -> hd <> "?" <> T.intercalate "&" rest
    _           -> t
