{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Kythe.Api.Convert
    ( kytheUriToDirectoryRequest
    , kytheUriFromDirectoryRequest
    )
where

import           Protolude

import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Network.URI
import           Network.HTTP.Types.URI         ( parseQueryText )

import           Kythe.Api

-- TODO see https://kythe.io/docs/kythe-uri-spec.html, parse more, and
-- not directly into DirectoryRequest.
kytheUriToDirectoryRequest :: KytheUri -> Maybe DirectoryRequest
kytheUriToDirectoryRequest (KytheUri raw) = do
    uri <- parseURI . toS . fixKytheUri $ raw
    guard (uriScheme uri == "kythe:")
    let corpus' = toS . maybe "" uriRegName . uriAuthority $ uri
        kvs     = parseQueryText . toS . uriQuery $ uri
        mbPath  = join (L.lookup "path" kvs)
        mbRoot  = join (L.lookup "root" kvs)
    return $! DirectoryRequest { corpus'DirReq = Corpus corpus'
                               , root'DirReq   = Root <$> mbRoot
                               , path'DirReq   = mbPath
                               }

kytheUriFromDirectoryRequest :: DirectoryRequest -> KytheUri
kytheUriFromDirectoryRequest DirectoryRequest {..} =
    KytheUri $ "kythe://" <> unCorpus corpus'DirReq <> mconcat
        (catMaybes
            [param "root" (unRoot <$> root'DirReq), param "path" path'DirReq]
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
