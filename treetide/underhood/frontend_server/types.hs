{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
module TreeTide.UnderHood.FrontendServer.Types
  ( KytheApiUsed
  , getCorpusRoots
  , getCrossReferences
  , getDecorations
  , getDirectory
  , getDocumentation
  --
  , ServedApi
  ) where

import           Servant
import           Servant.Client (ClientM, client)

import qualified TreeTide.UnderHood.FrontendApi as Api
import qualified TreeTide.UnderHood.KytheApi as K

-- Note: see the excellent https://haskell-servant.readthedocs.io/en/stable/
-- about servant.
type KytheApiUsed
    = K.CorpusRootsApi
    :<|> K.DirectoryApi
    :<|> K.DecorationsApi
    :<|> K.DocumentationApi
    :<|> K.CrossReferencesApi

getCorpusRoots :: ClientM K.CorpusRootsReply
getDirectory :: K.DirectoryRequest -> ClientM K.DirectoryReply
getDecorations :: K.DecorationsRequest -> ClientM K.DecorationsReply
getDocumentation :: K.DocumentationRequest -> ClientM K.DocumentationReply
getCrossReferences :: K.CrossReferencesRequest -> ClientM K.CrossReferencesReply
getCorpusRoots :<|> getDirectory :<|> getDecorations :<|> getDocumentation
    :<|> getCrossReferences = client (Proxy :: Proxy KytheApiUsed)

type ServedApi
    = "api" :> (
         Api.FileTreeApi
    :<|> Api.SourceApi
    :<|> Api.DecorApi
    :<|> Api.DocApi
    :<|> Api.XRefApi
    :<|> Api.InfoApi)

