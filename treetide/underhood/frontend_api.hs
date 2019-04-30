{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators #-}
module TreeTide.UnderHood.FrontendApi
    ( XRefApi
    , XRefReply(..)
    , Site(..)
    --
    , FileTreeApi
    , Subtree(..)
    , TreeEntryKind(..), treeEntryIsDir
    --
    , SourceApi
    --
    , DecorApi
    , DecorReply(..)
    , Decor(..)
    , CmPoint(..), CmRange(..)
    --
    , DocApi
    , DocReply(..)
    )
where

import           Protolude

import           Data.Aeson.Types
import           Data.Text                      ( Text, toLower )
import           Servant.API

type FileTreeApi = "filetree" :> Get '[JSON] Subtree

data Subtree = Subtree
    { kytheUri :: Text
      -- ^ Note: for merged (that is directory) nodes this will be present,
      -- but randomly selected from the merged ones. Needed on client side
      -- to provide a unique id (and let's use the kythe uri, why not).
    , display :: Text
      -- ^ The text to display on the tree UI. The name of the dir/file.
    , treeEntryKind :: TreeEntryKind
    , onlyGenerated :: Bool
      -- ^ True if the subtree contains generated files only.
    , children :: [Subtree]
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data TreeEntryKind
    = Directory
    | File
    | GeneratedFile
    deriving (Eq, Ord, Show, Generic)

instance ToJSON TreeEntryKind where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = toS . toLower . toS }
instance FromJSON TreeEntryKind where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = toS . toLower . toS }

treeEntryIsDir :: TreeEntryKind -> Bool
treeEntryIsDir k = case k of
    Directory -> True
    File -> False
    GeneratedFile -> False

-- TODO send the byte index in the source where a pageful of stuff can be
--   shown, for faster CodeMirror loading (first load the pageful, and then
--   the full source).
type SourceApi = "source"
    :> QueryParam "ticket" Text
    :> QueryParam "preview" Int
    :> Get '[JSON] Text

type DecorApi = "decor" :> QueryParam "ticket" Text :> Get '[JSON] DecorReply

data DecorReply = DecorReply
    { decors :: [Decor]
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Decor = Decor
    { dTarget :: Text
    , dStart :: CmPoint  -- TODO CmRange
    , dEnd :: CmPoint
    , dSpan :: Maybe Int
      -- ^ The span size of the anchor.
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data CmPoint = CmPoint
    { line :: Int
    , ch :: Int
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data CmRange = CmRange
    { from :: CmPoint
    , to :: CmPoint
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

--

type DocApi = "doc"
    :> QueryParam "ticket" Text
    :> Get '[JSON] DocReply

data DocReply = DocReply
    { docText :: Maybe Text
      -- ^ TODO unescape escaped [] brackets (they are Kythe link delimiters
      -- if not escaped), and likely other stuff.

      -- TODO plumb type string. OTOH MarkedText doesn't include tick xrefs..
      --   so might need to mine xrefs from the type tick? Or maybe docs api
      --   provides a way to query node info. Maybe not.
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

--

type XRefApi = "xref"
    :> QueryParam "ticket" Text
    :> Get '[JSON] XRefReply

data XRefReply = XRefReply
    { refs :: [Site]
    , refCount :: Int
    , definitions :: [Site]
    , declarations :: [Site]
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- TODO caller fn for calls
data Site = Site
    { sFileTicket :: Text  -- TODO newtype KytheUri
    --, sDisplayName :: Text
    , sSnippet :: Text
    , sSnippetSpan :: CmRange
    , sSpan :: CmRange
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

