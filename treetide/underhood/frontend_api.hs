{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators #-}
module TreeTide.UnderHood.FrontendApi
    ( XRefApi
    , XRefReply(..)
    , Site(..)
    , CallContext(..)
    , DisplayedFile(..)
    , Snippet(..)
    --
    , InfoApi
    , InfoReply(..)
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
    , dEdge :: Int
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

type InfoApi = "info"
    :> QueryParam "ticket" Text
    :> Get '[JSON] InfoReply

data InfoReply = InfoReply
    { bindings :: [Text]
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

type XRefApi = "xref"
    :> QueryParam "ticket" Text
    :> Get '[JSON] XRefReply

data XRefReply = XRefReply
    { refs :: [Site]
      -- ^ Current page of references.
    , refCount :: Int
      -- ^ Total (maybe approximate) count of refs.
    , calls :: [CallContext]
      -- ^ Current page of call contexts.
    , callCount :: Int
      -- ^ Total (maybe approximate) count of call-contexts.
    , definitions :: [Site]
    , declarations :: [Site]
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- | Info about source-code occurrence of some entity.
-- The occurrence can be usage, definition or declaration (to name some).
data Site = Site
    { sContainingFile :: DisplayedFile
      -- ^ The containing file, can be used to show and navigate there.
    , sSnippet :: Snippet
      -- ^ How the entity is used.
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data CallContext = CallContext
    { ccContextSite :: Site
      -- ^ Info about the calling context (for example function definition).
    , ccContextTicket :: Text
      -- ^ Can chain callsite lookup using this ticket.
    , ccSites :: [Snippet]
      -- ^ Snippets about usage within the context.
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- | File to be displayed on the UI.
data DisplayedFile = DisplayedFile
    { dfFileTicket :: Text
      -- ^ Ticket of the file.
    , dfDisplayName :: Text
      -- ^ How should the filename be displayed.
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- | Contains the snippet of the source code where an entity occurs.
data Snippet = Snippet
    { snippetText :: Text
      -- ^ The snippet itself, containing the usage occurrence.
      -- Could be multiline, but in practice it is not.
    , snippetFullSpan :: CmRange
      -- ^ The full span of the given snippet.
    , snippetOccurrenceSpan :: CmRange
      -- ^ The span of the occurrence (falls within the snippet's span).
      -- Can be used for highlighting it.
    }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

