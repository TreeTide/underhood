{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}  -- NOTE maybe DerivingVia to select?

{-# LANGUAGE TypeOperators #-}
module Kythe.Api
    ( CorpusRootsApi
    , CorpusRootsReply(..)
    , CorpusRoots(..)
    --
    , DirectoryApi
    , DirectoryRequest(..)
    , DirectoryReply(..)
    , DirEntry(..)
    , DirEntryKind(..)
    --
    , DecorationsApi
    , DecorationsRequest(..)
    , DecorationsReply(..)
    --
    , DocumentationApi
    , DocumentationRequest(..)
    , DocumentationReply(..)
    , Document(..)
    , Printable(..)
    --
    , CrossReferencesApi
    , CrossReferencesRequest(..)
    , CrossReferencesReply(..)
    , CrossReferenceSet(..)
    , Total(..)
    , RelatedAnchor(..)
    , Anchor(..)
    --
    , Corpus(..)
    , Root(..)
    , KytheUri(..)
    , Location(..)
    , Reference(..)
    , Span(..)
    , Point(..)
    )
where

import           Data.Aeson.Types
import           Data.Default
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Servant.API

import           TreeTide.JsonNaming            ( dropPrimedPostfix
                                                , unwrapNewtype
                                                )

type CorpusRootsApi = "corpusRoots" :> Get '[JSON] CorpusRootsReply

data CorpusRootsReply = CorpusRootsReply
    { corpus'CRRep :: [CorpusRoots]
    }
    deriving (Eq, Ord, Show, Generic)

data CorpusRoots = CorpusRoots
    { name'CR :: Maybe Corpus
    , root'CR :: [Root]
    }
    deriving (Eq, Ord, Show, Generic)

newtype Corpus = Corpus { unCorpus :: Text }
    deriving (Eq, Ord, Show, Generic)

newtype Root = Root { unRoot :: Text }
    deriving (Eq, Ord, Show, Generic)

type DirectoryApi
    = "dir" :> ReqBody '[JSON] DirectoryRequest :> Get '[JSON] DirectoryReply

data DirectoryRequest = DirectoryRequest
    { corpus'DirReq :: Corpus
    , root'DirReq :: Maybe Root
    , path'DirReq :: Maybe Text
    }
    deriving (Eq, Ord, Show, Generic)

data DirectoryReply = DirectoryReply
    { entry'DirRep :: Maybe [DirEntry]
    }
    deriving (Eq, Ord, Show, Generic)

data DirEntry = DirEntry
    { kind'DirEntry :: DirEntryKind
    , name'DirEntry :: Text
    -- TODO(robinp): build_config
    }
    deriving (Eq, Ord, Show, Generic)

data DirEntryKind = FILE | DIRECTORY
    deriving (Eq, Ord, Show, Generic)

newtype KytheUri = KytheUri { unKytheUri :: Text }
    deriving (Eq, Ord, Show, Generic)

--

type DecorationsApi
    = "decorations" :> ReqBody '[JSON] DecorationsRequest :> Get '[JSON] DecorationsReply

data DecorationsRequest = DecorationsRequest
    { location'DecReq :: Location
    , source_text'DecReq :: Maybe Bool
    , references'DecReq :: Maybe Bool
    , target_definitions'DecReq :: Maybe Bool
    , filter'DecReq :: Maybe [Text]
    , diagnostics'DecReq :: Maybe Bool
      -- TODO more
    }
    deriving (Eq, Ord, Show, Generic)

instance Default DecorationsRequest where
    def = DecorationsRequest (Location Nothing)
                             Nothing
                             Nothing
                             Nothing
                             Nothing
                             Nothing

data Location = Location
    { ticket'L :: Maybe KytheUri
      -- TODO more
    }
    deriving (Eq, Ord, Show, Generic)

data DecorationsReply = DecorationsReply
    { source_text'DecRep :: Maybe Text
    , reference'DecRep :: Maybe [Reference]
    }
    deriving (Eq, Ord, Show, Generic)

data Reference = Reference
    { target_ticket'Ref :: KytheUri
    , kind'Ref :: Text
    , span'Ref :: Span
    }
    deriving (Eq, Ord, Show, Generic)

data Span = Span
    { start'Span :: Point
    , end'Span :: Point
    }
    deriving (Eq, Ord, Show, Generic)

data Point = Point
    { byte_offset'P :: Maybe Int
    , line_number'P :: Int
    , column_offset'P :: Maybe Int
    }
    deriving (Eq, Ord, Show, Generic)

--

type DocumentationApi
    = "documentation" :> ReqBody '[JSON] DocumentationRequest :> Get '[JSON] DocumentationReply

data DocumentationRequest = DocumentationRequest
    { ticket'DocReq :: [KytheUri]
      -- TODO more
    }
    deriving (Eq, Ord, Show, Generic)

data DocumentationReply = DocumentationReply
    { document'DocRep :: Maybe [Document]
      -- TODO more
    }
    deriving (Eq, Ord, Show, Generic)

data Document = Document
    { ticket'DocRep :: KytheUri
    , text'DocRep :: Maybe Printable
      -- TODO more
    }
    deriving (Eq, Ord, Show, Generic)

data Printable = Printable
    { raw_text'Printable :: Maybe Text
      -- TODO links
    }
    deriving (Eq, Ord, Show, Generic)

--

type CrossReferencesApi
    = "xrefs" :> ReqBody '[JSON] CrossReferencesRequest :> Get '[JSON] CrossReferencesReply

data CrossReferencesRequest = CrossReferencesRequest
    { ticket'CRReq :: [KytheUri]
    , definition_kind'CRReq :: DefinitionKind
    , declaration_kind'CRReq :: DeclarationKind
    , reference_kind'CRReq :: ReferenceKind
    , caller_kind'CRReq :: CallerKind
    , filter'CRReq :: Maybe [Text]
    -- TODO more
    , page_size'CRReq :: Maybe Int
    , page_token'CRReq :: Maybe Text
    , snippets'CRReq :: SnippetsKind
    }
    deriving (Eq, Ord, Show, Generic)

instance Default CrossReferencesRequest where
    def = CrossReferencesRequest []
                                 BINDING_DEFINITIONS
                                 ALL_DECLARATIONS
                                 ALL_REFERENCES
                                 DIRECT_CALLERS
                                 Nothing
                                 Nothing
                                 Nothing
                                 DEFAULT

data DefinitionKind
    = NO_DEFINITIONS
    | ALL_DEFINITIONS
    | FULL_DEFINITIONS
    | BINDING_DEFINITIONS
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data DeclarationKind
    = NO_DECLARATIONS
    | ALL_DECLARATIONS
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data ReferenceKind
    = NO_REFERENCES
    | CALL_REFERENCES
    | NON_CALL_REFERENCES
    | ALL_REFERENCES
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data CallerKind
    = NO_CALLERS
    | DIRECT_CALLERS
    | OVERRIDE_CALLERS
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data SnippetsKind
    = NONE
    | DEFAULT
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data CrossReferencesReply = CrossReferencesReply
    { total'CRRep :: Maybe Total
    , cross_references'CRRep :: Maybe (Map KytheUri CrossReferenceSet)
    }
    deriving (Eq, Ord, Show, Generic)

data Total = Total
    { definitions'Total :: Maybe Text
    , declarations'Total :: Maybe Text
    , references'Total :: Maybe Text -- Bug? These should all be Int.
    , documentation'Total :: Maybe Text
    , callers'Total :: Maybe Text
    -- TODO related_nodes_by_relation
    }
    deriving (Eq, Ord, Show, Generic)

data CrossReferenceSet = CrossReferenceSet
    { ticket'CRS :: KytheUri
    -- TODO marked_source
    , definition'CRS :: Maybe [RelatedAnchor]
    , declaration'CRS :: Maybe [RelatedAnchor]
    , reference'CRS :: Maybe [RelatedAnchor]
    , caller'CRS :: Maybe [RelatedAnchor]
    -- TODO related_node
    }
    deriving (Eq, Ord, Show, Generic)

data RelatedAnchor = RelatedAnchor
    { anchor'RA :: Maybe Anchor
    -- TODO marked_source
    , site'RA :: Maybe [Anchor]
    , ticket'RA :: Maybe KytheUri
    }
    deriving (Eq, Ord, Show, Generic)

data Anchor = Anchor
    { ticket'A :: KytheUri
    , kind'A :: Maybe Text
    , parent'A :: Maybe KytheUri
    , span'A :: Maybe Span
    , text'A :: Maybe Text
    , snippet_span'A :: Maybe Span
    , snippet'A :: Maybe Text
    }
    deriving (Eq, Ord, Show, Generic)

--

instance ToJSON DirEntryKind
instance FromJSON DirEntryKind
instance ToJSON CorpusRootsReply
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON CorpusRoots
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON DirectoryRequest
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON DirectoryReply
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON DirEntry
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON DecorationsRequest
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON DecorationsReply
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON Location
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON Reference
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON Span
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON Point
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON DocumentationRequest
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON DocumentationReply
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON Document
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON Printable
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON CrossReferencesRequest
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON CrossReferencesReply
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON Total
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON CrossReferenceSet
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON RelatedAnchor
  where
    toJSON = genericToJSON dropPrimedPostfix
instance ToJSON Anchor
  where
    toJSON = genericToJSON dropPrimedPostfix

instance FromJSON CorpusRootsReply
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON CorpusRoots
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON DirectoryRequest
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON DirectoryReply
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON DirEntry
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON DecorationsRequest
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON DecorationsReply
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON Location
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON Reference
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON Span
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON Point
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON DocumentationRequest
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON DocumentationReply
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON Document
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON Printable
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON CrossReferencesRequest
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON CrossReferencesReply
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON Total
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON CrossReferenceSet
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON RelatedAnchor
  where
    parseJSON = genericParseJSON dropPrimedPostfix
instance FromJSON Anchor
  where
    parseJSON = genericParseJSON dropPrimedPostfix

instance ToJSON Corpus
  where
    toJSON = genericToJSON unwrapNewtype
instance ToJSON Root
  where
    toJSON = genericToJSON unwrapNewtype
instance ToJSON KytheUri
  where
    toJSON = genericToJSON unwrapNewtype
instance ToJSONKey KytheUri
  where
    toJSONKey = toJSONKeyText unKytheUri

instance FromJSON Corpus
  where
    parseJSON = genericParseJSON unwrapNewtype
instance FromJSON Root
  where
    parseJSON = genericParseJSON unwrapNewtype
instance FromJSON KytheUri
  where
    parseJSON = genericParseJSON unwrapNewtype
instance FromJSONKey KytheUri
  where
    fromJSONKey = fromJSONKeyCoerce

