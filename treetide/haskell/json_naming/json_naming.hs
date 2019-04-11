-- | Generic usage of 'Options' types:
--
--     instance ToJSON Foo
--       where toJSON = genericToJSON (optionModifier defaultOptions)
--
--     instance FromJSON Foo
--       where parseJSON = genericParseJSON (optionModifier defaultOptions)
--
module TreeTide.JsonNaming (dropPrimedPostfix, unwrapNewtype) where

import           Data.Aeson.Types

-- | Removes trailing part after prime. For example
--
--     foo_field'DT -> foo_field
--
dropPrimedPostfix :: Options -> Options
dropPrimedPostfix o = o { fieldLabelModifier = fieldModifier }
  where
    fieldModifier :: String -> String
    fieldModifier = reverse . tail . dropWhile (/= '\'') . reverse

-- | Stores the content unwrapped.
unwrapNewtype :: Options -> Options
unwrapNewtype o = o { unwrapUnaryRecords = True }

