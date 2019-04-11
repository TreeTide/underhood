-- | Generic usage of 'Options' types:
--
--     instance ToJSON Foo
--       where toJSON = genericToJSON theOptions
--
--     instance FromJSON Foo
--       where parseJSON = genericParseJSON theOptions
--
module TreeTide.JsonNaming (dropPrimedPostfix, unwrapNewtype) where

import           Data.Aeson.Types

-- | Removes trailing part after prime. For example
--
--     foo_field'DT -> foo_field
--
dropPrimedPostfix :: Options
dropPrimedPostfix = defaultOptions { fieldLabelModifier = fieldModifier }
  where
    fieldModifier :: String -> String
    fieldModifier = reverse . tail . dropWhile (not . (== '\'')) . reverse

-- | Stores the content unwrapped.
unwrapNewtype :: Options
unwrapNewtype = defaultOptions { unwrapUnaryRecords = True }

