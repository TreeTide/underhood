module Database.PostgreSQL.Simple.Extended
    ( singleOnly
    , headOnly
    , singleOnlyOrThrow
    , headOnlyOrThrow
    , singleRowOrThrow
    , fromMaybeSingleOnlyOrThrow
  -- * Reexports
    , module PG
    )
where

import           Database.PostgreSQL.Simple    as PG

import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           UnliftIO                       ( MonadUnliftIO
                                                , throwString
                                                )

fromMaybeSingleOnlyOrThrow :: MonadUnliftIO m => Text -> a -> [PG.Only a] -> m a
fromMaybeSingleOnlyOrThrow e fallback r = case r of
    []          -> pure fallback
    [PG.Only a] -> pure a
    _           -> throwString (unpack e)

singleOnlyOrThrow :: MonadUnliftIO m => Text -> [PG.Only a] -> m a
singleOnlyOrThrow e r = maybe (throwString (unpack e)) pure (singleOnly r)

headOnlyOrThrow :: MonadUnliftIO m => Text -> [PG.Only a] -> m a
headOnlyOrThrow e r = maybe (throwString (unpack e)) pure (headOnly r)

singleOnly :: [PG.Only a] -> Maybe a
singleOnly as = case as of
    [PG.Only a] -> Just $! a
    _           -> Nothing

headOnly :: [PG.Only a] -> Maybe a
headOnly as = PG.fromOnly <$> listToMaybe as

singleRowOrThrow :: MonadUnliftIO m => Text -> [a] -> m a
singleRowOrThrow e r = case r of
    [s] -> pure s
    _   -> throwString (unpack e)
