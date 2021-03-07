{-# Language NoImplicitPrelude #-}
module TreeTide.PgPool
    ( PgPool
    , PgPoolOptions(..)
    , createPgPool
    --
    , queryPool
    , queryPool_
    , executePool
    , withTransactionPool
    --
    , module X
    )
where

import           Protolude

import           Data.Pool                     as X
import           Database.PostgreSQL.Simple    as PG
import           Data.Time.Clock

type PgPool = Pool PG.Connection

data PgPoolOptions = PgPoolOptions
    { pgpConnString :: ByteString
    }

createPgPool :: PgPoolOptions -> IO PgPool
createPgPool opts = do
    createPool (PG.connectPostgreSQL (pgpConnString opts))
               (PG.close)
               5
               (realToFrac (secondsToDiffTime 60))
               1

queryPool :: (ToRow q, FromRow r) => PgPool -> Query -> q -> IO [r]
queryPool pool q p = withResource pool $ \conn -> query conn q p

queryPool_ :: (FromRow r) => PgPool -> Query -> IO [r]
queryPool_ pool q = withResource pool $ \conn -> query_ conn q

executePool :: ToRow q => PgPool -> Query -> q -> IO Int64
executePool pool q p = withResource pool $ \conn -> execute conn q p

withTransactionPool :: PgPool -> (Connection -> IO a) -> IO a
withTransactionPool pool f =
    withResource pool $ \conn -> withTransaction conn (f conn)
