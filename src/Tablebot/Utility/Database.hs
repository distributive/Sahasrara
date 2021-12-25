-- |
-- Module      : Tablebot.Utility.Database
-- Description : Wrappers to database functionality to match our main monad.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Wrappers to database functionality to match our main monad.
module Tablebot.Utility.Database
  ( module Tablebot.Utility.Database,
    Sql.fromSqlKey,
    Sql.toSqlKey,
    liftSql,
  )
where

import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import qualified Database.Persist.Sqlite as Sql
import Tablebot.Utility (EnvDatabaseDiscord, liftSql)

insert :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> EnvDatabaseDiscord d (Sql.Key record)
insert r = liftSql $ Sql.insert r

insert_ :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> EnvDatabaseDiscord d ()
insert_ r = liftSql $ Sql.insert_ r

insertMany :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [record] -> EnvDatabaseDiscord d [Sql.Key record]
insertMany r = liftSql $ Sql.insertMany r

insertMany_ :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [record] -> EnvDatabaseDiscord d ()
insertMany_ r = liftSql $ Sql.insertMany_ r

insertEntityMany :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Entity record] -> EnvDatabaseDiscord d ()
insertEntityMany r = liftSql $ Sql.insertEntityMany r

insertEntity :: (Sql.PersistEntity e, Sql.PersistEntityBackend e ~ Sql.SqlBackend) => e -> EnvDatabaseDiscord d (Sql.Entity e)
insertEntity r = liftSql $ Sql.insertEntity r

insertEntityUnique :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> EnvDatabaseDiscord d (Maybe (Sql.Entity record))
insertEntityUnique r = liftSql $ Sql.insertUniqueEntity r

insertUnique :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> EnvDatabaseDiscord d (Maybe (Sql.Key record))
insertUnique r = liftSql $ Sql.insertUnique r

delete :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Key record -> EnvDatabaseDiscord d ()
delete r = liftSql $ Sql.delete r

deleteBy :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Unique record -> EnvDatabaseDiscord d ()
deleteBy r = liftSql $ Sql.deleteBy r

deleteCascade :: Sql.DeleteCascade record Sql.SqlBackend => Sql.Key record -> EnvDatabaseDiscord d ()
deleteCascade r = liftSql $ Sql.deleteCascade r

deleteCascadeWhere :: Sql.DeleteCascade record Sql.SqlBackend => [Sql.Filter record] -> EnvDatabaseDiscord d ()
deleteCascadeWhere r = liftSql $ Sql.deleteCascadeWhere r

deleteWhereCount :: (Sql.PersistEntity val, Sql.PersistEntityBackend val ~ Sql.SqlBackend) => [Sql.Filter val] -> EnvDatabaseDiscord d Int64
deleteWhereCount r = liftSql $ Sql.deleteWhereCount r

update :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Key record -> [Sql.Update record] -> EnvDatabaseDiscord d ()
update r v = liftSql $ Sql.update r v

updateWhere :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> [Sql.Update record] -> EnvDatabaseDiscord d ()
updateWhere r v = liftSql $ Sql.updateWhere r v

updateWhereCount :: (Sql.PersistEntity val, Sql.PersistEntityBackend val ~ Sql.SqlBackend) => [Sql.Filter val] -> [Sql.Update val] -> EnvDatabaseDiscord d Int64
updateWhereCount r v = liftSql $ Sql.updateWhereCount r v

updateGet :: (Sql.PersistEntity a, Sql.PersistEntityBackend a ~ Sql.SqlBackend) => Sql.Key a -> [Sql.Update a] -> EnvDatabaseDiscord d a
updateGet r v = liftSql $ Sql.updateGet r v

upsert :: (Sql.OnlyOneUniqueKey record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> [Sql.Update record] -> EnvDatabaseDiscord d (Sql.Entity record)
upsert r v = liftSql $ Sql.upsert r v

replace :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Key record -> record -> EnvDatabaseDiscord d ()
replace r v = liftSql $ Sql.replace r v

replaceUnique :: (Sql.PersistEntity record, Eq (Sql.Unique record), Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Key record -> record -> EnvDatabaseDiscord d (Maybe (Sql.Unique record))
replaceUnique r v = liftSql $ Sql.replaceUnique r v

count :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> EnvDatabaseDiscord d Int
count r = liftSql $ Sql.count r

exists :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> EnvDatabaseDiscord d Bool
exists r = liftSql $ Sql.exists r

selectFirst :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> [Sql.SelectOpt record] -> EnvDatabaseDiscord d (Maybe (Sql.Entity record))
selectFirst r v = liftSql $ Sql.selectFirst r v

selectKeysList :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> [Sql.SelectOpt record] -> EnvDatabaseDiscord d [Sql.Key record]
selectKeysList r v = liftSql $ Sql.selectKeysList r v

selectList :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> [Sql.SelectOpt record] -> EnvDatabaseDiscord d [Sql.Entity record]
selectList r v = liftSql $ Sql.selectList r v

get :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Key record -> EnvDatabaseDiscord d (Maybe record)
get v = liftSql $ Sql.get v

getBy :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Unique record -> EnvDatabaseDiscord d (Maybe (Sql.Entity record))
getBy v = liftSql $ Sql.getBy v

getByValue :: (Sql.AtLeastOneUniqueKey record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> EnvDatabaseDiscord d (Maybe (Sql.Entity record))
getByValue v = liftSql $ Sql.getByValue v

getEntity :: (Sql.PersistEntity e, Sql.PersistEntityBackend e ~ Sql.SqlBackend) => Sql.Key e -> EnvDatabaseDiscord d (Maybe (Sql.Entity e))
getEntity v = liftSql $ Sql.getEntity v

getFieldName :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.EntityField record typ -> EnvDatabaseDiscord d Text
getFieldName v = liftSql $ Sql.getFieldName v

getJust :: (Sql.PersistEntity a, Sql.PersistEntityBackend a ~ Sql.SqlBackend) => Sql.Key a -> EnvDatabaseDiscord d a
getJust v = liftSql $ Sql.getJust v

getJustEntity :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Key record -> EnvDatabaseDiscord d (Sql.Entity record)
getJustEntity v = liftSql $ Sql.getJustEntity v

getMany :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Key record] -> EnvDatabaseDiscord d (Map (Sql.Key record) record)
getMany v = liftSql $ Sql.getMany v
