module Tablebot.Plugin.Database
  ( module Tablebot.Plugin.Database,
    Sql.fromSqlKey,
    Sql.toSqlKey,
    liftSql,
  )
where

import Data.Int (Int64)
import Data.Map
import Data.Text (Text)
import qualified Database.Persist.Sqlite as Sql
import Tablebot.Plugin (DatabaseDiscord, liftSql)

insert :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> DatabaseDiscord d (Sql.Key record)
insert r = liftSql $ Sql.insert r

insert_ :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> DatabaseDiscord d ()
insert_ r = liftSql $ Sql.insert_ r

insertMany :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [record] -> DatabaseDiscord d [Sql.Key record]
insertMany r = liftSql $ Sql.insertMany r

insertMany_ :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [record] -> DatabaseDiscord d ()
insertMany_ r = liftSql $ Sql.insertMany_ r

insertEntityMany :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Entity record] -> DatabaseDiscord d ()
insertEntityMany r = liftSql $ Sql.insertEntityMany r

insertEntity :: (Sql.PersistEntity e, Sql.PersistEntityBackend e ~ Sql.SqlBackend) => e -> DatabaseDiscord d (Sql.Entity e)
insertEntity r = liftSql $ Sql.insertEntity r

insertEntityUnique :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> DatabaseDiscord d (Maybe (Sql.Entity record))
insertEntityUnique r = liftSql $ Sql.insertUniqueEntity r

insertUnique :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> DatabaseDiscord d (Maybe (Sql.Key record))
insertUnique r = liftSql $ Sql.insertUnique r

delete :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Key record -> DatabaseDiscord d ()
delete r = liftSql $ Sql.delete r

deleteBy :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Unique record -> DatabaseDiscord d ()
deleteBy r = liftSql $ Sql.deleteBy r

deleteCascade :: Sql.DeleteCascade record Sql.SqlBackend => Sql.Key record -> DatabaseDiscord d ()
deleteCascade r = liftSql $ Sql.deleteCascade r

deleteCascadeWhere :: Sql.DeleteCascade record Sql.SqlBackend => [Sql.Filter record] -> DatabaseDiscord d ()
deleteCascadeWhere r = liftSql $ Sql.deleteCascadeWhere r

deleteWhereCount :: (Sql.PersistEntity val, Sql.PersistEntityBackend val ~ Sql.SqlBackend) => [Sql.Filter val] -> DatabaseDiscord d Int64
deleteWhereCount r = liftSql $ Sql.deleteWhereCount r

update :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Key record -> [Sql.Update record] -> DatabaseDiscord d ()
update r v = liftSql $ Sql.update r v

updateWhere :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> [Sql.Update record] -> DatabaseDiscord d ()
updateWhere r v = liftSql $ Sql.updateWhere r v

updateWhereCount :: (Sql.PersistEntity val, Sql.PersistEntityBackend val ~ Sql.SqlBackend) => [Sql.Filter val] -> [Sql.Update val] -> DatabaseDiscord d Int64
updateWhereCount r v = liftSql $ Sql.updateWhereCount r v

updateGet :: (Sql.PersistEntity a, Sql.PersistEntityBackend a ~ Sql.SqlBackend) => Sql.Key a -> [Sql.Update a] -> DatabaseDiscord d a
updateGet r v = liftSql $ Sql.updateGet r v

upsert :: (Sql.OnlyOneUniqueKey record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> [Sql.Update record] -> DatabaseDiscord d (Sql.Entity record)
upsert r v = liftSql $ Sql.upsert r v

count :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> DatabaseDiscord d Int
count r = liftSql $ Sql.count r

selectFirst :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> [Sql.SelectOpt record] -> DatabaseDiscord d (Maybe (Sql.Entity record))
selectFirst r v = liftSql $ Sql.selectFirst r v

selectKeysList :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> [Sql.SelectOpt record] -> DatabaseDiscord d [Sql.Key record]
selectKeysList r v = liftSql $ Sql.selectKeysList r v

selectList :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Filter record] -> [Sql.SelectOpt record] -> DatabaseDiscord d [Sql.Entity record]
selectList r v = liftSql $ Sql.selectList r v

get :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Key record -> DatabaseDiscord d (Maybe record)
get v = liftSql $ Sql.get v

getBy :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Unique record -> DatabaseDiscord d (Maybe (Sql.Entity record))
getBy v = liftSql $ Sql.getBy v

getByValue :: (Sql.AtLeastOneUniqueKey record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => record -> DatabaseDiscord d (Maybe (Sql.Entity record))
getByValue v = liftSql $ Sql.getByValue v

getEntity :: (Sql.PersistEntity e, Sql.PersistEntityBackend e ~ Sql.SqlBackend) => Sql.Key e -> DatabaseDiscord d (Maybe (Sql.Entity e))
getEntity v = liftSql $ Sql.getEntity v

getFieldName :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.EntityField record typ -> DatabaseDiscord d Text
getFieldName v = liftSql $ Sql.getFieldName v

getJust :: (Sql.PersistEntity a, Sql.PersistEntityBackend a ~ Sql.SqlBackend) => Sql.Key a -> DatabaseDiscord d a
getJust v = liftSql $ Sql.getJust v

getJustEntity :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => Sql.Key record -> DatabaseDiscord d (Sql.Entity record)
getJustEntity v = liftSql $ Sql.getJustEntity v

getMany :: (Sql.PersistEntity record, Sql.PersistEntityBackend record ~ Sql.SqlBackend) => [Sql.Key record] -> DatabaseDiscord d (Map (Sql.Key record) record)
getMany v = liftSql $ Sql.getMany v
