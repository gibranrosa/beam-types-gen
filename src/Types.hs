module Types where 

import Database.HDBC
import Data.Maybe (fromMaybe)

data Constraint = Constraint {
  constraintName :: String
  , constraintTableName :: String 
  , constraintColumnName :: String 
  , constraintColumnPos :: Int
  , referencedTableName :: Maybe String 
  , referencedColumnName :: Maybe String   
} deriving (Eq, Show)


toConstraints :: [String] -> [[SqlValue]] -> [Constraint]
toConstraints tblList =
  filter (\c ->fromMaybe True $ (`elem` tblList) <$> referencedTableName c)
  . fmap (\[name, tbName, colName, fieldPos, refTbName, refColName] -> 
            Constraint (fromSql name) (fromSql tbName) (fromSql colName) (fromSql fieldPos) (fromSql refTbName) (fromSql refColName))
              

data ColumnType = SColumnar SqlTypeId (Maybe Int) | SForeignKey Constraint deriving (Eq, Show)

data Column = Column {
  columnName :: String 
  , columnType :: ColumnType 
  , columnPK :: Maybe Int
} deriving (Eq, Show)
