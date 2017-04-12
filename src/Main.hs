{-# LANGUAGE FlexibleContexts, MultiWayIf  #-}

module Main(main) where

import PrettyPrint
import Types

import Data.Char
import Data.Maybe
import Database.HDBC
import Database.HDBC.ODBC
import System.Environment
import System.IO
import Control.Monad (forM_)
                      
getColumns :: [Constraint] -> [(String, SqlColDesc)] -> [Column]
getColumns constrs = 
  fmap $ \(colName, colDesc) -> 
    let colConstrs = filter (\c -> constraintColumnName c == colName) constrs
    in case colConstrs of            
      [] -> createColOrPk colName colDesc Nothing
      constrsLst -> 
        case filter (isJust . referencedTableName) colConstrs of          
          [] -> createColOrPk  colName colDesc $ Just $ constraintColumnPos $ head constrsLst
          [constr] -> createFK (const "") colConstrs constr
          fkLst -> multipleFK colName colConstrs fkLst
      
  where     
    createColOrPk :: String -> SqlColDesc -> Maybe Int -> Column
    createColOrPk colName colDesc = Column colName (SColumnar (colType colDesc) $ colSize colDesc)

    createFK :: (Constraint -> String) -> [Constraint] -> Constraint -> Column
    createFK fname colConstrs constr = 
      let pk = case filter (isNothing . referencedTableName) colConstrs of 
                  [] -> Nothing
                  [constrPk] -> Just $ constraintColumnPos constrPk
                  _ -> error "More than one Primary Key with the same column"
        
      in Column (fromJust (referencedTableName constr) ++ fname constr) (SForeignKey constr) pk

    multipleFK colName colConstrs = createFK constraintColumnName colConstrs . head . filter (\c -> constraintColumnName c == colName)
               
generateModule :: Connection -> [String] -> IO ()
generateModule conn tblList =
  forM_ tblList $ \tbl -> do 
    constraints <- toConstraints tblList <$> getConstraintsRecs tbl
     
    cols <- getColumns constraints <$> describeTable conn tbl 

    withFile (tbl ++ ".hs") WriteMode $ \file -> hPutStrLn file $ ppModule tbl cols

  where 
     getConstraintsRecs tbl = quickQuery conn
       (unlines
         ["SELECT",
          "                  trim(rc.RDB$CONSTRAINT_NAME) AS FK_CONSTRAINT_NAME",
          "                 ,trim(RC.RDB$RELATION_NAME) AS FK_TABLE_NAME                                ",
          "                 ,trim(i.RDB$FIELD_NAME) AS FK_COLUMN_NAME",
          "                 ,I.RDB$FIELD_POSITION",
          "                 ,trim(rcref.RDB$RELATION_NAME) AS REFERENCED_TABLE_NAME",
          "                 ,trim(iref.RDB$FIELD_NAME) AS FK_CONSTRAINT_COLUMN",
          "             FROM RDB$RELATION_CONSTRAINTS AS RC",
          "             inner join RDB$INDEX_SEGMENTS i on i.RDB$INDEX_NAME=rc.RDB$INDEX_NAME",
          "             left join RDB$REF_CONSTRAINTS ref on ref.RDB$CONSTRAINT_NAME=rc.RDB$CONSTRAINT_NAME",
          "             left join RDB$RELATION_CONSTRAINTS AS RCref on rcref.RDB$CONSTRAINT_NAME=ref.RDB$CONST_NAME_UQ",
          "             left join RDB$INDEX_SEGMENTS iref on iref.RDB$INDEX_NAME=rcref.RDB$INDEX_NAME",
          "  --Children", "  WHERE RC.RDB$RELATION_NAME = '" ++ tbl ++ "'"])
       []  
    

main :: IO ()
main = handleSqlError $ do 
  args <- fmap (map $ map toUpper) getArgs
  --let _ =
  case args of 
    connStr:tblList -> do 
      conn <- connectODBC connStr
  
      generateModule conn tblList
      
      print "\n--\n"
      --print constraints
      return ()

    _ -> error "Usage: beam-types-gen <connectionString> table1 [table2...]"


