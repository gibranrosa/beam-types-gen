module PrettyPrint where

import Database.HDBC
import Types 

import Data.Char (toUpper, toLower)
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Control.Monad ((<=<))
import qualified Data.Set as Set

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
    "" -> []
    s' -> w : split p s''
          where
            (w, s'') = break p s'

toCamelCase :: String -> String
toCamelCase = toCamelCaseOpt True

toCamelCaseOpt :: Bool -> String -> String
toCamelCaseOpt firstCaps = 
  concatMap ((if firstCaps then capitalize else id) . map toLower) . split (=='_')
  where 
    transformFst :: (Char -> Char) -> String -> String
    transformFst _ [] = []
    transformFst f (x:xs) = f x : xs
    
    capitalize :: String -> String
    capitalize = transformFst toUpper 

ppType :: SqlTypeId -> String
ppType SqlCharT = "Text"
ppType SqlVarCharT = "Text"
ppType SqlLongVarCharT = "Text"
ppType SqlIntegerT = "Int"
ppType SqlNumericT = "Scientific"
ppType SqlTimestampT = "LocalTime"
ppType SqlLongVarBinaryT = "ByteString"
ppType other = error $ "Unimplemented ppType: " ++ show other

ppImport :: SqlTypeId -> Maybe String
ppImport SqlNumericT = Just "Data.Scientific (Scientific)"
ppImport SqlTimestampT = Just "Data.Time.LocalTime (LocalTime)"
ppImport SqlLongVarBinaryT = Just "Data.ByteString (ByteString)"
ppImport _ = Nothing

ppColType :: ColumnType -> String
ppColType (SColumnar t _) = "Columnar f " ++ ppType t
ppColType (SForeignKey Constraint {referencedTableName = Just tbName}) = "PrimaryKey " ++ toCamelCase tbName ++ "T f"  -- TODO: *** Converter Nome para Type
ppColType _ = undefined

indent :: Int -> String
indent num = concat $ replicate num "  "

ppRecordField :: Column -> String
ppRecordField col = concat [indent 1, toCamelCaseOpt False $ columnName col, " :: ", ppColType $ columnType col]

ppRecordType :: String -> [Column] -> String
ppRecordType tblCamel cols =   
  unlines [
    "data " ++ tblCamel ++ "T f = " ++ tblCamel ++ "{",
    intercalate ",\n" $ fmap ppRecordField cols,
    "} deriving (Generic)"
  ]

ppModule :: String -> [Column] -> String 
ppModule tbl cols =  
  unlines [
    "{-# LANGUAGE GADTs, DeriveGeneric #-}",
    "module " ++ tblCamel ++ " where",
    "import Database.Beam",
    "import Database.Beam.Firebird",
    "import Data.Text",
    "import Lens.Micro",
    ppImports,
    ppFKModules,
    ppRecordType tblCamel cols,    
    "",
    ppInstanceTable
  ]
  where 
    tblCamel = toCamelCase tbl 
    tblIdCamel = tblCamel ++ "Id "

    maybeFK (SForeignKey constr) = Just constr
    maybeFK _ = Nothing

    ppImports = 
      intercalate "\n" $
        Set.toList $
          Set.fromList $ fmap ("import "++) $ filter (/= "") $ fmap (fromMaybe "" . (ppImport <=< maybeType . columnType)) cols 
      where maybeType (SColumnar t _) = Just t 
            maybeType _ = Nothing

    ppFKModules = 
      let fks = filter isJust $ fmap (maybeFK . columnType) cols
      in case fks of 
           _:_ -> intercalate  "\n" $ fmap (\(Just c) -> "import " ++ toCamelCase (fromJust $ referencedTableName c)) fks
           _ -> ""

    ppInstanceTable = 
      unlines [
        "instance Beamable " ++ tblCamel ++ "T",
        "instance Beamable (PrimaryKey " ++ tblCamel ++ "T)",
        "instance Table " ++ tblCamel ++ "T where",
        indent 1 ++ "data PrimaryKey " ++ tblCamel ++ "T f = " ++ tblIdCamel ++ " " ++ ppPKTypes ++ " deriving Generic",
        "",
        indent 1 ++ ppPKDef
      ]

    ppPKDef = 
      "primaryKey " ++ 
      case filter (isJust . columnPK) cols of
        [pkCol]        -> "= " ++ tblIdCamel ++ " . " ++ ppColumn pkCol
        pkCols@(_:_:_) ->           
          tblCamel ++ " { " ++ 
          intercalate ", " ((\c -> ppColumn c ++ "=" ++ ppColumnPKVal True c) <$> pkCols)
          ++ "} = " ++ tblIdCamel ++ " " ++ unwords (ppColumnPKVal False <$> pkCols)
        []             -> error "Table " ++ tbl ++ " has NO primary keys"

    ppColumnPKVal :: Bool -> Column -> String
    ppColumnPKVal _       col@Column {columnType = SColumnar _ _} = ppColumn col ++ "Val"
    ppColumnPKVal leftVal Column {columnType = SForeignKey constr} = 
      (if leftVal then "(" ++ toCamelCase (fromJust $ referencedTableName constr) ++ "Id " else "")
      ++ toCamelCaseOpt False (fromJust $ referencedColumnName constr) 
      ++ (if leftVal then ")" else "") 

    ppPKTypes = 
      unwords 
        $ ((\ t -> "(" ++ t ++ ")") . ppColType . columnType) 
        <$> filter (isJust . columnPK) cols

    ppColumn = toCamelCaseOpt False . columnName
