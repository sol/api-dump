{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Format (
  format
#ifdef TEST
, skipParens
, breakOutsideParens
, joinDefinitions
, formatRecord
, formatSum
#endif
) where

import Prelude hiding (String)
import Haskell (String)

use Solid

format :: Solid.String -> Solid.String
format = unlines . map pack . map (formatSum . formatRecord) . joinDefinitions . map unpack . lines

joinDefinitions :: [String] -> [String]
joinDefinitions = go
  where
    go :: [String] -> [String]
    go = \ case
      c : (span startsWithSpace -> (cs, ds)) | isClass c -> joinClassDefinition (c : cs) : go ds
      x : (' ' : y) : ys -> go $ (x ++ ' ' : dropWhile Char.space? y) : ys
      d : ds -> d : go ds
      [] -> []
    isClass = List.startsWith "class"
    startsWithSpace = List.startsWith " "

joinClassDefinition :: [String] -> String
joinClassDefinition = List.intercalate "\n" . go
  where
    go :: [String] -> [String]
    go = \ case
      c : (' ' : ' ' : ' ' : ' ' : ' ' : ' ' : d) : cs -> go $ (c ++ ' ' : dropWhile Char.space? d) : cs
      c : cs -> c : go cs
      [] -> []

formatSum :: String -> String
formatSum input
  | '|' `notElem` input = input
  | otherwise = sum_
  where
    sum_ :: String
    sum_ = case break (== '=') input of
      (xs, '=' : ys) -> List.intercalate "\n" $ strip xs : constructors ys
      _ -> input

    constructors :: String -> [String]
    constructors xs = case break (== '|') xs of
      (ys, '|' : zs) -> ("  | " ++ strip ys) : constructors zs
      _ -> ["  | " ++ strip xs]

formatRecord :: String -> String
formatRecord = record
  where
    record :: String -> String
    record input = case break (== '{') input of
      (_,  '{' : '-' : _) -> input
      (xs, '{' : ys) -> List.intercalate "\n" $ strip xs : fields ys
      _ -> input

    fields :: String -> [String]
    fields input = case breakOutsideParens (== ',') input of
      (xs, _ : ys) -> ("  " ++ dropWhile Char.space? xs) : fields ys
      (xs, "") -> ["  " ++ (dropWhile Char.space? $ take xs.length.pred xs)]

breakOutsideParens :: (Char -> Bool) -> String -> (String, String)
breakOutsideParens p = go
  where
    go :: String -> (String, String)
    go = \ case
      '(' : xs -> case skipParens xs of
        (ys, zs) -> first ('(' :) $ first (ys ++) (go zs)
      x : xs | p x -> ([], x : xs)
      x : xs ->  first (x :) (go xs)
      [] -> ([], [])

skipParens :: String -> (String, String)
skipParens xs = case break isParen xs of
  (ys, ')' : zs) -> (ys ++ [')'], zs)
  (ys, '(' : zs) -> case skipParens zs of
    (yys, bbb) -> case skipParens bbb of
      (yyys, zzs) -> (ys ++ '(' : yys ++ yyys, zzs)
  (ys, zs) -> (ys, zs)
  where
    isParen :: Char -> Bool
    isParen c = c == '(' || c == ')'

strip :: String -> String
strip = reverse . dropWhile Char.space? . reverse . dropWhile Char.space?
