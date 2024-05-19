-- This file is part of ProSe
--
-- ProSe is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- ProSe is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with ProSe. If not, see <http://www.gnu.org/licenses/>.
--
-- Copyright 2024 Luca Padovani

-- |Pretty printer for session types and error messages.
module Render
  ( printTitle
  , printWarning
  , printOK
  , printNO
  , printType
  , printSolution
  , printProcess
  , printProcessDec )
where

import Atoms
import Measure
import Type
import Process
import Prelude hiding ((<>))
import Prettyprinter
import qualified Prettyprinter.Render.String as PR
import qualified Prettyprinter.Render.Terminal as PT
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (chr, ord)
import Control.Monad (forM_)

-- PRETTY PRINTER COMPATIBILITY

type Document = Doc PT.AnsiStyle

sub :: Int -> String
sub n = map aux (show n)
  where
    aux '-' = '₋'
    aux c | c >= '0' && c <= '9' = chr (ord c - ord '0' + ord '₀')

sup :: Int -> String
sup n = map aux (show n)
  where
    aux '-' = '⁻'
    aux c | c >= '0' && c <= '9' = chr (ord c - ord '0' + ord '⁰')

keyword :: String -> Document
keyword = annotate (PT.color PT.Blue) . pretty

identifier :: String -> Document
identifier = pretty

constant :: String -> Document
constant = annotate (PT.color PT.Magenta) . pretty

operator :: String -> Document
operator = annotate PT.bold . pretty

emark :: Document
emark = operator "!"

qmark :: Document
qmark = operator "?"

dot :: Document
dot = operator "."

bar :: Document
bar = operator "|"

ampersand :: Document
ampersand = operator "&"

-- UTILITIES

embrace :: Document -> Document -> Document -> [Document] -> Document
embrace open close sep ds = align (encloseSep (open <> space) (space <> close) (sep <> space) ds)

sepembrace :: Document -> Document -> Document -> [Document] -> Document
sepembrace open close sep ds = embrace open close sep (map (<> space) (init ds) ++ [last ds])

-- MEASURES

instance Show MVar where
  show (MVar n) | major == 0 = [letters!!minor]
                | otherwise  = letters!!minor : sub major
    where
      letters = ['α'..]
      max = 20
      major = n `div` max
      minor = n `mod` max

atom :: Measure -> Bool
atom (MCon _) = True
atom (MRef _) = True
atom _ = False

instance Show Measure where
  show (MCon n) = show n
  show (MRef u) = show u
  show (MMul d m) = show d ++ "*" ++ if atom m then show m else "(" ++ show m ++ ")"
  show (MAdd m n) = show m ++ " + " ++ show n
  show (MSub m n) = show m ++ " - " ++ if atom n then show n else "(" ++ show n ++ ")"

instance Show Constraint where
  show (CEq m n) = show m ++ " == " ++ show n
  show (CLe m n) = show m ++ " <= " ++ show n

prettyMeasure :: Measure -> Document
prettyMeasure = annotate (PT.color PT.Green) . pretty . show

-- LEVELS

prettyLevel :: Level -> Document
prettyLevel = annotate (PT.color PT.Red) . pretty . show

-- LABELS

prettyLabel :: Label -> Document
prettyLabel = identifier . show

-- TYPES

prettyType :: (m -> Document) -> Type m -> Document
prettyType prettyMeasure = annotate (PT.colorDull PT.Cyan) . aux
  where
    aux One  = keyword "1"
    aux Bot  = keyword "⊥"
    aux (Var tname) = identifier (show tname)
    aux (Rec tname t) = keyword "rec" <+> identifier (show tname) <> Render.dot <> aux t
    aux (Par t s) = brackets (aux t <+> bar <+> aux s)
    aux (Mul t s) = brackets (aux t <+> operator "*" <+> aux s)
    aux (Plus l bs) = operator "+" <> prettyLevel l <> embrace lbrace rbrace comma (map auxB bs)
    aux (With l bs) = operator "&" <> prettyLevel l <> embrace lbrace rbrace comma (map auxB bs)
    aux (OfCourse t) = operator "?" <> aux t
    aux (WhyNot t) = operator "!" <> aux t
    aux (Put m t) = operator "++" <> brackets (prettyMeasure m) <+> aux t
    aux (Get m t) = operator "--" <> brackets (prettyMeasure m) <+> aux t

    auxB (tag, t) = constant (show tag) <+> colon <+> aux t

instance Show m => Show (Type m) where
  show = PR.renderString . layoutPretty defaultLayoutOptions . prettyType unprettyMeasure
    where
      unprettyMeasure = const (pretty "...")

-- |Print a type.
printType :: TypeM -> IO ()
printType = PT.putDoc . prettyType prettyMeasure

-- PROCESSES

prettyProcess :: ProcessM -> Document
prettyProcess = go
  where
    go (Call pname xs) = identifier (show pname) <> encloseSep lparen rparen comma (map (identifier . show) xs)
    go (Link x y) = identifier (show x) <+> operator "=" <+> identifier (show y)
    go (Close x) = keyword "close" <+> identifier (show x)
    go (Wait x p) = keyword "wait" <+> identifier (show x) <> semi <+> go p
    go (Fork x y p q) = identifier (show x) <> parens (identifier (show y)) <+> go p <+> keyword "in" <+> go q
    go (Join x y p) = identifier (show x) <> parens (identifier (show y)) <> semi <+> go p
    go (Select x tag p) = identifier (show x) <> Render.dot <> identifier (show tag) <> semi <+> go p
    go (Case x bs) = keyword "case" <+> identifier (show x) <+> embrace lbrace rbrace comma (map goCase bs)
    go (Client x y p) = operator "?" <> identifier (show x) <> parens (identifier (show y)) <+> go p
    go (Server x y p) = operator "!" <> identifier (show x) <> parens (identifier (show y)) <+> go p
    go (Cut x t p q) = keyword "new" <+> parens (identifier (show x) <+> colon <+> prettyType prettyMeasure t) <> encloseSep lparen rparen (space <> bar <> space) [go p, go q]
    go (Flip l bs) = keyword "flip" <> prettyLevel l <+> embrace lbrace rbrace comma (map goFlip bs)
    go (PutGas x p) = keyword "put" <+> identifier (show x) <> semi <+> go p
    go (GetGas x p) = keyword "get" <+> identifier (show x) <> semi <+> go p

    goCase (tag, p) = identifier (show tag) <+> colon <+> go p

    goFlip (w, p) = keyword (show w) <+> colon <+> go p

printProcess :: ProcessM -> IO ()
printProcess = PT.putDoc . prettyProcess

-- AUXILIARY PRINTING OPERATIONS

-- |Print a newline.
printNewLine :: IO ()
printNewLine = putStrLn ""

-- |Print a string with style annotations.
printAnnotatedString :: [PT.AnsiStyle] -> String -> IO ()
printAnnotatedString anns msg = PT.putDoc (foldr annotate (pretty msg) anns)

-- |Print a string as a title.
printTitle :: String -> IO ()
printTitle msg = printAnnotatedString [PT.bold, PT.underlined] msg >> printNewLine

-- |Print a warning message.
printWarning :: String -> IO ()
printWarning msg = printAnnotatedString [PT.color PT.Red] msg >> printNewLine

-- |Print an error message.
printNO :: String -> IO ()
printNO msg = do
  printAnnotatedString [PT.color PT.Red] "NO:"
  putStrLn $ " " ++ msg

-- |Print a success message.
printOK :: Maybe String -> IO ()
printOK msg = do
  printAnnotatedString [PT.bold, PT.color PT.Green] "OK"
  case msg of
    Nothing -> printNewLine
    Just m -> putStrLn $ " (" ++ m ++ ")"

printSolution :: Show a => Map.Map MVar a -> IO ()
printSolution = PT.putDoc . fillSep . punctuate comma . map aux . Map.toList
  where
    aux (μ, n) = prettyMeasure (MRef μ) <+> operator "=" <+> pretty (show n)

printProcessDec :: ProcessDef -> IO ()
printProcessDec (pname, μ, xts, p) = do
  PT.putDoc (identifier (show pname) <> brackets (prettyMeasure μ))
  printNewLine
  forM_ xts (\(x, t) -> do
               PT.putDoc (space <+> identifier (show x) <+> colon <+> prettyType prettyMeasure t)
               printNewLine
            )
  PT.putDoc (operator "=" <+> prettyProcess p)
  printNewLine
