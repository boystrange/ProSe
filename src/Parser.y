{
{-# OPTIONS -w #-}
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

-- |This module implements the parser for FairCheck scripts.
module Parser (parseProcess) where

import Lexer
import Atoms
import Type
import Process
import Render

import Data.Either (partitionEithers)
import Control.Exception
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
  TYPE      { Token _ TokenType }
  WAIT      { Token _ TokenWait }
  CLOSE     { Token _ TokenClose }
  CASE      { Token _ TokenCase }
  FLIP      { Token _ TokenFlip }
  NEW       { Token _ TokenNew }
  IN        { Token _ TokenIn }
  DUAL      { Token _ TokenDual }
  CID       { $$@(Token _ (TokenCID _)) }
  LID       { $$@(Token _ (TokenLID _)) }
  INT       { $$@(Token _ (TokenINT _)) }
  FLOAT     { $$@(Token _ (TokenFLOAT _)) }
  '='       { Token _ TokenEQ }
  '.'       { Token _ TokenDot }
  ':'       { Token _ TokenColon }
  ';'       { Token _ TokenSemiColon }
  ','       { Token _ TokenComma }
  '('       { Token _ TokenLParen }
  ')'       { Token _ TokenRParen }
  '{'       { Token _ TokenLBrace }
  '}'       { Token _ TokenRBrace }
  '['       { Token _ TokenLBrack }
  ']'       { Token _ TokenRBrack }
  '&'       { Token _ TokenAmp }
  '|'       { Token _ TokenPar }
  '⊥'       { Token _ TokenBot }
  '*'       { Token _ TokenTimes }
  '+'       { Token _ TokenPlus }
  'ᴸ'        { Token _ TokenLow }
  'ᴴ'        { Token _ TokenHigh }
  '++'      { Token _ TokenPut }
  '--'      { Token _ TokenGet }
  '?'       { Token _ TokenQMark }
  '!'       { Token _ TokenEMark }

%nonassoc '}' ']' IN

%right ','
%left '+'
%left '*' '|'
%nonassoc '++' '--'
%left ';'

%%

-- PROGRAMS

Program
  : TypeDefList ProcessDefList { ($1, $2) }

TypeDefList
  : { [] }
  | TypeDef TypeDefList { $1 : $2 }

TypeDef
  : TYPE TypeName '=' Type { ($2, $4) }

ProcessDefList
  : { [] }
  | ProcessDef ProcessDefList { $1 : $2 }

ProcessDef
  : ProcessName Parameters '=' Process { ($1, $2, $4) }

Parameters
  : { [] }
  | '(' ParameterList ')' { $2 }

ParameterList
  : { [] }
  | ParameterNeList { $1 }

ParameterNeList
  : Parameter { [$1] }
  | ParameterNeList ',' ParameterNeList { $1 ++ $3 }

Parameter
  : ChannelName ':' TypeExpr { ($1, $3) }

-- PROCESSES

Process
  : '(' Process ')' { $2 }
  | ChannelName '=' ChannelName { Link $1 $3 }
  | CLOSE ChannelName { Close $2 }
  | WAIT ChannelName ';' Process { Wait $2 $4 }
  | ChannelName '(' ChannelName ')' Process IN Process { Fork $1 $3 $5 $7 }
  | ChannelName '(' ChannelName ')' ';' Process { Join $1 $3 $6 }
  | ChannelName '.' Label ';' Process { Select $1 $3 $5 }
  | CASE ChannelName Cases { Case $2 $3 }
  | FLIP LevelOpt Choices { Flip $2 $3 }
  | NEW '(' ChannelName ':' TypeExpr ')' Process IN Process { Cut $3 $5 $7 $9 }
  | ChannelName '++' ';' Process { PutGas $1 $4 }
  | ChannelName '--' ';' Process { GetGas $1 $4 }
  | ProcessName Names { Call $1 $2 }
  | '?' ChannelName '(' ChannelName ')' Process { Client $2 $4 $6 }
  | '!' ChannelName '(' ChannelName ')' Process { Server $2 $4 $6 }

Names
  : { [] }
  | '(' ')' { [] }
  | '(' NameNeList ')' { $2 }

NameNeList
  : ChannelName { [$1] }
  | ChannelName ',' NameNeList { $1 : $3 }

Choices
  : '{' ChoiceNeList '}' { $2 }

ChoiceNeList
  : Choice { [$1] }
  | Choice ',' ChoiceNeList { $1 : $3 }

Choice
  : WeightOpt Process { ($1, $2) }

WeightOpt
  :         { 1 }
  | Num ':' { $1 }

Cases
  : '{' CaseList '}' { $2 }

CaseList
  : { [] }
  | CaseNeList { $1 }

CaseNeList
  : Case { [$1] }
  | Case ',' CaseNeList { $1 : $3 }

Case
  : Label ':' Process { ($1, $3) }

-- IDENTIFIERS

ChannelName
  : LID { Identifier (At $ getPos $1) (getId $1) :: ChannelName }

TypeName
  : CID { Identifier (At $ getPos $1) (getId $1) :: TypeName }

ProcessName
  : CID { Identifier (At $ getPos $1) (getId $1) :: ProcessName }

Label
  : LID { Identifier (At $ getPos $1) (getId $1) :: Label }

-- TYPES

TypeExpr
  : Type { Type $1 }
  | DUAL Type { Dual $2 }

Type
  : Num  { if $1 == 1 then One
           else error $ (show $1) ++ " is not a type" }
  | '⊥'  { Bot }
  | TypeName { Var $1 }
  | '(' Type ')' { $2 }
  | Type '*' Type { Mul $1 $3 }
  | Type '|' Type { Par $1 $3 }
  | '&' LevelOpt Branches { With $2 $3 }
  | '+' LevelOpt Branches { Plus $2 $3 }
  | '?' Type { WhyNot $2 }
  | '!' Type { OfCourse $2 }
  | '++' MeasureOpt Type { Put $2 $3 }
  | '--' MeasureOpt Type { Get $2 $3 }

MeasureOpt
  : { Nothing }
  | '[' Int ']' { Just $2 }

Num : Int { fromIntegral $1 }
  | Float { $1 }

Int : INT { getInt $1 }

Float : FLOAT { getFloat $1 }

LevelOpt
  : { L }
  | 'ᴸ' { L }
  | 'ᴴ' { H }

Branches
  : '{' BranchList '}' { $2 }

BranchList
  : { [] }
  | BranchNeList { $1 }

BranchNeList
  : Branch { [$1] }
  | Branch ',' BranchNeList { $1 : $3 }

Branch
  : Label ':' Type { ($1, $3) }

{
-- external :: Type -> Type -> Type
-- external (Type.Label In bs1) (Type.Label In bs2) = Type.Label In (bs1 ++ bs2)
-- external t s = error $ "cannot combine external choice " ++ show t ++ " and " ++ show s

-- internal :: Type -> Type -> Type
-- internal (Type.Label Out bs1) (Type.Label Out bs2) = Type.Label Out (bs1 ++ bs2)
-- internal t s = error $ "cannot combine internal choice " ++ show t ++ " and " ++ show s

getId :: Token -> String
getId (Token _ (TokenLID x)) = x
getId (Token _ (TokenCID x)) = x

getInt :: Token -> Int
getInt (Token _ (TokenINT n)) = n

getFloat :: Token -> Double
getFloat (Token _ (TokenFLOAT n)) = n

getPos :: Token -> (Int, Int)
getPos (Token (AlexPn _ line col) _) = (line, col)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")

parseProcess :: FilePath -> String -> Either String ([TypeDef], [ProcessDefE])
parseProcess = runAlex' parse
}
