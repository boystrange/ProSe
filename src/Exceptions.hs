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

-- |Representation of FairCheck-specific syntax and typing errors.
module Exceptions where

import Atoms
import Type (Type)
import Measure
import Render ()
import Control.Exception (Exception)
import qualified Data.List as List

-- |The type of FairCheck exceptions.
data MyException
  = ErrorSyntax String
  | ErrorMultipleTypeDefinitions TypeName
  | ErrorMultipleProcessDefinitions ProcessName
  | ErrorMultipleNameDeclarations ChannelName
  | ErrorUnknownIdentifier String String
  | ErrorActionUnbounded ProcessName
  | ErrorCastUnbounded ProcessName ChannelName
  | ErrorSessionUnbounded ProcessName ChannelName
  | ErrorTypeUnbounded ChannelName
  | ErrorTypeNonContractive TypeName
  | ErrorTypeMismatch ChannelName String (Type Measure)
  | ErrorTypeRelation (Type Measure) (Type Measure)
  | ErrorArityMismatch ProcessName Int Int
  | ErrorLabelMismatch ChannelName [Label] [Label]
  | ErrorInvalidType String
  | ErrorLinearity [ChannelName]
  | ErrorRuntime String
  | ErrorGeneric
  | ErrorSecurity String
  | ErrorSecurityLevelMismatch ChannelName Level Level

instance Exception MyException

instance Show MyException where
  show (ErrorSyntax msg) = msg
  show (ErrorMultipleTypeDefinitions tname) = "multiple type definitions: " ++ showWithPos tname
  show (ErrorMultipleProcessDefinitions pname) = "multiple process definitions: " ++ showWithPos pname
  show (ErrorUnknownIdentifier kind name) = "unknown " ++ kind ++ ": " ++ name
  show (ErrorMultipleNameDeclarations u) = "multiple declarations: " ++ showWithPos u
  show (ErrorTypeMismatch name e t) = "type error: " ++ showWithPos name ++ ": expected " ++ e ++ ", actual " ++ show t
  show (ErrorArityMismatch pname expected actual) =
    "arity mismatch for " ++ showWithPos pname ++ ": expected " ++
    show expected ++ ", actual " ++ show actual
  show (ErrorInvalidType msg) = "invalid type: " ++ msg
  show (ErrorActionUnbounded pname) = "action-unbounded process: " ++ showWithPos pname
  show (ErrorSessionUnbounded pname name) = "session-unbounded process: " ++ showWithPos pname ++ " creates " ++ showWithPos name
  show (ErrorCastUnbounded pname name) = "cast-unbounded process: " ++ showWithPos pname ++ " casts " ++ showWithPos name
  show (ErrorLinearity pnames) = "linearity violation: " ++ List.intercalate ", " (map showWithPos pnames)
  show (ErrorLabelMismatch name elabels alabels) = "labels mismatch for " ++ showWithPos name ++ ": expected " ++ show elabels ++ ", actual " ++ show alabels
  show (ErrorTypeUnbounded name) = "unbounded type: " ++ showWithPos name
  show (ErrorTypeNonContractive tname) = "non-contractive type: " ++ showWithPos tname
  show (ErrorSecurityLevelMismatch name l1 l2) = "security level mismatch: " ++ showWithPos name ++ ": expected " ++ show l1 ++ ", actual " ++ show l2
  show (ErrorRuntime msg) = "runtime error: " ++ msg
  show (ErrorSecurity msg) = "security error: " ++ msg
  show (ErrorTypeRelation t s) = "type mismatch: " ++ show t ++ " and " ++ show s
  show ErrorGeneric = "generic error"
