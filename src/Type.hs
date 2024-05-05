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

-- |This module defines the external representation of __session types__
-- (Section 3.1).
module Type where

import Common
import Atoms
import Measure
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)

-- |Session type representation. In addition to the forms described
-- in the paper, we also provide a 'Rec' constructor to represent
-- recursive session types explicitly in a closed form that is
-- easier to convert into regular trees.
data Type m
  = One
  | Bot
  -- |Session type variable.
  | Var TypeName
  -- |Recursive session type.
  | Rec TypeName (Type m)
  -- |Input/output of a channel.
  | Par (Type m) (Type m)
  | Mul (Type m) (Type m)
  | With Level [(Label, Type m)]
  | Plus Level [(Label, Type m)]
  | Put m (Type m)
  | Get m (Type m)
  deriving (Eq, Ord)

type TypeS = Type ()
data TypeE = Type TypeS | Dual TypeS
type TypeM = Type Measure

type TypeDef = (TypeName, TypeS)

-- DEFINITIONS

-- |A type definition is a pair consisting of a type name and a
-- session type.
-- type TypeDef = (TypeName, Type ())

ftv :: Type m -> Set TypeName
ftv (Var tname) = Set.singleton tname
ftv (Rec tname t) = Set.delete tname (ftv t)
ftv (Par t s) = Set.union (ftv t) (ftv s)
ftv (Mul t s) = Set.union (ftv t) (ftv s)
ftv (With _ bs) = Set.unions (map (ftv . snd) bs)
ftv (Plus _ bs) = Set.unions (map (ftv . snd) bs)
ftv (Put _ t) = ftv t
ftv (Get _ t) = ftv t
ftv _ = Set.empty

substT :: TypeName -> Type m -> Type m -> Type m
substT tname t = aux
  where
    aux (Par t1 t2) = Par (aux t1) (aux t2)
    aux (Mul t1 t2) = Mul (aux t1) (aux t2)
    aux (Plus l bs) = Plus l (mapSnd aux bs)
    aux (With l bs) = With l (mapSnd aux bs)
    aux (Put m t)   = Put m (aux t)
    aux (Get m t)   = Get m (aux t)
    aux (Var sname) | tname == sname = t
    aux (Rec sname s) | tname /= sname = Rec sname (aux s)
    aux s = s

unfold :: Type m -> Type m
unfold t@(Rec tname s) = substT tname t s
unfold t = t

dual :: Type m -> Type m
dual = aux
  where
    aux (Var tname) = Var tname
    aux (Rec tname t) = Rec tname (aux t)
    aux One = Bot
    aux Bot = One
    aux (Par t s) = Mul (aux t) (aux s)
    aux (Mul t s) = Par (aux t) (aux s)
    aux (With l bs) = Plus l (mapSnd aux bs)
    aux (Plus l bs) = With l (mapSnd aux bs)
    aux (Put m t) = Get m (aux t)
    aux (Get m t) = Put m (aux t)
