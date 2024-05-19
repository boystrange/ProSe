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
  | WhyNot (Type m)
  | OfCourse (Type m)
  deriving (Eq, Ord)

type TypeS = Type (Maybe Int)
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
ftv (WhyNot t) = ftv t
ftv (OfCourse t) = ftv t
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
    aux (WhyNot t)  = WhyNot (aux t)
    aux (OfCourse t) = OfCourse (aux t)
    aux (Var sname) | tname == sname = t
    aux (Rec sname s) | tname /= sname = Rec sname (aux s)
    aux s = s

unfold :: Type m -> Type m
unfold t@(Rec tname s) = substT tname t s
unfold t = t

un :: Type m -> Bool
un t = case unfold t of
         WhyNot _ -> True
         _ -> False

lin :: Type m -> Bool
lin = not . un

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
    aux (WhyNot t) = OfCourse (aux t)
    aux (OfCourse t) = WhyNot (aux t)
    aux (Put m t) = Get m (aux t)
    aux (Get m t) = Put m (aux t)

instance Functor Type where
  fmap f (Var tname) = Var tname
  fmap f (Rec tname t) = Rec tname (fmap f t)
  fmap f One = One
  fmap f Bot = Bot
  fmap f (Par t s) = Par (fmap f t) (fmap f s)
  fmap f (Mul t s) = Mul (fmap f t) (fmap f s)
  fmap f (With l bs) = With l (mapSnd (fmap f) bs)
  fmap f (Plus l bs) = Plus l (mapSnd (fmap f) bs)
  fmap f (WhyNot t) = OfCourse (fmap f t)
  fmap f (OfCourse t) = WhyNot (fmap f t)
  fmap f (Put m t) = Put (f m) (fmap f t)
  fmap f (Get m t) = Get (f m) (fmap f t)

instance MeasureVariables t => MeasureVariables (Type t) where
  mv (Var tname) = Set.empty
  mv (Rec tname t) = mv t
  mv One = Set.empty
  mv Bot = Set.empty
  mv (Par t s) = Set.union (mv t) (mv s)
  mv (Mul t s) = Set.union (mv t) (mv s)
  mv (With l bs) = Set.unions (map (mv . snd) bs)
  mv (Plus l bs) = Set.unions (map (mv . snd) bs)
  mv (WhyNot t) = mv t
  mv (OfCourse t) = mv t
  mv (Put m t) = Set.union (mv m) (mv t)
  mv (Get m t) = Set.union (mv m) (mv t)
  subst = fmap . subst
