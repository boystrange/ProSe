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

module Measure where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (sort)

data MVar = MVar Int
  deriving (Eq, Ord)

instance Enum MVar where
  toEnum = MVar
  fromEnum (MVar n) = n

data Measure
  = MCon Int
  | MRef MVar
  | MAdd Measure Measure
  | MSub Measure Measure
  | MMul Double Measure
  deriving (Eq, Ord)

mzero :: Measure
mzero = MCon 0

mone :: Measure
mone = MCon 1

madd :: Measure -> Measure -> Measure
madd = MAdd

msucc :: Measure -> Measure
msucc = madd mone

data Constraint
  = CEq Measure Measure
  | CLe Measure Measure
  deriving (Eq, Ord)

class Ord a => MeasureVariables a where
  mv :: a -> Set MVar

instance MeasureVariables Measure where
  mv (MCon _) = Set.empty
  mv (MRef x) = Set.singleton x
  mv (MAdd m n) = Set.union (mv m) (mv n)
  mv (MSub m n) = Set.union (mv m) (mv n)
  mv (MMul w m) = mv m

instance MeasureVariables Constraint where
  mv (CEq m n) = Set.union (mv m) (mv n)
  mv (CLe m n) = Set.union (mv m) (mv n)

instance (MeasureVariables a, MeasureVariables b) => MeasureVariables (a, b) where
  mv (x, y) = Set.union (mv x) (mv y)

instance MeasureVariables a => MeasureVariables [a] where
  mv = Set.unions . map mv

instance MeasureVariables a => MeasureVariables (Set a) where
  mv = Set.unions . Set.elems . Set.map mv
