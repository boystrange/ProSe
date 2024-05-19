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

import Common
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Partition (Partition)
import qualified Data.Partition as Partition
import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (sort)

data MVar = MVar Int
  deriving (Eq, Ord)

type MSubst = Partition MVar

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
  subst :: MSubst -> a -> a

instance MeasureVariables Measure where
  mv (MCon _) = Set.empty
  mv (MRef x) = Set.singleton x
  mv (MAdd m n) = Set.union (mv m) (mv n)
  mv (MSub m n) = Set.union (mv m) (mv n)
  mv (MMul w m) = mv m

  subst σ (MCon n) = MCon n
  subst σ (MRef x) = MRef (Partition.rep σ x)
  subst σ (MAdd m n) = MAdd (subst σ m) (subst σ n)
  subst σ (MSub m n) = MSub (subst σ m) (subst σ n)
  subst σ (MMul w m) = MMul w (subst σ m)

instance MeasureVariables Constraint where
  mv (CEq m n) = Set.union (mv m) (mv n)
  mv (CLe m n) = Set.union (mv m) (mv n)
  subst σ (CEq m n) = CEq (subst σ m) (subst σ n)
  subst σ (CLe m n) = CLe (subst σ m) (subst σ n)

instance (MeasureVariables a, MeasureVariables b) => MeasureVariables (a, b) where
  mv (x, y) = Set.union (mv x) (mv y)
  subst σ (x, y) = (subst σ x, subst σ y)

instance MeasureVariables a => MeasureVariables [a] where
  mv = Set.unions . map mv
  subst = map . subst

instance MeasureVariables a => MeasureVariables (Set a) where
  mv = Set.unions . Set.elems . Set.map mv
  subst = Set.map . subst

type MPartition = Partition MVar

gatherSubstitutions :: [Constraint] -> (MSubst, [Constraint])
gatherSubstitutions = foldl aux (Partition.empty, [])
  where
    aux (σ, cs) (CEq (MRef μ) (MRef ν)) = (Partition.joinElems μ ν σ, cs)
    aux (σ, cs) c = (σ, c : cs)
