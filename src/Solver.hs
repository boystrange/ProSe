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

{-# LANGUAGE DataKinds #-}
module Solver where

import Debug.Trace

import Numeric.Limp.Rep.Rep     as R
import Numeric.Limp.Rep.IntDouble     as R
import Numeric.Limp.Program.ResultKind (K(KR))
import Numeric.Limp.Program as P
import Numeric.Limp.Canon   as C
import Numeric.Limp.Solve.Simplex.Maps   as SM
import Numeric.Limp.Solve.Simplex.StandardForm   as ST
import Numeric.Limp.Solve.Branch.Simple  as B
import Numeric.Limp.Canon.Pretty

import Control.Applicative

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Atoms
import Measure
import Process
import Type (Type, Type(..))

limp :: MyProgram -> Maybe (MyAssignment, R IntDouble)
limp prog = bb prog'
  where
    prog' = C.program prog

    simpl p = SM.simplex $ ST.standard p

    solver p | st <- ST.standard p
             , Just s' <- SM.simplex st
             , ass <- SM.assignment s' = Just ass
             | otherwise = Nothing

    bb = B.branch solver

type MyAssignment = Assignment () MVar R.IntDouble
type MyProgram = P.Program () MVar R.IntDouble
type MyLinear = P.Linear () MVar R.IntDouble KR
type MyConstraint = P.Constraint () MVar R.IntDouble
type MyBounds = Bounds () MVar R.IntDouble

type Solution = Map MVar Double

solve :: [MVar] -> [Measure.Constraint] -> Maybe Solution
solve μs cs =
  case limp program of
    Nothing -> Nothing
    Just (ass, _) -> Just $ Map.fromList (zip μs (map (assigned ass . fromEnum) μs))
  where
    program :: MyProgram
    program = P.minimise objective constraint bounds

    objective :: MyLinear
    objective = foldr (.+.) (conR 0) [ r i 1 | i <- μs ]

    constraints :: [MyConstraint]
    constraints = map auxC cs
      where
        auxC (CEq m n) = auxM m :== auxM n
        auxC (CLe m n) = auxM m :<= auxM n

        auxM (MCon k) = conR (fromIntegral k)
        auxM (MRef i) = r1 i
        auxM (MAdd m n) = auxM m .+. auxM n
        auxM (MSub m n) = auxM m .-. auxM n
        auxM (MMul w m) = R w *. auxM m

    constraint :: MyConstraint
    constraint = foldl (:&&) CTrue constraints

    bounds :: [MyBounds]
    bounds = map (lowerR 0) μs

    assigned :: MyAssignment -> Int -> Double
    assigned ass i = unwrapR $ rOf ass (toEnum i)

    -- aux :: MyAssignment -> ProcessDefM -> ProcessDefI
    -- aux ass (pname, m, us, _) = (pname, auxM ass m, map (auxB ass) us, Nothing)

    -- auxM :: MyAssignment -> Measure -> Int
    -- auxM ass (MCon k) = k
    -- auxM ass (MRef i) = assigned ass i
    -- auxM ass (MAdd m n) = auxM ass m + auxM ass n

    -- auxB :: MyAssignment -> (ChannelName, TypeM) -> (ChannelName, TypeS)
    -- auxB ass (x, t) = (x, auxT ass t)

    -- auxT :: MyAssignment -> TypeM -> TypeS
    -- auxT ass = fmap (auxM ass)
