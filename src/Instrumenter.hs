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

-- |Automatic instrumentation
module Instrumenter where

import Atoms
import Common
import Type
import Process

instrument :: [ProcessDefS] -> [ProcessDefS]
instrument = map goD
  where
    goD :: ProcessDefS -> ProcessDefS
    goD (pname, xts, p) = (pname, mapSnd goT xts, goP p)

    goP :: ProcessS -> ProcessS
    goP (Wait x p) = Wait x (goP p)
    goP (Fork x y p q) = Fork x y (goP p) (goP q)
    goP (Join x y p) = Join x y (goP p)
    goP (Select x tag p) = Select x tag (PutGas x (goP p))
    goP (Case x bs) = Case x (mapSnd (GetGas x . goP) bs)
    goP (Cut x t p q) = Cut x (goT t) (goP p) (goP q)
    goP (Flip l bs) = Flip l (mapSnd goP bs)
    goP (PutGas x p) = PutGas x (goP p)
    goP (GetGas x p) = GetGas x (goP p)
    goP (Merge mx ps) = Merge mx (map goP ps)
    goP p = p

    goT :: TypeS -> TypeS
    goT (Rec tname t) = Rec tname (goT t)
    goT (Par t s) = Par (goT t) (goT s)
    goT (Mul t s) = Mul (goT t) (goT s)
    goT (With l bs) = With l (mapSnd (Get Nothing . goT) bs)
    goT (Plus l bs) = Plus l (mapSnd (Put Nothing . goT) bs)
    goT (Put m t) = Put m (goT t)
    goT (Get m t) = Get m (goT t)
    goT t = t
