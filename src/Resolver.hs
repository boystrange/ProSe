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

-- |Expansion of session types into closed recursive terms.
module Resolver (resolve) where

import Common
import Atoms
import Exceptions
import Measure
import Type
import Process
import Control.Exception (throw)
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State

import qualified Data.Set as Set

-- |Given a list of type definitions and a possibly open type,
-- create a closed type.
resolveT :: [TypeDef] -> TypeS -> TypeS
resolveT tdefs = aux []
  where
    aux :: [TypeName] -> TypeS -> TypeS
    aux tnames One  = One
    aux tnames Bot  = Bot
    aux tnames (Var tname) | tname `elem` tnames = Var tname
    aux tnames (Var tname) =
      case lookup tname tdefs of
        Nothing -> throw (ErrorUnknownIdentifier "type" (showWithPos tname))
        Just t  -> let s = aux (tname : tnames) t in
                     if Set.member tname (ftv s) then
                       Rec tname s
                     else
                       s
    aux tnames (Par t s) = Par (aux tnames t) (aux tnames s)
    aux tnames (Mul t s) = Mul (aux tnames t) (aux tnames s)
    aux tnames (Plus l bs) = Plus l (mapSnd (aux tnames) bs)
    aux tnames (With l bs) = With l (mapSnd (aux tnames) bs)
    aux tnames (WhyNot t) = WhyNot (aux tnames t)
    aux tnames (OfCourse t) = OfCourse (aux tnames t)
    aux tnames (Put m t) = Put m (aux tnames t)
    aux tnames (Get m t) = Get m (aux tnames t)

resolveE :: [TypeDef] -> TypeE -> TypeS
resolveE tdefs (Type t) = resolveT tdefs t
resolveE tdefs (Dual t) = dual (resolveT tdefs t)

-- |Given a list of type definitions and a process, close all types
-- occurring in the process.
resolveP :: [TypeDef] -> ProcessE -> ProcessS
resolveP tdefs = aux
  where
    aux (Link x y)       = Link x y
    aux (Call pname xs)  = Call pname xs
    aux (Wait x p)       = Wait x (aux p)
    aux (Close x)        = Close x
    aux (Fork x y p q)   = Fork x y (aux p) (aux q)
    aux (Join x y p)     = Join x y (aux p)
    aux (Select x tag p) = Select x tag (aux p)
    aux (Case x bs)      = Case x (mapSnd aux bs)
    aux (Client x y p)   = Client x y (aux p)
    aux (Server x y p)   = Server x y (aux p)
    aux (Flip l bs)      = Flip l (mapSnd aux bs)
    aux (Cut x t p q)    = Cut x (resolveE tdefs t) (aux p) (aux q)
    aux (PutGas x p)     = PutGas x (aux p)
    aux (GetGas x p)     = GetGas x (aux p)

-- |Given a list of type definitions and a list of process
-- definitions, close all process definitions.
resolve :: [TypeDef] -> [ProcessDefE] -> [ProcessDefS]
resolve tdefs = map auxD
  where
    auxD :: ProcessDefE -> ProcessDefS
    auxD (pname, xts, p) = (pname, map (uncurry auxT) xts, resolveP tdefs p)

    auxT :: ChannelName -> TypeE -> (ChannelName, TypeS)
    auxT x t = (x, resolveE tdefs t)
