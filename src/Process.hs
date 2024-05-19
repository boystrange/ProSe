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

-- |Representation of processes (Section 4).
module Process where

import Common
import Atoms
import Measure
import Type
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- |Representation of processes.
data Process t
  -- |Process invocation.
  = Call ProcessName [ChannelName]
  -- |Link.
  | Link ChannelName ChannelName
  -- |Receive session termination signal.
  | Wait ChannelName (Process t)
  -- |Send session termination signal.
  | Close ChannelName
  -- |Fork/output of channel.
  | Fork ChannelName ChannelName (Process t) (Process t)
  -- |Join/input of channel.
  | Join ChannelName ChannelName (Process t)
  -- |Output of label.
  | Select ChannelName Label (Process t)
  -- |Input of label.
  | Case ChannelName [(Label, Process t)]
  -- |Cut.
  | Cut ChannelName t (Process t) (Process t)
  -- |Non-deterministic choice.
  | Flip Level [(Double, Process t)]
  -- |Put gas
  | PutGas ChannelName (Process t)
  -- |Get gas
  | GetGas ChannelName (Process t)
  -- |Internal use.
  | Merge (Maybe ChannelName) [Process t]
  | Client ChannelName ChannelName (Process t)
  | Server ChannelName ChannelName (Process t)
  deriving (Eq, Ord)

-- |Set of channel names occurring free in a process.
fn :: Process t -> Set ChannelName
fn (Call _ xs) = Set.fromList xs
fn (Link x y) = Set.fromList [x, y]
fn (Wait x p) = Set.insert x (fn p)
fn (Close x) = Set.singleton x
fn (Fork x y p q) = Set.insert x (Set.union (Set.delete y (fn p)) (fn q))
fn (Join x y p) = Set.insert x (Set.delete y (fn p))
fn (Select x l p) = Set.insert x (fn p)
fn (Case x gs) = Set.insert x (Set.unions (map (fn . snd) gs))
fn (Cut x _ p q) = Set.delete x (Set.union (fn p) (fn q))
fn (PutGas x p) = Set.insert x (fn p)
fn (GetGas x p) = Set.insert x (fn p)
fn (Flip _ ps) = Set.unions (map (fn . snd) ps)
fn (Client x y p) = Set.insert x (Set.delete y (fn p))
fn (Server x y p) = Set.insert x (Set.delete y (fn p))

-- | A __process definition__ is a triple made of a process name, a
-- list of name declarations and an optional process body. When the
-- body is 'Nothing' the process is declared and assumed to be well
-- typed but is left unspecified.
type ProcessE = Process TypeE
type ProcessS = Process TypeS
type ProcessM = Process TypeM

type ProcessDefE = (ProcessName, [(ChannelName, TypeE)], ProcessE)
type ProcessDefS = (ProcessName, [(ChannelName, TypeS)], ProcessS)
type ProcessDef = (ProcessName, Measure, [(ChannelName, TypeM)], ProcessM)

isThread :: ChannelName -> Process t -> Bool
isThread x (Link y z) = x == y || x == z
isThread x (Close y) = x == y
isThread x (Wait y _) = x == y
isThread x (Fork y _ _ _) = x == y
isThread x (Join y _ _) = x == y
isThread x (Select y _ _) = x == y
isThread x (Case y _) = x == y
isThread x (PutGas y _) = x == y
isThread x (GetGas y _) = x == y
isThread x (Client y _ _) = x == y
isThread x (Server y _ _) = x == y
isThread _ _ = False

instance Functor Process where
  fmap f (Call pname xs) = Call pname xs
  fmap f (Link x y) = Link x y
  fmap f (Wait x p) = Wait x (fmap f p)
  fmap f (Close x) = Close x
  fmap f (Fork x y p q) = Fork x y (fmap f p) (fmap f q)
  fmap f (Join x y p) = Join x y (fmap f p)
  fmap f (Select x l p) = Select x l (fmap f p)
  fmap f (Case x bs) = Case x (mapSnd (fmap f) bs)
  fmap f (Cut x t p q) = Cut x (f t) (fmap f p) (fmap f q)
  fmap f (Flip l bs) = Flip l (mapSnd (fmap f) bs)
  fmap f (PutGas x p) = PutGas x (fmap f p)
  fmap f (GetGas x p) = GetGas x (fmap f p)
  fmap f (Client x y p) = Client x y (fmap f p)
  fmap f (Server x y p) = Server x y (fmap f p)
  fmap f (Merge m ps) = Merge m (map (fmap f) ps)

substProcessDef :: MSubst -> ProcessDef -> ProcessDef
substProcessDef σ (pname, μ, xts, p) = (pname, subst σ μ, mapSnd (subst σ) xts, fmap (subst σ) p)
