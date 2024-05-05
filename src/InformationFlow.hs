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

module InformationFlow where

import Common
import Atoms
import Measure
import Type
import Process
import Exceptions

import Control.Exception (throw)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

mergeable :: [ProcessDef] -> Bool
mergeable pdefs = all ok pdefs
  where
    ok :: ProcessDef -> Bool
    ok (_, _, _, p) = maybe False (const True) (auxU [] p)

    pmap :: Map ProcessName ([ChannelName], ProcessM)
    pmap = Map.fromList [ (pname, (map fst args, p)) | (pname, _, args, p) <- pdefs ]

    auxG p@(Call _ _) = p
    auxG (Wait x p) = Wait x (auxG p)
    auxG p@(Close _) = p
    auxG (Fork x y p q) = Fork x y (auxG p) (auxG q)
    auxG (Join x y p) = Join x y (auxG p)
    auxG (Select x tag p) = Select x tag (auxG p)
    auxG (Case x bs) = Case x (mapSnd auxG bs)
    auxG (Flip l bs) = Flip l (mapSnd auxG bs)
    auxG (Merge mx ps) | Set.size pset == 1
                       , isThreadMaybe mx (Set.findMin pset) = Set.findMin pset
                       | otherwise = throw $ ErrorSecurity "information flow error"
      where
        pset = Set.fromList (map auxG ps)
    auxG (Cut x t p q) = Cut x t (auxG p) (auxG q)

    isThreadMaybe :: Maybe ChannelName -> ProcessM -> Bool
    isThreadMaybe Nothing _ = True
    isThreadMaybe (Just x) p = isThread x p

    auxU :: [ProcessName] -> ProcessM -> Maybe ProcessM
    auxU pnames (Call pname xs) | pname `elem` pnames = Nothing
    auxU pnames (Call pname xs) | Just (ys, p) <- Map.lookup pname pmap =
                                    auxU (pname : pnames) p
    auxU pnames (Merge mx ps) | Set.size pset == 1
                              , isThreadMaybe mx (Set.findMin pset) = Just (Set.findMin pset)
                              | otherwise = throw $ ErrorSecurity "information flow error"
      where
        pset = Set.fromList [ q | Just q <- map (auxU pnames) ps ]
    auxU _ p = Just (auxG p)
