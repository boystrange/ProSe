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

module Strategy where

import Measure

data Strategy
  = Strategy
    { mcall  :: Measure -> Measure
    , mlink  :: Measure -> Measure
    , mclose :: Measure -> Measure
    , mfork  :: Measure -> Measure
    , mtag   :: Measure -> Measure
    , mput   :: Measure -> Measure
    , mflip  :: Measure -> Measure }

-- defaultStrategy :: Strategy
-- defaultStrategy = Strategy { mcall = id, mred = msucc, mput = msucc }

-- freePutStrategy :: Strategy
-- freePutStrategy = Strategy { mcall = id, mred = msucc, mput = id }

-- onlyCallStrategy :: Strategy
-- onlyCallStrategy = Strategy { mcall = msucc, mred = id, mput = id }
