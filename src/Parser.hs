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
--
-- Copyright 2023 Luca Padovani

-- |This module implements the parser for FairCheck scripts.
module Parser (parseProcess) where

import Lexer
import Atoms
import qualified SourceType as Type
import Process
import Render

import Data.Either (partitionEithers)
import Control.Exception
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,239) ([0,4096,0,0,0,256,0,0,0,4096,0,0,0,1,0,0,0,16,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,32768,0,0,0,0,0,0,0,4,0,0,8192,0,0,0,0,0,0,0,34832,1630,0,0,0,0,0,0,8448,0,0,33024,26088,0,0,0,384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,0,4096,24200,6,0,33024,26088,0,0,4096,0,0,0,64,0,0,0,0,0,0,4096,0,0,0,0,0,0,15872,131,0,0,0,16384,0,0,35840,24576,0,0,2048,0,0,0,2,0,0,8192,0,0,0,512,0,0,0,32,0,0,0,128,0,0,13280,8,0,0,33152,26088,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,2,0,0,4096,33,0,0,59521,101,0,4096,24200,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,528,0,0,34832,1630,0,0,256,4,0,8192,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,4128,0,0,0,2,0,0,8192,0,0,0,512,0,0,0,512,0,0,0,32,0,0,0,32768,1,0,33598,0,0,57344,2099,0,0,15872,131,0,0,0,16,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,16,0,0,16384,0,0,0,0,0,0,15872,131,0,0,0,0,0,0,512,0,0,0,256,0,0,0,0,0,0,0,8448,0,0,4096,0,0,0,16384,0,0,0,64,0,0,8192,0,0,0,0,0,0,0,34832,1630,0,32768,59521,101,0,0,64,0,0,16384,0,0,0,256,0,0,0,0,0,0,8192,0,0,0,0,0,0,57344,2099,0,0,15872,163,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,4,0,13280,8,0,0,0,0,0,0,0,0,0,15872,131,0,0,8192,0,0,0,0,0,0,0,4096,0,0,0,4096,2,0,0,0,0,0,33598,0,0,0,0,0,0,0,0,4,0,0,0,0,0,33598,0,0,0,0,64,0,16384,0,4,0,13280,8,0,0,0,1024,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","TypeDefList","TypeDef","ProcessDefList","ProcessDef","Parameters","ParameterList","ParameterNeList","Parameter","Process","Names","NameNeList","Cases","CaseNeList","Case","ChannelName","TypeName","ProcessName","Label","TypeExpr","Type","LevelOpt","Branches","BranchNeList","Branch","TYPE","WAIT","CLOSE","FAIL","CASE","NEW","IN","DUAL","CID","LID","'='","'.'","':'","';'","','","'('","')'","'{'","'}'","'&'","'|'","'0'","'1'","'\8869'","'\8868'","'*'","'+'","'\7480'","'\7476'","'++'","'--'","%eof"]
        bit_start = st Prelude.* 60
        bit_end = (st Prelude.+ 1) Prelude.* 60
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..59]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (29) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyReduce_2

action_1 (29) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (37) = happyShift action_12
action_2 (7) = happyGoto action_9
action_2 (8) = happyGoto action_10
action_2 (21) = happyGoto action_11
action_2 _ = happyReduce_5

action_3 (29) = happyShift action_4
action_3 (5) = happyGoto action_8
action_3 (6) = happyGoto action_3
action_3 _ = happyReduce_2

action_4 (37) = happyShift action_7
action_4 (20) = happyGoto action_6
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (60) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (39) = happyShift action_16
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_39

action_8 _ = happyReduce_3

action_9 _ = happyReduce_1

action_10 (37) = happyShift action_12
action_10 (7) = happyGoto action_15
action_10 (8) = happyGoto action_10
action_10 (21) = happyGoto action_11
action_10 _ = happyReduce_5

action_11 (44) = happyShift action_14
action_11 (9) = happyGoto action_13
action_11 _ = happyReduce_8

action_12 _ = happyReduce_40

action_13 (39) = happyShift action_33
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (38) = happyShift action_32
action_14 (10) = happyGoto action_28
action_14 (11) = happyGoto action_29
action_14 (12) = happyGoto action_30
action_14 (19) = happyGoto action_31
action_14 _ = happyReduce_10

action_15 _ = happyReduce_6

action_16 (37) = happyShift action_7
action_16 (44) = happyShift action_19
action_16 (48) = happyShift action_20
action_16 (50) = happyShift action_21
action_16 (51) = happyShift action_22
action_16 (52) = happyShift action_23
action_16 (53) = happyShift action_24
action_16 (55) = happyShift action_25
action_16 (58) = happyShift action_26
action_16 (59) = happyShift action_27
action_16 (20) = happyGoto action_17
action_16 (24) = happyGoto action_18
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_48

action_18 (49) = happyShift action_53
action_18 (54) = happyShift action_54
action_18 _ = happyReduce_4

action_19 (37) = happyShift action_7
action_19 (44) = happyShift action_19
action_19 (48) = happyShift action_20
action_19 (50) = happyShift action_21
action_19 (51) = happyShift action_22
action_19 (52) = happyShift action_23
action_19 (53) = happyShift action_24
action_19 (55) = happyShift action_25
action_19 (58) = happyShift action_26
action_19 (59) = happyShift action_27
action_19 (20) = happyGoto action_17
action_19 (24) = happyGoto action_52
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (56) = happyShift action_49
action_20 (57) = happyShift action_50
action_20 (25) = happyGoto action_51
action_20 _ = happyReduce_56

action_21 _ = happyReduce_44

action_22 _ = happyReduce_45

action_23 _ = happyReduce_46

action_24 _ = happyReduce_47

action_25 (56) = happyShift action_49
action_25 (57) = happyShift action_50
action_25 (25) = happyGoto action_48
action_25 _ = happyReduce_56

action_26 (37) = happyShift action_7
action_26 (44) = happyShift action_19
action_26 (48) = happyShift action_20
action_26 (50) = happyShift action_21
action_26 (51) = happyShift action_22
action_26 (52) = happyShift action_23
action_26 (53) = happyShift action_24
action_26 (55) = happyShift action_25
action_26 (58) = happyShift action_26
action_26 (59) = happyShift action_27
action_26 (20) = happyGoto action_17
action_26 (24) = happyGoto action_47
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (37) = happyShift action_7
action_27 (44) = happyShift action_19
action_27 (48) = happyShift action_20
action_27 (50) = happyShift action_21
action_27 (51) = happyShift action_22
action_27 (52) = happyShift action_23
action_27 (53) = happyShift action_24
action_27 (55) = happyShift action_25
action_27 (58) = happyShift action_26
action_27 (59) = happyShift action_27
action_27 (20) = happyGoto action_17
action_27 (24) = happyGoto action_46
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (45) = happyShift action_45
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (43) = happyShift action_44
action_29 _ = happyReduce_11

action_30 _ = happyReduce_12

action_31 (41) = happyShift action_43
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_38

action_33 (30) = happyShift action_37
action_33 (31) = happyShift action_38
action_33 (32) = happyShift action_39
action_33 (33) = happyShift action_40
action_33 (34) = happyShift action_41
action_33 (37) = happyShift action_12
action_33 (38) = happyShift action_32
action_33 (44) = happyShift action_42
action_33 (13) = happyGoto action_34
action_33 (19) = happyGoto action_35
action_33 (21) = happyGoto action_36
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (55) = happyShift action_78
action_34 _ = happyReduce_7

action_35 (39) = happyShift action_73
action_35 (40) = happyShift action_74
action_35 (44) = happyShift action_75
action_35 (58) = happyShift action_76
action_35 (59) = happyShift action_77
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (44) = happyShift action_72
action_36 (14) = happyGoto action_71
action_36 _ = happyReduce_29

action_37 (38) = happyShift action_32
action_37 (19) = happyGoto action_70
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (38) = happyShift action_32
action_38 (19) = happyGoto action_69
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (38) = happyShift action_32
action_39 (19) = happyGoto action_68
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (38) = happyShift action_32
action_40 (19) = happyGoto action_67
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (44) = happyShift action_66
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (30) = happyShift action_37
action_42 (31) = happyShift action_38
action_42 (32) = happyShift action_39
action_42 (33) = happyShift action_40
action_42 (34) = happyShift action_41
action_42 (37) = happyShift action_12
action_42 (38) = happyShift action_32
action_42 (44) = happyShift action_42
action_42 (13) = happyGoto action_65
action_42 (19) = happyGoto action_35
action_42 (21) = happyGoto action_36
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (36) = happyShift action_64
action_43 (37) = happyShift action_7
action_43 (44) = happyShift action_19
action_43 (48) = happyShift action_20
action_43 (50) = happyShift action_21
action_43 (51) = happyShift action_22
action_43 (52) = happyShift action_23
action_43 (53) = happyShift action_24
action_43 (55) = happyShift action_25
action_43 (58) = happyShift action_26
action_43 (59) = happyShift action_27
action_43 (20) = happyGoto action_17
action_43 (23) = happyGoto action_62
action_43 (24) = happyGoto action_63
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (38) = happyShift action_32
action_44 (11) = happyGoto action_61
action_44 (12) = happyGoto action_30
action_44 (19) = happyGoto action_31
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_9

action_46 _ = happyReduce_55

action_47 _ = happyReduce_54

action_48 (46) = happyShift action_59
action_48 (26) = happyGoto action_60
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_57

action_50 _ = happyReduce_58

action_51 (46) = happyShift action_59
action_51 (26) = happyGoto action_58
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (45) = happyShift action_57
action_52 (49) = happyShift action_53
action_52 (54) = happyShift action_54
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (37) = happyShift action_7
action_53 (44) = happyShift action_19
action_53 (48) = happyShift action_20
action_53 (50) = happyShift action_21
action_53 (51) = happyShift action_22
action_53 (52) = happyShift action_23
action_53 (53) = happyShift action_24
action_53 (55) = happyShift action_25
action_53 (58) = happyShift action_26
action_53 (59) = happyShift action_27
action_53 (20) = happyGoto action_17
action_53 (24) = happyGoto action_56
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (37) = happyShift action_7
action_54 (44) = happyShift action_19
action_54 (48) = happyShift action_20
action_54 (50) = happyShift action_21
action_54 (51) = happyShift action_22
action_54 (52) = happyShift action_23
action_54 (53) = happyShift action_24
action_54 (55) = happyShift action_25
action_54 (58) = happyShift action_26
action_54 (59) = happyShift action_27
action_54 (20) = happyGoto action_17
action_54 (24) = happyGoto action_55
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_50

action_56 _ = happyReduce_51

action_57 _ = happyReduce_49

action_58 _ = happyReduce_52

action_59 (38) = happyShift action_84
action_59 (22) = happyGoto action_95
action_59 (27) = happyGoto action_96
action_59 (28) = happyGoto action_97
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_53

action_61 (43) = happyShift action_44
action_61 _ = happyReduce_13

action_62 _ = happyReduce_14

action_63 (49) = happyShift action_53
action_63 (54) = happyShift action_54
action_63 _ = happyReduce_42

action_64 (37) = happyShift action_7
action_64 (44) = happyShift action_19
action_64 (48) = happyShift action_20
action_64 (50) = happyShift action_21
action_64 (51) = happyShift action_22
action_64 (52) = happyShift action_23
action_64 (53) = happyShift action_24
action_64 (55) = happyShift action_25
action_64 (58) = happyShift action_26
action_64 (59) = happyShift action_27
action_64 (20) = happyGoto action_17
action_64 (24) = happyGoto action_94
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (45) = happyShift action_93
action_65 (55) = happyShift action_78
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (38) = happyShift action_32
action_66 (19) = happyGoto action_92
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (46) = happyShift action_91
action_67 (16) = happyGoto action_90
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_19

action_69 _ = happyReduce_17

action_70 (42) = happyShift action_89
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_28

action_72 (38) = happyShift action_32
action_72 (45) = happyShift action_88
action_72 (15) = happyGoto action_86
action_72 (19) = happyGoto action_87
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (38) = happyShift action_32
action_73 (19) = happyGoto action_85
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (38) = happyShift action_84
action_74 (22) = happyGoto action_83
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (38) = happyShift action_32
action_75 (19) = happyGoto action_82
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (42) = happyShift action_81
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (42) = happyShift action_80
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (56) = happyShift action_49
action_78 (57) = happyShift action_50
action_78 (25) = happyGoto action_79
action_78 _ = happyReduce_56

action_79 (30) = happyShift action_37
action_79 (31) = happyShift action_38
action_79 (32) = happyShift action_39
action_79 (33) = happyShift action_40
action_79 (34) = happyShift action_41
action_79 (37) = happyShift action_12
action_79 (38) = happyShift action_32
action_79 (44) = happyShift action_42
action_79 (13) = happyGoto action_112
action_79 (19) = happyGoto action_35
action_79 (21) = happyGoto action_36
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (30) = happyShift action_37
action_80 (31) = happyShift action_38
action_80 (32) = happyShift action_39
action_80 (33) = happyShift action_40
action_80 (34) = happyShift action_41
action_80 (37) = happyShift action_12
action_80 (38) = happyShift action_32
action_80 (44) = happyShift action_42
action_80 (13) = happyGoto action_111
action_80 (19) = happyGoto action_35
action_80 (21) = happyGoto action_36
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (30) = happyShift action_37
action_81 (31) = happyShift action_38
action_81 (32) = happyShift action_39
action_81 (33) = happyShift action_40
action_81 (34) = happyShift action_41
action_81 (37) = happyShift action_12
action_81 (38) = happyShift action_32
action_81 (44) = happyShift action_42
action_81 (13) = happyGoto action_110
action_81 (19) = happyGoto action_35
action_81 (21) = happyGoto action_36
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (45) = happyShift action_109
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (42) = happyShift action_108
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_41

action_85 _ = happyReduce_16

action_86 (45) = happyShift action_107
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (43) = happyShift action_106
action_87 _ = happyReduce_32

action_88 _ = happyReduce_30

action_89 (30) = happyShift action_37
action_89 (31) = happyShift action_38
action_89 (32) = happyShift action_39
action_89 (33) = happyShift action_40
action_89 (34) = happyShift action_41
action_89 (37) = happyShift action_12
action_89 (38) = happyShift action_32
action_89 (44) = happyShift action_42
action_89 (13) = happyGoto action_105
action_89 (19) = happyGoto action_35
action_89 (21) = happyGoto action_36
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_23

action_91 (38) = happyShift action_84
action_91 (17) = happyGoto action_102
action_91 (18) = happyGoto action_103
action_91 (22) = happyGoto action_104
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (41) = happyShift action_101
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_15

action_94 (49) = happyShift action_53
action_94 (54) = happyShift action_54
action_94 _ = happyReduce_43

action_95 (41) = happyShift action_100
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (47) = happyShift action_99
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (43) = happyShift action_98
action_97 _ = happyReduce_60

action_98 (38) = happyShift action_84
action_98 (22) = happyGoto action_95
action_98 (27) = happyGoto action_122
action_98 (28) = happyGoto action_97
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_59

action_100 (37) = happyShift action_7
action_100 (44) = happyShift action_19
action_100 (48) = happyShift action_20
action_100 (50) = happyShift action_21
action_100 (51) = happyShift action_22
action_100 (52) = happyShift action_23
action_100 (53) = happyShift action_24
action_100 (55) = happyShift action_25
action_100 (58) = happyShift action_26
action_100 (59) = happyShift action_27
action_100 (20) = happyGoto action_17
action_100 (24) = happyGoto action_121
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (36) = happyShift action_64
action_101 (37) = happyShift action_7
action_101 (44) = happyShift action_19
action_101 (48) = happyShift action_20
action_101 (50) = happyShift action_21
action_101 (51) = happyShift action_22
action_101 (52) = happyShift action_23
action_101 (53) = happyShift action_24
action_101 (55) = happyShift action_25
action_101 (58) = happyShift action_26
action_101 (59) = happyShift action_27
action_101 (20) = happyGoto action_17
action_101 (23) = happyGoto action_120
action_101 (24) = happyGoto action_63
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (47) = happyShift action_119
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (43) = happyShift action_118
action_103 _ = happyReduce_35

action_104 (41) = happyShift action_117
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_18

action_106 (38) = happyShift action_32
action_106 (15) = happyGoto action_116
action_106 (19) = happyGoto action_87
action_106 _ = happyFail (happyExpListPerState 106)

action_107 _ = happyReduce_31

action_108 (30) = happyShift action_37
action_108 (31) = happyShift action_38
action_108 (32) = happyShift action_39
action_108 (33) = happyShift action_40
action_108 (34) = happyShift action_41
action_108 (37) = happyShift action_12
action_108 (38) = happyShift action_32
action_108 (44) = happyShift action_42
action_108 (13) = happyGoto action_115
action_108 (19) = happyGoto action_35
action_108 (21) = happyGoto action_36
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (30) = happyShift action_37
action_109 (31) = happyShift action_38
action_109 (32) = happyShift action_39
action_109 (33) = happyShift action_40
action_109 (34) = happyShift action_41
action_109 (37) = happyShift action_12
action_109 (38) = happyShift action_32
action_109 (42) = happyShift action_114
action_109 (44) = happyShift action_42
action_109 (13) = happyGoto action_113
action_109 (19) = happyGoto action_35
action_109 (21) = happyGoto action_36
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_25

action_111 _ = happyReduce_26

action_112 _ = happyReduce_27

action_113 (35) = happyShift action_127
action_113 (55) = happyShift action_78
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (30) = happyShift action_37
action_114 (31) = happyShift action_38
action_114 (32) = happyShift action_39
action_114 (33) = happyShift action_40
action_114 (34) = happyShift action_41
action_114 (37) = happyShift action_12
action_114 (38) = happyShift action_32
action_114 (44) = happyShift action_42
action_114 (13) = happyGoto action_126
action_114 (19) = happyGoto action_35
action_114 (21) = happyGoto action_36
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_22

action_116 _ = happyReduce_33

action_117 (30) = happyShift action_37
action_117 (31) = happyShift action_38
action_117 (32) = happyShift action_39
action_117 (33) = happyShift action_40
action_117 (34) = happyShift action_41
action_117 (37) = happyShift action_12
action_117 (38) = happyShift action_32
action_117 (44) = happyShift action_42
action_117 (13) = happyGoto action_125
action_117 (19) = happyGoto action_35
action_117 (21) = happyGoto action_36
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (38) = happyShift action_84
action_118 (17) = happyGoto action_124
action_118 (18) = happyGoto action_103
action_118 (22) = happyGoto action_104
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_34

action_120 (45) = happyShift action_123
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (49) = happyShift action_53
action_121 (54) = happyShift action_54
action_121 _ = happyReduce_62

action_122 _ = happyReduce_61

action_123 (30) = happyShift action_37
action_123 (31) = happyShift action_38
action_123 (32) = happyShift action_39
action_123 (33) = happyShift action_40
action_123 (34) = happyShift action_41
action_123 (37) = happyShift action_12
action_123 (38) = happyShift action_32
action_123 (44) = happyShift action_42
action_123 (13) = happyGoto action_129
action_123 (19) = happyGoto action_35
action_123 (21) = happyGoto action_36
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_36

action_125 (55) = happyShift action_78
action_125 _ = happyReduce_37

action_126 _ = happyReduce_21

action_127 (30) = happyShift action_37
action_127 (31) = happyShift action_38
action_127 (32) = happyShift action_39
action_127 (33) = happyShift action_40
action_127 (34) = happyShift action_41
action_127 (37) = happyShift action_12
action_127 (38) = happyShift action_32
action_127 (44) = happyShift action_42
action_127 (13) = happyGoto action_128
action_127 (19) = happyGoto action_35
action_127 (21) = happyGoto action_36
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (55) = happyShift action_78
action_128 _ = happyReduce_20

action_129 (35) = happyShift action_130
action_129 (55) = happyShift action_78
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (30) = happyShift action_37
action_130 (31) = happyShift action_38
action_130 (32) = happyShift action_39
action_130 (33) = happyShift action_40
action_130 (34) = happyShift action_41
action_130 (37) = happyShift action_12
action_130 (38) = happyShift action_32
action_130 (44) = happyShift action_42
action_130 (13) = happyGoto action_131
action_130 (19) = happyGoto action_35
action_130 (21) = happyGoto action_36
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (55) = happyShift action_78
action_131 _ = happyReduce_24

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1, happy_var_2)
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_0  7 happyReduction_5
happyReduction_5  =  HappyAbsSyn7
		 ([]
	)

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 8 happyReduction_7
happyReduction_7 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_1, happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_0  9 happyReduction_8
happyReduction_8  =  HappyAbsSyn9
		 ([]
	)

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  10 happyReduction_10
happyReduction_10  =  HappyAbsSyn10
		 ([]
	)

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn13
		 (Link happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  13 happyReduction_17
happyReduction_17 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Close happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 13 happyReduction_18
happyReduction_18 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Wait happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  13 happyReduction_19
happyReduction_19 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Fail happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 7 13 happyReduction_20
happyReduction_20 ((HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Fork happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 6 13 happyReduction_21
happyReduction_21 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Join happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 5 13 happyReduction_22
happyReduction_22 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Select happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Case happy_var_2 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 9 13 happyReduction_24
happyReduction_24 ((HappyAbsSyn13  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Cut happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 13 happyReduction_25
happyReduction_25 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (PutGas happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 13 happyReduction_26
happyReduction_26 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (GetGas happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 13 happyReduction_27
happyReduction_27 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Choice happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_2  13 happyReduction_28
happyReduction_28 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn13
		 (Call happy_var_1 happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  14 happyReduction_29
happyReduction_29  =  HappyAbsSyn14
		 ([]
	)

happyReduce_30 = happySpecReduce_2  14 happyReduction_30
happyReduction_30 _
	_
	 =  HappyAbsSyn14
		 ([]
	)

happyReduce_31 = happySpecReduce_3  14 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  15 happyReduction_33
happyReduction_33 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  17 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn18
		 ((happy_var_1, happy_var_3)
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  19 happyReduction_38
happyReduction_38 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn19
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: ChannelName
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyTerminal (happy_var_1@(Token _ (TokenCID _))))
	 =  HappyAbsSyn20
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: TypeName
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  21 happyReduction_40
happyReduction_40 (HappyTerminal (happy_var_1@(Token _ (TokenCID _))))
	 =  HappyAbsSyn21
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: ProcessName
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  22 happyReduction_41
happyReduction_41 (HappyTerminal (happy_var_1@(Token _ (TokenLID _))))
	 =  HappyAbsSyn22
		 (Identifier (At $ getPos happy_var_1) (getId happy_var_1) :: Label
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  23 happyReduction_42
happyReduction_42 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (Type.Type happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  23 happyReduction_43
happyReduction_43 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (Type.Dual happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  24 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn24
		 (Type.Zero
	)

happyReduce_45 = happySpecReduce_1  24 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn24
		 (Type.One
	)

happyReduce_46 = happySpecReduce_1  24 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn24
		 (Type.Bot
	)

happyReduce_47 = happySpecReduce_1  24 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn24
		 (Type.Top
	)

happyReduce_48 = happySpecReduce_1  24 happyReduction_48
happyReduction_48 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn24
		 (Type.Var happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  24 happyReduction_49
happyReduction_49 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  24 happyReduction_50
happyReduction_50 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (Type.Mul happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  24 happyReduction_51
happyReduction_51 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (Type.Par happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  24 happyReduction_52
happyReduction_52 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Type.With happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  24 happyReduction_53
happyReduction_53 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Type.Plus happy_var_2 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2  24 happyReduction_54
happyReduction_54 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Type.Put happy_var_2
	)
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  24 happyReduction_55
happyReduction_55 (HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (Type.Get happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_0  25 happyReduction_56
happyReduction_56  =  HappyAbsSyn25
		 (L
	)

happyReduce_57 = happySpecReduce_1  25 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn25
		 (L
	)

happyReduce_58 = happySpecReduce_1  25 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn25
		 (H
	)

happyReduce_59 = happySpecReduce_3  26 happyReduction_59
happyReduction_59 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (happy_var_2
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  27 happyReduction_60
happyReduction_60 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 ([happy_var_1]
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  27 happyReduction_61
happyReduction_61 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 : happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  28 happyReduction_62
happyReduction_62 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn28
		 ((happy_var_1, happy_var_3)
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 60 60 tk (HappyState action) sts stk;
	Token _ TokenType -> cont 29;
	Token _ TokenWait -> cont 30;
	Token _ TokenClose -> cont 31;
	Token _ TokenFail -> cont 32;
	Token _ TokenCase -> cont 33;
	Token _ TokenNew -> cont 34;
	Token _ TokenIn -> cont 35;
	Token _ TokenDual -> cont 36;
	happy_dollar_dollar@(Token _ (TokenCID _)) -> cont 37;
	happy_dollar_dollar@(Token _ (TokenLID _)) -> cont 38;
	Token _ TokenEQ -> cont 39;
	Token _ TokenDot -> cont 40;
	Token _ TokenColon -> cont 41;
	Token _ TokenSemiColon -> cont 42;
	Token _ TokenComma -> cont 43;
	Token _ TokenLParen -> cont 44;
	Token _ TokenRParen -> cont 45;
	Token _ TokenLBrace -> cont 46;
	Token _ TokenRBrace -> cont 47;
	Token _ TokenAmp -> cont 48;
	Token _ TokenPar -> cont 49;
	Token _ TokenZero -> cont 50;
	Token _ TokenOne -> cont 51;
	Token _ TokenBot -> cont 52;
	Token _ TokenTop -> cont 53;
	Token _ TokenTimes -> cont 54;
	Token _ TokenPlus -> cont 55;
	Token _ TokenLow -> cont 56;
	Token _ TokenHigh -> cont 57;
	Token _ TokenPut -> cont 58;
	Token _ TokenGet -> cont 59;
	_ -> happyError' (tk, [])
	})

happyError_ explist 60 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Alex a
happyReturn = (Prelude.return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> happyError tokens) tk
parse = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- external :: Type -> Type -> Type
-- external (Type.Label In bs1) (Type.Label In bs2) = Type.Label In (bs1 ++ bs2)
-- external t s = error $ "cannot combine external choice " ++ show t ++ " and " ++ show s

-- internal :: Type -> Type -> Type
-- internal (Type.Label Out bs1) (Type.Label Out bs2) = Type.Label Out (bs1 ++ bs2)
-- internal t s = error $ "cannot combine internal choice " ++ show t ++ " and " ++ show s

getId :: Token -> String
getId (Token _ (TokenLID x)) = x
getId (Token _ (TokenCID x)) = x

getPos :: Token -> (Int, Int)
getPos (Token (AlexPn _ line col) _) = (line, col)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")

parseProcess :: FilePath -> String -> Either String ([Type.TypeDef], [SourceProcessDef Type.SourceTypeExpr])
parseProcess = runAlex' parse
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
