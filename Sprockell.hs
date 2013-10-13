{-# LANGUAGE RecordWildCards #-}

{-------------------------------------------------------------
| SPROCKELL: Simple PROCessor in hasKELL :-)
|
| j.kuper@utwente.nl
| October 28, 2012
-------------------------------------------------------------}

module Sprockell where

import Debug.Trace
import TypesEtc
import Prelude



initstate	= State	   { regbank	= replicate regbanksize 0	-- register bank
			   , dmem	= replicate dmemsize 0		-- data memory
			   , pc		= 0				-- program counter
			   , sp		= sp0				-- stack pointer
			   }


nullcode	= MachCode { ldCode	= NoLoad
			   , stCode	= NoStore
			   , opCode	= NoOp
			   , jmpCode	= NoJump
			   , spCode	= None
			   , jumpN	= 0
			   , immvalueR	= 0
			   , immvalueS	= 0
			   , fromreg0	= 0
			   , fromreg1	= 0
			   , fromaddr	= 0
			   , toreg	= 0
			   , toaddr	= 0
			   }

{-------------------------------------------------------------
| some constants
-------------------------------------------------------------}

dmemsize    = 128 :: Int		-- memory sizes as yet unused, no "out of memory" yet
regbanksize =   7 :: Int


-- some special registers
zeroreg	=  0 :: Int
regA    =  1 :: Int
regB    =  2 :: Int
jmpreg	=  5 :: Int		-- for jump instructions
pcreg	=  regbanksize		-- pc is added at the end of the regbank => regbank0

sp0	= 20 :: Int		-- start value of stack pointer


-- xs <~ (i,x) : put value x on position i in list xs
-- used for putting a values on an address in the register bank
xs <~ (0,x) = xs
xs <~ (i,x) = take i xs ++ [x] ++ drop (i+1) xs		-- note the effect for i >= length xs


-- xs <~~ (wen,i,x) : put value x on position i in list xs when wen==True (for "write-enable")
-- used for putting a values on an address in the register bank
xs <~~ (wen,i,x) | wen		= take i xs ++ [x] ++ drop (i+1) xs	-- ibid
		 | otherwise	= xs


(i,x)      ~> xs  =  xs <~  (i,x)
(wen,i,x) ~~> xs  =  xs <~~ (wen,i,x)


{-------------------------------------------------------------
| The actual Sprockell
-------------------------------------------------------------}

-- ============================
decode :: Int -> Assembly -> MachCode

decode sp instr  = case instr of

       		Compute c i0 i1 i2	->  nullcode {ldCode =LdAlu,  opCode=c, fromreg0=i0, fromreg1=i1, toreg=i2}

		Jump jc n		->  nullcode {jmpCode=jc,     fromreg0=regA, fromreg1 =jmpreg, jumpN=n}

   		Load  (Imm  n) j	->  nullcode {ldCode =LdImm,  immvalueR=n,  toreg=j}
   		Load  (Addr i) j	->  nullcode {ldCode =LdAddr, fromaddr =i,  toreg=j}

       		Store (Imm  n) j	->  nullcode {stCode =StImm,  immvalueS=n,  toaddr=j}
       		Store (Addr i) j	->  nullcode {stCode =StReg,  fromreg0 =i,  toaddr=j}

		Push r			->  nullcode {stCode =StReg,  fromreg0 =r,  toaddr=sp+1, spCode=Up}
		Pop r			->  nullcode {ldCode =LdAddr, fromaddr =sp, toreg =r,    spCode=Down}

		WrInstr			->  nullcode {fromreg0 =regA}

		EndProg			->  nullcode


-- ============================

alu :: OpCode -> Int -> Int -> Int

alu opCode x y = case opCode of
			Id	-> x
			Incr	-> x+1
			Decr	-> x-1
			Neg	-> -x
			Add	-> x + y
			Sub	-> x - y
			Mul	-> x * y
			Div	-> x `div` y
			Mod	-> x `mod` y
			Equal	-> tobit (x == y)
			NEq	-> tobit (x /= y)
			Gt	-> tobit (x > y)
			Lt	-> tobit (x < y)
			And	-> x * y
			Or	-> x `max` y
			Not	-> 1 - x
			NoOp	-> 0
		where
		  tobit True  = 1
		  tobit False = 0

-- ============================
load :: [Int] -> LdCode -> Int -> (Int,Int,Int) -> [Int]

load regbank ldCode toreg (immvalueR,mval,z) = regbank'
		where
		  v =  case ldCode of
			NoLoad	-> 0
			LdImm	-> immvalueR
			LdAddr	-> mval
			LdAlu	-> z

		  regbank'	=  regbank <~ (toreg,v)


-- ============================
store :: [Int] -> StCode -> Int -> (Int,Int) -> [Int]

store dmem stCode toaddr (immvalueS,x) = dmem'
		where
		  v =  case stCode of
			NoStore	-> 0
			StImm	-> immvalueS
			StReg	-> x

		  wen		=  stCode /= NoStore
		  dmem'		=  dmem <~~ (wen,toaddr,v)


-- ============================
pcUpd :: (JmpCode, Int) -> (Int,Int) -> Int

pcUpd (jmpCode,x) (pc,jumpN)
		=  case jmpCode of
			NoJump	-> pc+1
			UA	-> jumpN
			UR	-> pc+jumpN
			CA	-> if x==1 then jumpN    else pc+1
			CR	-> if x==1 then pc+jumpN else pc+1



-- ============================
spUpd :: SPCode -> Int -> Int

spUpd spCode sp	= case spCode of
			Up	-> sp+1
			Down	-> sp-1
			None	-> sp


-- ======================================================================================
-- Putting it all together

sprockell :: [Assembly] -> State -> State

sprockell  instrs  State{..}  =	State {dmem=dmem',regbank=regbank',pc=pc',sp=sp'}
		where
		  MachCode{..}	=  decode sp (instrs!!pc)
		  regbank0	=  regbank++[pc]

		  (x,y)		=  (regbank0!!fromreg0 , regbank0!!fromreg1)
		  mval	 	=  dmem!!fromaddr

		  z		=  alu    opCode x y

		  regbank'	=  load   regbank ldCode toreg  	(immvalueR,mval,z)
		  dmem'		=  store  dmem	  stCode toaddr 	(immvalueS,x)

		  pc'     	=  pcUpd  (jmpCode,x)  (pc,jumpN)
		  sp'		=  spUpd  spCode	 sp


