module TypesEtc where

import Data.List
import Debug.Trace
import Prelude

-- ==========================================================================================================
-- Sprockell

data Value      = Addr Int
                | Imm Int			deriving (Eq,Show)

data LdCode     = NoLoad
                | LdImm
                | LdAddr
                | LdAlu
						deriving (Eq,Show)

data StCode     = NoStore
                | StImm
                | StReg
						deriving (Eq,Show)

data SPCode     = None
                | Up
                | Down				deriving (Eq,Show)

data JmpCode    = NoJump			-- No jump
                | UA				-- UnConditional - Absolute
                | UR				-- UnConditional - Relative
                | CA				-- Conditional   - Absolute
                | CR				-- Conditional   - Relative
						deriving (Eq,Show)

data MachCode   = MachCode { ldCode     :: LdCode       -- 0/1: load from dmem to rbank?
			   , stCode     :: StCode       -- storeCode
			   , spCode     :: SPCode
			   , opCode     :: OpCode       -- opCode
			   , immvalueR  :: Int          -- value from Immediate - to regbank
			   , immvalueS  :: Int          -- value from Immediate - to store
			   , fromreg0   :: Int          -- ibid, first parameter of Compute
			   , fromreg1   :: Int          -- ibid, second parameter of Compute
			   , fromaddr   :: Int          -- address in dmem
			   , toreg      :: Int          -- ibid, third parameter of Compute
			   , toaddr     :: Int          -- address in dmem
			   , jmpCode    :: JmpCode      -- 0/1: indicates a jump
			   , jumpN      :: Int          -- which instruction to jump to
			   }
			deriving (Eq,Show)


data OpCode	= NoOp | Id  | Incr | Decr						-- no corresponding functions in prog.language
		| Neg  | Not								-- unary operations
		| Add  | Sub | Mul  | Div | Mod | Equal | NEq | Gt | Lt | And | Or	-- binary operations
		deriving (Eq,Show)


data Assembly	= Compute OpCode Int Int Int	-- Compute opCode r0 r1 r2: go to "alu",
						--      do "opCode" on regs r0, r1, and put result in reg r2
		| Jump JmpCode Int		-- JumpAbs n: set program counter to n
		| Load Value Int		-- Load (Addr a) r : from "memory a" to "regbank r"
						-- Load (Imm  v) r : put "Int v" in "regbank r"
		| Store Value Int		-- Store (Addr r) a: from "regbank r" to "memory a"
						-- Store (Imm  v) r: put "Int v" in "memory r"
		| Push Int			-- push a value on the stack
		| Pop Int			-- pop a value from the stack

		| WrInstr			-- Write content of regA to output

		| EndProg			-- end of program, handled bij exec function
		deriving (Eq,Show)


data  State	= State	  { regbank	:: [Int]        -- register bank
			  , dmem	:: [Int]        -- main memory, data memory
			  , pc		:: Int
			  , sp		:: Int
			  }
		deriving (Eq,Show)

-- ==========================================================================================================
-- Clock for simulation of the Sprockell

data Tick = Tick
        deriving (Eq,Show)

clock = Tick : clock




-- ==============================================
-- output type for demonstration of execution


data DemoOutput	=  DemoOutput	Int				-- program counter
				Assembly			-- corresponding instruction
				[Int]				-- selected registers
				[Int]				-- selected dmem cells
				Int				-- stack pointer
				[Int]				-- the stack


instance Show DemoOutput where

	show (DemoOutput pc instr regs dmems sp stack)

		= (rjustify 3 $ show pc) ++ ": " ++

		  (ljustify 24 $ filter (/= '\"') $ show instr) ++ "  || " ++

		  (concatWith ',' $ map (rjustify 4) $ map show regs) ++ "  || " ++

		  (concatWith ',' $ map (rjustify 4) $ map show dmems) ++  "  ||  " ++

		  (rjustify 3 $ show sp) ++ ": " ++

		  (concatWith ',' $ map show stack)

		where

		  myshow (Just x) = show x
		  myshow Nothing  = "-"



-- ==========================================================================================================
-- elementary functions


concatWith x [] = []
concatWith x [y] = y
concatWith x (y:ys) = y ++ [x] ++ concatWith x ys

ljustify n x = x ++ replicate (n - length x) ' '
rjustify n x = replicate (n - length x) ' ' ++ x

