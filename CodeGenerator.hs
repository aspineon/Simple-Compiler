module CodeGenerator where

import Parser
import FPPrac
import Debug.Trace
import Data.Map
import Data.List

import TypesEtc

gen :: Ast -> [Assembly]

gen (AstProg dcl cs) = genCStmnt cs lut ++ [EndProg]
						where
							lut = genDecl dcl

genDecl :: AstDecl -> Map AstIdentifier Int

genDecl (AstVar ids) = genVars ids 0
	where
		genVars [] i = Data.Map.empty
		genVars (x:xs) i = Data.Map.insert x i (genVars xs (i+1))

genCStmnt :: AstCStmnt -> Map AstIdentifier Int -> [Assembly]

genCStmnt (AstCurly sts) l = genStmnts sts l
	where
		genStmnts [] l = []
		genStmnts (x:xs) l = (genStmnt x l) ++ (genStmnts xs l)

genCStmnt (AstStmntSingle st) l = genStmnt st l

genStmnt :: AstStmnt -> Map AstIdentifier Int -> [Assembly]

genStmnt (AstBecomes idt expr) l 	=  genExpr expr l ++ [Store (Addr 1) (declCheck var)] -- store register 1 in memory address 0 (variable a)
										where 
											var = (Data.Map.lookup idt l)
											declCheck Nothing = error "Variable not declared"
											declCheck (Just x) = x

genStmnt (AstIf expr cst0 cst1) l = genExpr expr l ++ [Jump CR 2,Jump UR (c0 + 2)] ++ il0 ++ [Jump UR (c1 + 1)] ++ il1
									where
										il0 = genCStmnt cst0 l
										il1 = genCStmnt cst1 l
										c0 = Data.List.length il1
										c1 = Data.List.length il1
genStmnt (AstWhile expr cst) l	= il0 ++ [Jump CR 2, Jump UR (c1 + 2)] ++ il1 ++ [Jump UR (-(c0+c1+2))]
									where
										il0 = genExpr expr l
										il1 = genCStmnt cst l
										c0 = Data.List.length il0
										c1 = Data.List.length il1

genStmnt (AstWrite expr) l		= genExpr expr l ++ [WrInstr]

genExpr :: AstExpr -> Map AstIdentifier Int -> [Assembly]

genExpr (AstExprSingle op) l		= genOperand op l
genExpr (AstPlus expr0 expr1) l		= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute Add 2 1 1]
genExpr (AstMinus expr0 expr1) l	= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute Sub 2 1 1]
genExpr (AstMult expr0 expr1) l		= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute Mul 2 1 1]
genExpr (AstDivide expr0 expr1) l	= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute Div 2 1 1]
genExpr (AstModulo expr0 expr1) l	= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute Mod 2 1 1]
genExpr (AstOr expr0 expr1) l		= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute Or 2 1 1]
genExpr (AstAnd expr0 expr1) l		= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute And 2 1 1]
genExpr (AstEq expr0 expr1) l		= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute Equal 2 1 1]
genExpr (AstNeq expr0 expr1) l		= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute NEq 2 1 1]
genExpr (AstGreat expr0 expr1) l	= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute Gt 2 1 1]
genExpr (AstLess expr0 expr1) l		= genExpr expr0 l ++ [Push 1] ++ genExpr expr1 l ++ [Pop 2,Compute Lt 2 1 1]
genExpr (AstNeg expr) l				= genExpr expr l ++ [Compute Not 2 1 1]
genExpr (AstUnaryPlus expr) l		= genExpr expr l -- do nothing this is correct
genExpr (AstUnaryMinus expr) l		= genExpr expr l ++ [Compute Neg 2 1 1]

genOperand :: AstOperand -> Map AstIdentifier Int -> [Assembly]

genOperand (AstNum num) l = [Load (Imm $ round num) 1]
genOperand (AstIdOperand idt) l =  [Load (Addr (declCheck var)) 1]
								where 
									var = (Data.Map.lookup idt l)
									declCheck Nothing = error "Variable not declared"
									declCheck (Just x) = x

genOperand (AstExprOperand expr) l = genExpr expr l



