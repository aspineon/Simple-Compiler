module Parser where

import Tokenizer
import FPPrac
import Debug.Trace

data Ast = AstProg AstDecl AstCStmnt
	deriving (Show,Eq)

data AstDecl = AstVar [AstIdentifier]
	deriving (Show,Eq)

data AstCStmnt = AstCurly [AstStmnt]
	| AstStmntSingle AstStmnt
		deriving (Show,Eq)

data AstStmnt = AstBecomes AstIdentifier AstExpr
	| AstIf AstExpr AstCStmnt AstCStmnt
	| AstWhile AstExpr AstCStmnt
	| AstWrite AstExpr
		deriving (Show,Eq)

data AstExpr = AstExprSingle AstOperand
	| AstPlus AstExpr AstExpr
	| AstMinus AstExpr AstExpr
	| AstMult AstExpr AstExpr
	| AstDivide AstExpr AstExpr
	| AstModulo AstExpr AstExpr
	| AstOr AstExpr AstExpr
	| AstAnd AstExpr AstExpr
	| AstEq AstExpr AstExpr
	| AstNeq AstExpr AstExpr
	| AstGreat AstExpr AstExpr
	| AstLess AstExpr AstExpr
	| AstNeg AstExpr
	| AstUnaryPlus AstExpr
	| AstUnaryMinus AstExpr
		deriving (Show,Eq)

data AstOperand = AstNum Number
	| AstIdOperand AstIdentifier
	| AstExprOperand AstExpr
		deriving (Show,Eq)

data AstIdentifier = AstId String
	deriving (Show,Eq,Ord)

parse :: [Token] -> Ast

parse xs	| r1 == [] = AstProg ast0 ast1 
			| otherwise = error "parse error"
				where
					(ast0,r0) = parseDecl xs
					(ast1,r1) = parseCStatement r0

parseDecl :: [Token] -> (AstDecl, [Token])

parseDecl (x:xs) 	| x == TokVar = (AstVar (reverse ss),r0)
					| otherwise = traceShow (x:xs) error "declaration parse error"	
						where
							(ss,r0) = parseDecl' xs []


parseDecl' :: [Token] -> [AstIdentifier] -> ([AstIdentifier],[Token])

parseDecl' (x:y:xs) ss	| isTokId x && y == TokSemi		= ((AstId $ stringFormTokId x):ss,xs)
						| isTokId x && y == TokComma 	= (ss0,r0)
						| otherwise = error "comma declaration parse error"
							where
								(ss0,r0) = parseDecl' xs ((AstId $ stringFormTokId x):ss)

parseCStatement :: [Token] -> (AstCStmnt,[Token])

parseCStatement (x:xs)	| x == TokLcurly	= (AstCurly (reverse ss),r0)
						| otherwise = (AstStmntSingle ast0,r1)
							where
								(ss,r0) = parseCStatement' xs []
								(ast0,r1) = parseStatement xs

parseCStatement' :: [Token] -> [AstStmnt] -> ([AstStmnt],[Token])

parseCStatement' (x:xs) ss	| x == TokRcurly	= (ss,xs)
							| otherwise 		= (ss0,r1) 
								where
									(ast0,r0) 	= parseStatement (x:xs)
									(ss0,r1) 	= parseCStatement' r0 (ast0:ss)

parseStatement :: [Token] -> (AstStmnt,[Token])

parseStatement (x:xs)	| x == TokIf 					= (AstIf ast0 ast1 ast2, r2)
						| x == TokWhile 				= (AstWhile ast0 ast3, r3)
						| x == TokWrite 				= (AstWrite ast5, r5)
						| isTokId x && y == TokAssign 	= (AstBecomes (AstId $ stringFormTokId x) ast4, r4)
						| otherwise = traceShow (x:xs) error "failed to parse statement"
							where
								(ast0,r0) = parseExpr xs
								(ast1,(TokElse:r1)) = traceShow r0 parseCStatement r0
								(ast2,(TokSemi:r2)) = parseCStatement r1
								(ast3,(TokSemi:r3)) = parseCStatement r0 -- for while
								y = head xs -- for the assign token
								ys = tail xs
								(ast4,(TokSemi:r4)) = parseExpr ys
								(ast5,(TokSemi:r5)) = parseExpr xs

isTokId (TokId _) 	= True
isTokId _        	= False

stringFormTokId (TokId x) = x

parseExpr :: [Token] -> (AstExpr,[Token])

parseExpr xs 	| r == TokOr = (AstOr ast0 ast1,r1) 
				| otherwise = parseExpr5 xs
					where
						(ast0,(r:r0)) = parseExpr5 xs
						(ast1,r1) = parseExpr r0

parseExpr5 :: [Token] -> (AstExpr,[Token])

parseExpr5 xs 	| r == TokAnd = (AstAnd ast0 ast1,r1) 
				| otherwise = parseExpr4 xs
					where
						(ast0,(r:r0)) = parseExpr4 xs
						(ast1,r1) = parseExpr5 r0
			  			
parseExpr4 :: [Token] -> (AstExpr,[Token])

parseExpr4 xs 	| r == TokLessEq = (AstOr (AstLess ast0 ast1) (AstEq ast0 ast1),r1) 
				| r == TokLess = (AstLess ast0 ast1,r1) 
				| r == TokEq = (AstEq ast0 ast1,r1) 
				| r == TokNeq = (AstNeq ast0 ast1,r1) 
				| r == TokGreat = (AstGreat ast0 ast1,r1) 
				| r == TokGreatEq = (AstOr (AstGreat ast0 ast1) (AstEq ast0 ast1),r1) 
				| otherwise = parseExpr3 xs
					where
						(ast0,(r:r0)) = parseExpr3 xs
						(ast1,r1) = parseExpr4 r0

parseExpr3 :: [Token] -> (AstExpr,[Token])

parseExpr3 xs 	| r == TokPlus = (AstPlus ast0 ast1,r1) 
				| r == TokMinus = (AstMinus ast0 ast1,r1) 
				| otherwise = parseExpr2 xs
					where
						(ast0,(r:r0)) = parseExpr2 xs
						(ast1,r1) = parseExpr3 r0

parseExpr2 :: [Token] -> (AstExpr,[Token])

parseExpr2 xs 	| r == TokMult = (AstMult ast0 ast1,r1) 
				| r == TokDivide = (AstDivide ast0 ast1,r1) 
				| r == TokModulo = (AstModulo ast0 ast1,r1) 
				| otherwise = parseExpr1 xs
					where
						(ast0,(r:r0)) = parseExpr1 xs
						(ast1,r1) = parseExpr2 r0

parseExpr1 :: [Token] -> (AstExpr,[Token])

parseExpr1 (x:xs) 	| x == TokMinus = (AstUnaryMinus ast0,r0) 
					| x == TokPlus = (AstUnaryPlus ast0,r0) 
					| x == TokNeg = (AstNeg ast0,r0) 
					| otherwise = (AstExprSingle ast1,r1)
						where
							(ast0,r0) = parseExpr1 xs
							(ast1,r1) = parseOperand (x:xs)

parseOperand :: [Token] -> (AstOperand,[Token])

parseOperand (x:xs)	| isTokId x = (AstIdOperand (AstId identifier),xs)
					| isTokNum x = (AstNum num,xs)
					| x == TokLparen = (AstExprOperand e, r)
					| otherwise = traceShow (x:xs) error "failed to parse operand"
						where
							identifier = stringFormTokId x
							num = numberFormTokNum x
							(e,(TokRparen:r)) = parseExpr xs

isTokNum (TokNum _) 	= True
isTokNum _        		= False

numberFormTokNum (TokNum x) = x
