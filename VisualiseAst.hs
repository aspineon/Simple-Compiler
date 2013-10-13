module VisualiseAst where

import FPPrac
import Parser
import RoseTree

vis (AstProg dcl cs) = RoseNode ("Prog") [visDecl dcl, visCStmnt cs]

visDecl (AstVar ids) = RoseNode ("Decl") [RoseNode ("Var") (visVars ids)]
	where
		visVars [] = []
		visVars (x:xs) = (visIdentifier x) : (visVars xs)

visCStmnt (AstCurly sts) = RoseNode ("CStmnt") (visStmnts sts)
	where
		visStmnts [] = []
		visStmnts (x:xs) = (visStmnt x) : (visStmnts xs)
visCStmnt (AstStmntSingle st) = RoseNode ("CStmnt") [RoseNode ("StmntSingle") [visStmnt st]]

visStmnt (AstBecomes idt expr) = RoseNode ("Becomes") [visIdentifier idt, visExpr expr]
visStmnt (AstIf expr cst0 cst1) = RoseNode ("If") [visExpr expr, visCStmnt cst0, visCStmnt cst1]
visStmnt (AstWhile expr cst) = RoseNode ("While") [visExpr expr, visCStmnt cst]
visStmnt (AstWrite expr) = RoseNode ("Write") [visExpr expr]

visExpr (AstExprSingle op) = RoseNode ("ExprSingle") [visOperand op]
visExpr (AstPlus expr0 expr1) = RoseNode ("Plus") [visExpr expr0, visExpr expr1]
visExpr (AstMinus expr0 expr1) = RoseNode ("Minus") [visExpr expr0, visExpr expr1]
visExpr (AstMult expr0 expr1) = RoseNode ("Mult") [visExpr expr0, visExpr expr1]
visExpr (AstDivide expr0 expr1) = RoseNode ("Divide") [visExpr expr0, visExpr expr1]
visExpr (AstModulo expr0 expr1) = RoseNode ("Modulo") [visExpr expr0, visExpr expr1]
visExpr (AstOr expr0 expr1) = RoseNode ("Or") [visExpr expr0, visExpr expr1]
visExpr (AstAnd expr0 expr1) = RoseNode ("And") [visExpr expr0, visExpr expr1]
visExpr (AstEq expr0 expr1) = RoseNode ("Eq") [visExpr expr0, visExpr expr1]
visExpr (AstNeq expr0 expr1) = RoseNode ("Neq") [visExpr expr0, visExpr expr1]
visExpr (AstGreat expr0 expr1) = RoseNode ("Great") [visExpr expr0, visExpr expr1]
visExpr (AstLess expr0 expr1) = RoseNode ("Less") [visExpr expr0, visExpr expr1]
visExpr (AstNeg expr) = RoseNode ("Neg") [visExpr expr]
visExpr (AstUnaryPlus expr) = RoseNode ("UnaryPlus") [visExpr expr]
visExpr (AstUnaryMinus expr) = RoseNode ("UnaryMinus") [visExpr expr]

visOperand (AstNum num) = RoseNode ("Num") [visNumber num]
visOperand (AstIdOperand idt) = RoseNode ("IdOperand") [visIdentifier idt]
visOperand (AstExprOperand expr) = RoseNode ("ExprOperand") [visExpr expr]

visIdentifier (AstId str) = RoseNode ("Identifier") [visString str]

visNumber num = RoseNode (show num) []

visString str = RoseNode (str) []