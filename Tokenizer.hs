module Tokenizer where

import Data.Char
import FPPrac
import Debug.Trace

data Token 	= 
	TokProgram
-- supporting
	| TokComma
	| TokSemi
	| TokLcurly
	| TokRcurly
	| TokLparen
	| TokRparen
-- assign
	| TokAssign
-- int operands
	| TokPlus 		-- can be unary
	| TokMinus 		-- can be unary
	| TokMult
	| TokDivide
	| TokModulo
-- bin operands
	| TokOr
	| TokAnd
	| TokEq
	| TokNeq
	| TokGreat
	| TokGreatEq
	| TokLess
	| TokLessEq
	| TokNeg
-- keywords
	| TokVar
	| TokWhile
	| TokIf
	| TokElse
	| TokWrite
-- identifier and numbers
	| TokId String
	| TokNum Number
		deriving (Show,Eq)	

tokenize :: String -> [Token]

tokenize xs = r
				where
					r = reverse $ tokenize' xs []

tokenize' :: String -> [Token] -> [Token]

tokenize' (x:xs) ts	| isDigit x = tokenize' r0 (t0:ts)
			  		| isAlphaNum x = tokenize' r1 (t1:ts)
			  		| x `elem` ",;{}()=+-*/%!<>&|" = tokenize' r2 (t2:ts)
			  		| isSpace x = tokenize' xs ts -- ignore spaces
			  		| otherwise = error "failed to tokenize"
			  			where
			  				(t0,r0) = tokenizeNumber (x:xs)
			  				(t1,r1) = tokenizeIdentifier (x:xs)
			  				(t2,r2) = tokenizeHelper (x:xs)

tokenize' [] ts = ts

tokenizeHelper :: String -> (Token,String)

tokenizeHelper ([x]) | x == ',' = (TokComma,"")
					| x == ';' = (TokSemi,"")
					| x == '{' = (TokLcurly,"")
					| x == '}' = (TokRcurly,"")
					| x == '(' = (TokLparen,"")
					| x == ')' = (TokRparen,"")
					| x == '+' = (TokPlus,"")
					| x == '-' = (TokMinus,"")
					| x == '*' = (TokMult,"")
					| x == '/' = (TokDivide,"")
					| x == '%' = (TokModulo,"")
					| otherwise = error "failed to tokenize last helper"

tokenizeHelper (x:y:xs) | x == ',' = (TokComma,(y:xs))
						| x == ';' = (TokSemi,(y:xs))
						| x == '{' = (TokLcurly,(y:xs))
						| x == '}' = (TokRcurly,(y:xs))
						| x == '(' = (TokLparen,(y:xs))
						| x == ')' = (TokRparen,(y:xs))
						| x == '+' = (TokPlus,(y:xs))
						| x == '-' = (TokMinus,(y:xs))
						| x == '*' = (TokMult,(y:xs))
						| x == '/' = (TokDivide,(y:xs))
						| x == '%' = (TokModulo,(y:xs))
						| x == '|' && y == '|' = (TokOr,xs)
						| x == '&' && y == '&' = (TokOr,xs)
						| x == '=' && y == '=' = (TokEq,xs)
						| x == '=' = (TokAssign,(y:xs))
						| x == '<' && y == '=' = (TokLessEq,xs)
						| x == '<' = (TokLess,(y:xs))
						| x == '>' && y == '=' = (TokGreatEq,xs)
						| x == '>' = (TokGreat,(y:xs))
						| x == '!' && y == '=' = (TokNeq,xs)
						| x == '!' = (TokNeg,(y:xs))
						| otherwise = error "failed to tokenize helper"

tokenizeNumber :: String -> (Token,String)

tokenizeNumber xs = ((TokNum (read ts)),r)
					where
						(ts,r) = tokenizeNumber' BfrDot xs

data TokenizeNumberState = BfrDot | DotConsmd | AftrDot
				
tokenizeNumber' :: TokenizeNumberState -> String -> (String,String)

tokenizeNumber' BfrDot ('.':xs) = ('.':z,rest)
									where
										(z,rest) = tokenizeNumber' DotConsmd xs

tokenizeNumber' BfrDot (x:xs) | isDigit x = ((x:y), r0)
							  | otherwise = ("",(x:xs))
								where
									(y,r0) = tokenizeNumber' BfrDot xs

tokenizeNumber' DotConsmd (x:xs)	| isDigit x = ((x:y), r0)
							 		| otherwise = ("",(x:xs))
										where
											(y,r0) = tokenizeNumber' AftrDot xs

tokenizeNumber' AftrDot (x:xs)	| isDigit x = ((x:y), r0)
								| otherwise = ("",(x:xs))
									where
										(y,r0) = tokenizeNumber' AftrDot xs

tokenizeIdentifier :: String -> (Token,String)

tokenizeIdentifier xs | ts == "var" 	= (TokVar,r)
					  | ts == "while" 	= (TokWhile,r)
					  | ts == "if" 		= (TokIf,r)
					  | ts == "else" 	= (TokElse,r)
					  | ts == "write" 	= (TokWrite,r)
					  | otherwise 		= ((TokId ts),r)
							where
								(ts,r) = tokenizeIdentifier' AccptAlpha xs

data TokenizeIdentifierState = AccptAlpha | AccAlphaNum

tokenizeIdentifier' :: TokenizeIdentifierState -> String -> (String,String)

tokenizeIdentifier' AccptAlpha (x:xs) 	| isAlpha x = (x:y, r0)
										| otherwise = ("",(x:xs))
											where
												(y,r0) = tokenizeIdentifier' AccAlphaNum xs

tokenizeIdentifier' AccAlphaNum (x:xs) 	| isAlphaNum x  = (x:y , r0)
										| otherwise = ("",(x:xs))
											where
												(y,r0) = tokenizeIdentifier' AccAlphaNum xs