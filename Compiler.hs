import FPPrac
import System.IO
import Tokenizer
import Parser
import VisualiseAst
import RoseTree
import CodeGenerator
import Exec
import TypesEtc
import Sprockell

-- Example program
{-
var n, s;
{ n = 10;
  s = 0;
  while n>0
    { s = s+n;
      n = n-1;
    };
  write s;
}
-}

tok fileName = do  
    contents <- readFile fileName
    putStr . show $ tokenize contents  

par fileName = do  
    contents <- readFile fileName
    putStr . show $ parse $ tokenize contents  

compile fileName = do  
    contents <- readFile fileName
    putStr . show $ gen $ parse $ tokenize contents  

visualiseAst fileName = do  
    contents <- readFile fileName
    showTree $ vis $ parse $ tokenize contents  

execute fileName = do  
    contents <- readFile fileName
    demo showaddrs $ gen $ parse $ tokenize contents  

showaddrs  =    ( [0,1]
                , [0,1,2,3]
                ) :: ([Int],[Int]) 

