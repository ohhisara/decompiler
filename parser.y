{
module Grammar where
import MIPS
import Data.Char
import Data.List
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import System.Environment
}

%name mips 
%tokentype { Token }
%error { parseError }

%token
	reg {TokenReg $$}
	int {TokenInt $$}
	label {TokenLbl $$}
	"add" { TokenPlus $$}
	"sub" { TokenMinus $$}
	"mult" { TokenTimes $$}
	"div" { TokenDiv $$}
	"lw" { TokenLoad $$}
	"sw" { TokenStore $$} 
	"move" {TokenMove $$}
	"beq" {TokenBrEq $$}				
	"bneq" {TokenBrNotEq $$}
	'(' {TokenLB}
	')' {TokenRB}
	':' {TokenDots}


%%

MIPSInstruction: Operation operands {MInst $1 $2}
	|label ':' {MLabel $1}
    --| Operation Operand Operand {MInst $1 [$2 $3]}

operands: Operand {[$1]}
	|Operand operands {$1:$2}

Operand: reg {Reg $1}
	|int '(' reg ')' {Addr $1 $3}
	|int {Immdt $1}
	--|label {Lbl $1}

Operation: "add" {Op $1}
      | "sub" {Op $1}
      | "mult" {Op $1}
      | "div" {Op $1}
      | "lw" {Op $1}
      | "sw" {Op $1}
      | "move" {Op $1}
      | "beq" {Op $1}
      | "bneq" {Op $1}

{

data Token
      = TokenLbl String
      | TokenReg String
      | TokenInt Int
      | TokenPlus String
      | TokenMinus String
      | TokenTimes String
      | TokenDiv String
      | TokenLoad String
      | TokenStore String
      | TokenMove String
      | TokenBrEq String
      | TokenBrNotEq String
      | TokenLB
      | TokenRB
      | TokenDots
 deriving (Eq, Show)


parseError :: [Token] -> a
parseError _ = error "Parse error"

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
   | isSpace c = lexer cs
   | c == '$' = lexReg (c:cs)
   | isAlpha c = lexVar (c:cs)
   | isDigit c = lexNum(c:cs)
lexer ('(':cs) = TokenLB : lexer cs
lexer (')':cs) = TokenRB : lexer cs
lexer (':':cs) = TokenDots : lexer cs

lexVar cs =
	case span isAlpha cs of
	("add", rest) -> TokenPlus "add": lexer rest
	("sub", rest) -> TokenMinus "sub": lexer rest
	("mult", rest) -> TokenTimes "mult": lexer rest
	("div", rest) -> TokenDiv "div": lexer rest
	("lw", rest) -> TokenLoad "lw": lexer rest
	("sw", rest) -> TokenStore "sw": lexer rest
	("move", rest) -> TokenMove "move": lexer rest
	("beq", rest) -> TokenBrEq "beq": lexer rest
	("bneq", rest) -> TokenBrNotEq "bneq": lexer rest
	(var,rest) -> TokenLbl var: lexer rest

lexReg (c:c1:c2:cs)
	|c=='$' && c1=='v' && c2=='0' = TokenReg "$v0" : lexer cs
	|c=='$' && c1=='v' && c2=='1' = TokenReg "$v1" : lexer cs
	|c=='$' && c1=='a' && c2=='0' = TokenReg "$a0" : lexer cs
	|c=='$' && c1=='a' && c2=='1' = TokenReg "$a1" : lexer cs
	|c=='$' && c1=='a' && c2=='2' = TokenReg "$a2" : lexer cs
	|c=='$' && c1=='s' && c2=='0' = TokenReg "$s0" : lexer cs
	|c=='$' && c1=='s' && c2=='1' = TokenReg "$s1" : lexer cs
	|c=='$' && c1=='s' && c2=='2' = TokenReg "$s2" : lexer cs
	|c=='$' && c1=='t' && c2=='0' = TokenReg "$t0" : lexer cs
	|c=='$' && c1=='t' && c2=='1' = TokenReg "$t1" : lexer cs
	|c=='$' && c1=='t' && c2=='2' = TokenReg "$t2" : lexer cs
	|otherwise = error "unknow register"

lexNum cs = TokenInt (read num) : lexer rest
	where (num,rest) = span isDigit cs

main = getContents >>= print . mips . lexer
}

{-

getString::String -> String
getString s = s

main :: IO ()
main = do
	f <- getArgs 
	print . mips . lexer =<< readFile f


parseLine::IO() -> [MIPSInstruction]
parseLine =  calc . lexer

main= do x <- readFile "example.txt"


main = do
	contents <-getContents 
	putStr (lexer getContents)
-}
{-main= do
	line <- getLine
	if null line  
	        then return ()  
	        else do  
	            transform line  
	            main -}

{-

parseProg:: [String] -> [MIPSInstruction]
parseProg [] = []
parseProg (s:xs) = parseLine s : parseProg xs 


-}
