module C where

	import MIPS 
 
 	data CInstruction = Atrib Expr Expr
	 | If Expr [CInstruction] 
	 | Else [CInstruction]
	 | While Expr [CInstruction]
	 | Impl Expr
	 | Return Expr
	 | Call Operand [Args] (Return,CType)
	-- deriving(Show)

	data Expr = Var String
	 | Const String
	 | Not Expr
	 | Add Expr Expr
	 | Sub Expr Expr
	 | Mult Expr Expr
	 | Div Expr Expr
	 | Eq Expr Expr
	 -- Func Label [Args] [CInstruction] 
	 | Func Label [(Args,CType)] [CInstruction]
	 | Array String Int 
	 --deriving(Show)

	data Prim = Integer|Float 
	data Comp = Pr Prim | Pointer Gen 
	data Gen =  Cp Comp | Arr [Comp] |Struct String 

	data CType = Primitive Prim | General Gen | Compact Comp
	type CVar = Register

	type Args = Register                                   
	type Return = Register 

	instance Show CType where
		show (Primitive p)= show p
		show (General g)= show g
		show (Compact c) =  show c

	instance Show Prim where
	 	show (Integer) = "int"  
	 	show (Float) = "float"

	instance Show Comp where
		show (Pr p) = show p 
		show (Pointer g) = show g ++ "*"

	instance Show Gen where
		show (Cp c) = show c
		show (Arr [c]) = show c ++ "[]"
		show (Struct s) = show s  

	instance Show CInstruction where
	 show (Atrib s e) = show s ++ " = " ++ show e ++ ";\n"
  	 show (If e inst) = "if (" ++ show e ++ "){" ++ show inst ++ "}" ++ "\n"
  	 show (Else inst) = "else {" ++ show inst ++ "}" ++ "\n"
  	 show (While e inst) = "while (" ++ show e ++ ") {" ++ "\n" ++ show inst ++ "}" ++ "\n"
  	 show (Impl e) = show e
  	 show (Return e) = "return " ++ show e ++ "\n"
  	 show (Call op args (ret, tp)) = show tp ++ " " ++ show ret ++ "=" ++ show op ++ "(" ++  show args ++ ");" ++ "\n"
  	 

	instance Show Expr where
	 show (Var v) = v
  	 show (Const c) = c
  	 show (Not e) = "!("++ show e ++ ")"
  	 show (Add e1 e2) = show e1 ++ "+" ++ show e2 
 	 show (Sub e1 e2) = show e1 ++ "-" ++ show e2
 	 show (Mult e1 e2) = show e1 ++ "*" ++ show e2
 	 show (Div e1 e2) = show e1 ++ "/" ++ show e2
 	 show (Eq e1 e2) = show e1 ++ "==" ++ show e2
 	 show (Func op args inst) = show op ++ "(" ++ show args ++ "){" ++ "\n" ++ show inst ++ "}" ++ "\n"
 	 show (Array s i ) = show s++"["++ show i++"]"




