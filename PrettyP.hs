module PrettyP where
	import MIPS
	import C
	import InstrDecompiler
	import Types


	ppInstrs::[CInstruction] -> String
	ppInstrs [] = ""
	ppInstrs (instr:xs) = (ppInstr instr)++(ppInstrs xs) 

	ppInstr::CInstruction -> String
	ppInstr (Atrib s e) = show s ++ " = " ++ ppExpr e ++ ";\n"
  	ppInstr (If e inst) = "if (" ++ ppExpr e ++ "){" ++ ppInstrs inst ++ "}" ++ "\n"
  	ppInstr (Else inst) = "else {" ++ ppInstrs inst ++ "}" ++ "\n"
  	ppInstr (While e inst) = "while (" ++ ppExpr e ++ ") {" ++ "\n" ++ ppInstrs inst ++ "}" ++ "\n"
  	ppInstr (Impl e) = ppExpr e
  	ppInstr (Return e) = "return " ++ ppExpr e ++ ";\n"
  	ppInstr (Call op args (ret, tp)) = ppType tp ++ " " ++ ret ++ "=" ++ ppOperand op ++ "(" ++  ppCArgs args ++ ");" ++ "\n"

  	ppExpr::Expr -> String
  	ppExpr (Var v) = v
  	ppExpr (Const c) = c
  	ppExpr (Not e) = "!("++ ppExpr e ++ ")"
  	ppExpr (Add e1 e2) = ppExpr e1 ++ "+" ++ ppExpr e2 
 	ppExpr (Sub e1 e2) = ppExpr e1 ++ "-" ++ ppExpr e2
 	ppExpr (Mult e1 e2) = ppExpr e1 ++ "*" ++ ppExpr e2
 	ppExpr (Div e1 e2) = ppExpr e1 ++ "/" ++ ppExpr e2
 	ppExpr (Eq e1 e2) = ppExpr e1 ++ "==" ++ ppExpr e2
 	ppExpr (Func op args inst) = op ++ "(" ++ ppDArgs args ++ "){" ++ "\n" ++ ppInstrs inst ++ "}" ++ "\n"
 	ppExpr (Array s i ) = s++"["++  show i++"]"

 	removeDollar xs = [ x | x <- xs, not (x `elem` "$") ]

 	ppLabel::Label->String
 	ppLabel l = show l

 	ppCArgs::[Args] -> String
 	ppCArgs [] = ""
 	ppCArgs (arg:[]) = arg
 	ppCArgs (arg:xs) = arg ++ ", " ++ (ppCArgs xs)

 	ppDArgs::[(CVar,CType)] -> String
 	ppDArgs [] = ""
 	ppDArgs ((var,tp):[]) = ppType tp ++ " " ++ var
 	ppDArgs ((var,tp):xs) = (ppType tp ++ " " ++ var ++ ", ")++(ppDArgs xs)

 	ppTypes::[(CVar,CType)] -> String
 	ppTypes [] = ""
 	ppTypes ((var,tp):xs) = (ppType tp ++ " " ++ var ++ ";\n")++(ppTypes xs)

 	ppOperation::Operation -> String
 	ppOperation (Op s) = s

 	ppOperand::Operand-> String
 	ppOperand (Reg p)= p
	ppOperand (Addr i r)= show i ++ "(" ++ r ++ ")"
	ppOperand (Immdt i) = show i
	ppOperand (Lbl l) = l

 	ppType::CType -> String
 	ppType (Primitive p)= ppPrim p
	ppType (General g)= ppGen g
	ppType (Compact c) = ppComp c

	ppPrim::Prim -> String
	ppPrim (Integer) = "int"  
	ppPrim (Float) = "float"

	ppComp::Comp -> String
	ppComp (Pr p) = ppPrim p 
	ppComp (Pointer g) = ppGen g ++ "*"

	ppGen:: Gen -> String
	ppGen (Cp c) = ppComp c
	ppGen (Arr [c]) = ppComp c ++ "[]"
	ppGen (Struct s) = s  