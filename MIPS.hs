module MIPS where 

	data MIPSInstruction = MInst Operation [Operand]
	 | MLabel Label 
	 deriving (Show)

	data Operation = Op String deriving(Show)

	data Operand= Reg Register 
	 | Addr Int Register
	 | Immdt Int
	 | Lbl Label --func

	type Register = String 
	type Label = String

 	charAt1::Register -> Char
 	charAt1 reg = reg!!1

 	isArg::Register -> Bool
 	isArg reg 
 	 | charAt1 reg == 'a' = True
 	 | otherwise =False

 	isFloat::Register -> Bool
 	isFloat reg 
 	 | charAt1 reg == 'f' = True
 	 | otherwise =False

 	isReturn::Register -> Bool
 	isReturn reg 
 	 | charAt1 reg == 'v' = True
 	 | otherwise =False

 	instance Show Operand where
		show (Reg p)= p
		show (Addr i r)= show i ++ "(" ++ show r ++ ")"
		show (Immdt i) = show i
		show (Lbl l) = show l
