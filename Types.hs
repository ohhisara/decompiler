module Types where
	import MIPS
	import C

	type CheckedTypes = [(CVar,CType)]

	recoverType::[MIPSInstruction] -> CheckedTypes -> [(CVar,CType)] 
	recoverType [] _ = []
	
	recoverType ((MInst (Op op) ([Reg r1, Reg r2, Reg r3])):xs) check = 
		let types = (fromList [r1,r2,r3] check)
		in let types1 = types++check 
			in types++(recoverType xs types1)

	recoverType ((MInst (Op op) ([Reg r1, Reg r2, Immdt i])):xs) check = 
		let types = (fromList [r1,r2] check)
		in let types1 = types++check 
			in types++(recoverType xs types1)


	recoverType ((MInst (Op "move") ([Reg r1, Reg r2])):xs) check = 
		let types = [(r1,Compact (Pr (Integer))),(r2,Compact (Pr (Integer)))]
		in let types1=types++check 
			in types++(recoverType xs types1)

	recoverType ((MInst (Op "lw") ([Reg r1, Addr i r2])):xs) check = 
		let types = [(r1,Compact (Pr (Integer))),(r2, Compact (Pointer (Arr [Pr Integer])))]
			in types++(recoverType xs types++check)

	recoverType ((MInst (Op "li") ([Reg r1, Immdt i])):xs) check = 
		let types = [(r1, Primitive Integer)]
			in types++(recoverType xs types++check)

	recoverType ((MInst(Op "sw") ([Reg r1, Addr i r2])):xs) check = 
		let types = [(r1,Compact (Pointer (Arr [Pr Integer]))),(r2, Compact (Pr (Integer)))]
			in types++(recoverType xs types++check)

	recoverType ((MInst (Op "jal")([Lbl l])):xs) check = 
		let funType = getFunctionType xs l check 
			in funType++(recoverType xs funType++check)

	recoverType (minst:xs) check = recoverType xs check		 

	fromList::[Register] -> CheckedTypes -> [(CVar,CType)]
	fromList [] check = []
	fromList (r1:xs) check
		| isVarChecked r1 check = (fromList xs check)
		| otherwise =
			if isFloat r1 
				then (r1,Primitive Float):(fromList xs check)
				else (r1,Primitive Integer):(fromList xs check)

	isVarChecked::CVar -> CheckedTypes ->Bool
	isVarChecked _ [] = False
	isVarChecked var ((var1,tp):xs)
		| var==var1 = True
		| otherwise = isVarChecked var xs

	getVarType:: CheckedTypes -> CVar -> (CVar,CType)
	getVarType [] var = error "aiai"
	getVarType ((var1,tp):xs) var 
		|var == var1 = (var,tp)
		|otherwise = getVarType xs var 

	getFunctionType::[MIPSInstruction] -> Label  -> CheckedTypes -> [(CVar,CType)]
	getFunctionType [] l check = error "aiai"
	getFunctionType ((MLabel l):xs) l1 check
		| l == l1 = inFunction xs check
		| otherwise = getFunctionType xs l1 check 
	getFunctionType (minst:xs) l check = getFunctionType xs l check 

	inFunction::[MIPSInstruction] -> CheckedTypes -> [(CVar,CType)]
	inFunction ((MInst (Op op) ([Reg r1, Reg r2, Reg r3])):xs) check
		| isReturn r1 = fromList [r1] check
		| otherwise = inFunction xs check




	{-isVarChecked::CVar -> CheckedTypes -> Bool
	isVarChecked var ((var1,t):xs) 
	 | var == var1 = True
	 | otherwise = isVarChecked var xs

	isAnyVarChecked::[CVar] -> CheckedTypes -> Bool
	isAnyChecked (var:xs) check = or ((isVarChecked var check):(isAnyChecked xs check))

	getVarType::CVar -> CheckedTypes -> CType
	getVarType var ((var1,t):xs) 
	 | var == var1 = t
	 | otherwise = getVarType var xs

	getCheckedandType::[CVar] ->CheckedTypes -> [(CVar,CType)]
	getCheckedandType [] _ = []
	getCheckedandType _ [] = []
	getCheckedandType var:xs check 
	 | isVarChecked var check = (getVarType var check):(getCheckedandType xs check)
	 | otherwise = getCheckedandType xs check

	addVarType::[(CVar,CType)] -> CheckedTypes -> CheckedTypes
	addVarType vt check = vt++check-}


