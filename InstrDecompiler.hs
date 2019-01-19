module InstrDecompiler where
import MIPS
import C
import Types
-------------------------------------------------------------------------- c

type CheckedBranchLabels = Label
type CheckedFunctionLabel = Maybe Label

type InFunction = Bool
type CalledFunction = Bool
type FunctionInfo= ([Args],CheckedFunctionLabel,CalledFunction, InFunction) 
type CheckedCInstruction = CInstruction

getArgs::MIPSInstruction -> [Args] -> [Args]
getArgs (MInst (Op op) ([(Reg "$a0"),_,_])) args = (("$a0"):args)
getArgs (MInst (Op op) [(Reg "$a0"),_]) args = (("$a0"):args)
getArgs (MInst (Op op) ([(Reg "$a1"),_,_])) args = (("$a1"):args)
getArgs (MInst (Op op) [(Reg "$a1"),_]) args = (("$a1"):args)
getArgs (MInst (Op op) ([(Reg "$a2"),_,_])) args = (("$a2"):args)
getArgs (MInst (Op op) [(Reg "$a2"),_]) args = (("$a2"):args)
getArgs (MInst (Op op) ([(Reg "$a3"),_,_])) args = (("$a3"):args)
getArgs (MInst (Op op) [(Reg "$a3"),_]) args = (("$a3"):args)
getArgs minst args = args

getLabelInstructions::([MIPSInstruction],Label) -> [CheckedCInstruction] -> [CheckedBranchLabels] -> [FunctionInfo] ->[(CVar, CType)]->[CInstruction]
getLabelInstructions ([],l) cinstr blbls finfo types= []
getLabelInstructions (((MLabel l1):xs), l2) cinstr blbls finfo types=
	if l1==l2
		then if l1 `elem` blbls
			then getCInstruction xs cinstr [] finfo types
			else []
		else []
getLabelInstructions ((inst:xs), l) cinstr blbls finfo types= getLabelInstructions (xs, l) cinstr blbls finfo types

getFunctionArgs:: Label -> [FunctionInfo] -> [(CVar, CType)] ->[(CVar, CType)]
getFunctionArgs _ [] types = []
getFunctionArgs l ((_, Nothing, _, _) : xs) types= getFunctionArgs l xs types
getFunctionArgs l ((args, Just label,_,_):xs) types
 |l == label = map (getVarType types) args
 |otherwise = getFunctionArgs l xs types

getFunctionInstruction:: [MIPSInstruction] -> [CInstruction] -> [CheckedBranchLabels] -> [FunctionInfo] ->[(CVar, CType)]->[CInstruction]
getFunctionInstruction [] _ _ _ _= []
getFunctionInstruction ((MInst (Op op) ([Reg "$v0", Reg r1, Reg r2])):xs) cinstr blbls finfo types= 
	(getCInstruction [(MInst (Op op) ([Reg "$v0", Reg r1, Reg r2]))] cinstr blbls finfo types) ++ (Return (C.Var "$v0")):(getCInstruction xs cinstr blbls finfo types)

getFunctionInstruction ((MInst (Op op) ([Reg "$v1", Reg r1, Reg r2])):xs) cinstr blbls finfo types= 
	(getCInstruction [(MInst (Op op) ([Reg "$v1", Reg r1, Reg r2]))] cinstr blbls finfo types) ++ (Return (C.Var "$v1")):(getCInstruction xs cinstr blbls finfo types)

getFunctionInstruction (minst:xs) cinstr blbls finfo types= (getCInstruction [minst] cinstr blbls finfo types)++(getFunctionInstruction xs cinstr blbls finfo types)

isLabelIn::Label -> [FunctionInfo] -> Bool
isLabelIn _ [] = False
isLabelIn l ((_, Nothing, _, _) : xs) = (isLabelIn l xs)
isLabelIn l ((_,Just label,_,_):xs) 
 | l == label = True
 |otherwise = (isLabelIn l xs)

getCInstruction::[MIPSInstruction] -> [CInstruction] -> [CheckedBranchLabels] -> [FunctionInfo] -> [(CVar,CType)]->[CInstruction]
getCInstruction [] _ _ _ types= []

getCInstruction ((MLabel l):xs) cinstr blbls finfo types= 
	if l `elem` blbls
		then []
		else if (isLabelIn l finfo)
			then [(Impl (Func l (getFunctionArgs l finfo types) (getFunctionInstruction xs cinstr blbls finfo types)))]
			else let lbls1 = (l:blbls)
			in getCInstruction xs [] lbls1 finfo types

----------- por tipo de return
getCInstruction ((MInst (Op "jal") ([Lbl l])):xs) cinstr blbls ((args,Nothing,False,False):xs1) types= 
	(Call (Lbl l) args (getVarType types "$v0")):(getCInstruction xs cinstr blbls (([],Nothing,False,False):(args,Just l,True,False):xs1) types)

getCInstruction ((MInst (Op "jr") ([Reg "$ra"])):xs) cinstr blbls finfo types= []

--getCInstruction ((MInst (Op op) ([Reg "$v0", Reg r2, Reg r3])):xs) cinstr blbls finfo = 

getCInstruction ((MInst (Op "addi") ([Reg r1, Reg r2, Immdt i])):xs) cinstr blbls ((args,label,called,inf):xs1) types=
	(Atrib (Var r1) (Add (Const r2)  (Const (show i)))):(getCInstruction xs cinstr blbls ((args,label,called,inf):xs1) types)


getCInstruction ((MInst (Op op) ([Reg r1, Reg r2, Reg r3])):xs) cinstr blbls ((args,label,called,inf):xs1) types=
	let args1 = getArgs (MInst (Op op) ([Reg r1, Reg r2, Reg r3])) args
	in if blbls == []
		then if op== "add" 
			then (Atrib (Var r1) (Add (Var r2)  (Var r3))):(getCInstruction xs cinstr blbls ((args1,label,called,inf):xs1) types)
			else if op == "sub"
				then (Atrib (Var r1) (Sub (Const r2) (Const r3))):(getCInstruction xs cinstr blbls ((args1,label,called,inf):xs1) types)
				else if op == "div"
					then (Atrib (Var r1) (Div (Const r2) (Const r3))):(getCInstruction xs cinstr blbls ((args1,label,called,inf):xs1) types)
					else if op == "mult"
						then (Atrib (Var r1) (Mult (Const r2)  (Const r3))):(getCInstruction xs cinstr blbls ((args1,label,called,inf):xs1) types)
						else error "ERROR: Undefined operation"
		else if op == "add"
			then let cinstr1=(Atrib (Var r1) (Add (Const r2)  (Const r3))):cinstr
			in getCInstruction xs cinstr1 blbls ((args1,label,called,inf):xs1) types
			else if op == "sub"
				then let cinstr2 = (Atrib (Var r1) (Sub (Const r2) (Const r3))):cinstr
				in getCInstruction xs cinstr2 blbls ((args1,label,called,inf):xs1) types
				else if op == "div"
					then let cinstr3 = (Atrib (Var r1) (Div (Const r2) (Const r3))):cinstr
					in getCInstruction xs cinstr3 blbls ((args1,label,called,inf):xs1) types
					else if op== "mult"
						then let cinstr4 = (Atrib (Var r1) (Mult (Const r2)  (Const r3))):cinstr
							in getCInstruction xs cinstr4 blbls ((args1,label,called,inf):xs1) types
							else error "ERROR: Undefined operation"
		
getCInstruction ((MInst (Op op) ([Reg r1, Reg r2, Lbl l])):xs) cinstr blbls finfo types =
	if l `elem` blbls 
		then if op == "beq"
			then (While (Eq (Const r1) (Const r2)) cinstr ):(getCInstruction xs [] [] finfo types)
			else if op== "bneq"
				then (While (Not (Eq (Const r1) (Const r2))) cinstr ):(getCInstruction xs [] [] finfo types)
				else error "ERROR: Undefined operation"
		else 
			let lbls1=l:blbls
			in if op == "beq"
			then (If (Eq (Const r1) (Const r2)) (getLabelInstructions (xs,l) cinstr lbls1 finfo types)):[( Else (getCInstruction xs [] [] finfo types) )]
			else if op== "bneq"
				then (If (Not (Eq (Const r1) (Const r2))) (getLabelInstructions (xs,l) cinstr lbls1 finfo types)):[( Else (getCInstruction xs [] [] finfo types) )]
				else error "ERROR: Undefined operation"

getCInstruction ((MInst (Op "move") ([Reg r1, Reg r2])):xs) cinstr blbls finfo types= (Atrib (Var r1) (Var r2)):(getCInstruction xs cinstr blbls finfo types)

getCInstruction ((MInst (Op "lw") ([Reg r1, Addr i r2])):xs) cinstr blbls finfo types= (Atrib (Var r1) (Array r2 i)):(getCInstruction xs cinstr blbls finfo types)

getCInstruction ((MInst (Op "sw") ([Addr i r1, Reg r2])):xs) cinstr blbls finfo types= (Atrib (Array r1 i) (Var r2)):(getCInstruction xs cinstr blbls finfo types)