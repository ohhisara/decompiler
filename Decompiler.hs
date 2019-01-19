module Decompiler where
	import MIPS
	import C
	import InstrDecompiler
	import Types
	import  PrettyP

	decompiler:: [MIPSInstruction] -> ([(CVar,CType)],[CInstruction])
	decompiler minst =
		let types = (recoverType minst [])
		in ((recoverType minst []),(getCInstruction minst [] [] [([],Nothing,False,False)]) types)


	{-ppTypes::[(CVar,CType)] -> String
	ppTypes [] = ""
	ppTypes ((var,tp):xs) = (show tp ++ " " ++ var ++ ";\n")++(ppTypes xs)

	ppInstr::[CInstruction] -> String
	ppInstr [] = ""
	ppInstr (instr:xs) = (show instr) ++ (ppInstr xs)-}

	pp::([(CVar,CType)],[CInstruction])->String
	pp (types,instrs) = (ppTypes types)++(ppInstrs instrs)

	out::[MIPSInstruction]->IO()
	out mips = putStr (removeDollar(pp (decompiler mips)))



