module Main where
	import Eval
	import Syntax
	import Parser
	import PreludeOps
	import Name
	import qualified Data.Map as Map
	import qualified Data.Maybe as Maybe

	import System.Environment
	import Debug.Trace
	import Data.List as List

	type StackTests = Map.Map Pos Integer

	getData :: [Decl] -> [Decl]
	getData [] = []
	getData (d:ds) = case d of 
		DataDecl {} -> d : getData ds 
		_ -> getData ds 

	compileData :: [Decl] -> TermEnv -> TermEnv 
	compileData [] env = env
	compileData (d@(DataDecl c n condecl p):ds) env = 
		let newEnv = insertConDecl condecl env
			in compileData ds newEnv

	insertConDecl :: [ConDecl] -> TermEnv -> TermEnv
	insertConDecl [] env = env
	insertConDecl ((ConDecl (Gen nm k pos) _):cs) env = 
		let newEnv = Map.insert nm (getCExpr k nm pos) env
			in insertConDecl cs newEnv

 	tarantula :: [(Pos,(Integer,Integer))] -> Int -> Int -> [(Float, Pos)] 
 	tarantula [] _ _ = []
 	tarantula ((pos,(p,f)):ps) totPass totFail = ((((fromIntegral f) / (fromIntegral totFail)) / (((fromIntegral f) / (fromIntegral totFail)) + ((fromIntegral p) / (fromIntegral totPass))), pos)) : tarantula ps totPass totFail

 	ochiai :: [(Pos,(Integer,Integer))] -> Int -> [(Float, Pos)] 
 	ochiai [] _ = []
 	ochiai ((pos,(p,f)):ps) totFail = ((fromIntegral f) / sqrt ((fromIntegral totFail) * (fromIntegral (f + p))),pos) : ochiai ps totFail

 	op2 :: [(Pos,(Integer,Integer))] -> Int -> [(Float, Pos)] 
 	op2 [] _ = []
 	op2 ((pos,(p,f)):ps) totPass = ((fromIntegral f) - ((fromIntegral p) / (fromIntegral (totPass + 1))), pos) : op2 ps totPass

 	barinel :: [(Pos,(Integer,Integer))] -> [(Float, Pos)] 
 	barinel [] = []
 	barinel ((pos,(p,f)):ps) = ((fromIntegral 1) - ((fromIntegral p) / (fromIntegral (p + f))), pos) : barinel ps

 	pushMap :: StackTests -> [Pos] -> StackTests
 	pushMap stack [] = stack
 	pushMap stack (p:ps) = 
 		if p == (0,0)
 			then pushMap stack ps 
 			else let newStack = Map.insert p (fromInteger 1) stack
		    						in pushMap newStack ps 

 	getStack :: TermEnv -> Expr -> StackTests
 	getStack env ex = pushMap Map.empty (getPosFromCExpr (eval env ex))

 	addStacks :: StackTests -> [(Pos, Integer)] -> StackTests
 	addStacks stack [] = stack 
 	addStacks stack ((p,i):ps) = 
		if (/= Nothing) (Map.lookup p stack)
			then let newStack = Map.insertWith (+) p i stack
						in addStacks newStack ps 
			else let newStack = Map.insert p i stack
						in addStacks newStack ps 

 	runAllTests :: TermEnv -> [Expr] -> StackTests -> StackTests
 	runAllTests _ [] stack = stack
 	runAllTests env (ex:exes) stack = runAllTests env exes newStack 
 			where newStack = addStacks (getStack env ex) (Map.assocs stack)

 	-- pass, fail
 	groupPassFail :: [(Pos, Integer)] -> Map.Map Pos (Integer,Integer) -> Map.Map Pos (Integer,Integer)
 	groupPassFail [] map = map 
 	groupPassFail ((k,v):ps) map = let newMap = Map.insert k (v,0) map
 					in groupPassFail ps newMap

	iterateOnFail :: [(Pos,Integer)] -> Map.Map Pos (Integer,Integer) -> Map.Map Pos (Integer,Integer)
	iterateOnFail [] map = map 
	iterateOnFail ((k,v):ps) map = 
		if (/= Nothing) (Map.lookup k map)
			then do let (v1,v2) = Maybe.fromJust (Map.lookup k map) in
					let newMap = Map.insert k (v1,v) map
						in iterateOnFail ps newMap
			else let newMap = Map.insert k (0,v) map
					in iterateOnFail ps newMap
		
	parseTests ::  [String] -> [Expr] 
	parseTests [] = []
	parseTests (s:ss) = (modifyPos (parseExp s)) : parseTests ss

	prettyPrint [] = ""
	prettyPrint ((l,s):xs) = ("Line = " ++ show l ++ ", Score = " ++ show (round3dp s) ++ "\n") ++ prettyPrint xs

	removeRepeatedLine [] acc = []
	removeRepeatedLine ((f,(l,c)):xs) acc = if l `elem` acc
		then removeRepeatedLine xs acc
		else (l,f) : (removeRepeatedLine xs (l : acc))

	round3dp x = fromIntegral (round $ x * 1e3) / 1e3

	main = do 
		args <- getArgs
		program <- readFile (args !! 0)
		testsPassing <- readFile (args !! 1)
		testsFailing <- readFile (args !! 2)

		let tests_pass = parseTests $ lines testsPassing
		    tests_fail = parseTests $ lines testsFailing
		
		let ast = parse program
		let datas = getData ast
		let group = groupDecls ast
		let env = compileData datas emptyTmenv
		let constructs = getConstrMap datas Map.empty

		let exps = map (compileDecl constructs) group
		let (res, newEnv) = callRunEval exps env

		let stack_pass = runAllTests newEnv tests_pass Map.empty
		let stack_fail = runAllTests newEnv tests_fail Map.empty
		let total_pass = length stack_pass
		let total_fail = length stack_fail

		let groupedMap1 = groupPassFail (Map.assocs stack_pass) Map.empty
		let groupedMap2 = iterateOnFail (Map.assocs stack_fail) groupedMap1

		let resT = List.reverse (List.sort (tarantula (Map.toList groupedMap2) (length tests_pass) (length tests_fail)))
		let resO = List.reverse (List.sort (ochiai (Map.toList groupedMap2) (length tests_fail)))

		print "## HaskellFL ##"
		if length args == 3
			then userInterface resT resO
			else do
				case (args !! 3) of 
					"tarantula" -> do 
						print "Tarantula: "
						putStrLn $ prettyPrint (removeRepeatedLine resT [])					
					"Tarantula" -> do 
						print "Tarantula: "
						putStrLn $ prettyPrint (removeRepeatedLine resT [])
					"ochiai" -> do 
						print "Ochiai: "
						putStrLn $ prettyPrint (removeRepeatedLine resO [])					
					"Ochiai" -> do 
						print "Ochiai: "
						putStrLn $ prettyPrint (removeRepeatedLine resO [])
					"run" -> do
						let functionName = (args !! 3)
						print "Passing Tests:"
						putStrLn (buildRun functionName tests_pass newEnv)
						print "Failing Tests:"
						putStrLn (buildRun functionName tests_fail newEnv)
					_ -> return ()

	buildRun _ [] env = ""
	buildRun name (ex:exs) env = let (res, _) = runEval env name ex
		in "Output: " ++ (show res) ++ "\n" ++ buildRun name exs env


	userInterface resT resO = do
		print "Enter 1 for Tarantula, 2 for Ochiai or 3 to quit: "
		method <- getLine	
		case method of 
			"1" -> do 
				putStrLn $ prettyPrint (removeRepeatedLine resT [])
				userInterface resT resO
			"2" -> do 
				putStrLn $ prettyPrint (removeRepeatedLine resO [])
				userInterface resT resO
			"3" -> return ()
			_ -> userInterface resT resO

