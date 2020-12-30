module Eval where 
    import Name
    import Parser
    import Syntax
    import PreludeOps

    import qualified Data.Map as Map
    import qualified Data.Maybe as Maybe
    import Data.Function (fix)
    import Debug.Trace

    getPosFromExpr :: Expr -> [Pos]
    getPosFromExpr (EVar n)                                      = [getPosFromName n]
    getPosFromExpr (ELit lit)                                    = [getPosLit lit]
    getPosFromExpr (EPack1 s e1 pos)                             = pos : (getPosFromExpr e1) 
    getPosFromExpr (EPack2 s e1 e2 pos)                          = pos : (getPosFromExpr e1) ++ (getPosFromExpr e2) 
    getPosFromExpr (EPack3 s e1 e2 e3 pos)                       = pos : (getPosFromExpr e1) ++ (getPosFromExpr e2) ++ (getPosFromExpr e3)
    getPosFromExpr (EPack4 s e1 e2 e3 e4 pos)                    = pos : (getPosFromExpr e1) ++ (getPosFromExpr e2) ++ (getPosFromExpr e3) ++ (getPosFromExpr e4)
    getPosFromExpr (EPack5 s e1 e2 e3 e4 e5 pos)                 = pos : (getPosFromExpr e1) ++ (getPosFromExpr e2) ++ (getPosFromExpr e3) ++ (getPosFromExpr e4) ++ (getPosFromExpr e5) 
    getPosFromExpr (EPack6 s e1 e2 e3 e4 e5 e6 pos)              = pos : (getPosFromExpr e1) ++ (getPosFromExpr e2) ++ (getPosFromExpr e3) ++ (getPosFromExpr e4) ++ (getPosFromExpr e5) ++ (getPosFromExpr e6) 
    getPosFromExpr (EPack7 s e1 e2 e3 e4 e5 e6 e7 pos)           = pos : (getPosFromExpr e1) ++ (getPosFromExpr e2) ++ (getPosFromExpr e3) ++ (getPosFromExpr e4) ++ (getPosFromExpr e5) ++ (getPosFromExpr e6) ++ (getPosFromExpr e7) 
    getPosFromExpr (EPack8 s e1 e2 e3 e4 e5 e6 e7 e8 pos)        = pos : (getPosFromExpr e1) ++ (getPosFromExpr e2) ++ (getPosFromExpr e3) ++ (getPosFromExpr e4) ++ (getPosFromExpr e5) ++ (getPosFromExpr e6) ++ (getPosFromExpr e7) ++ (getPosFromExpr e8) 
    getPosFromExpr (EPack9 s e1 e2 e3 e4 e5 e6 e7 e8 e9 pos)     = pos : (getPosFromExpr e1) ++ (getPosFromExpr e2) ++ (getPosFromExpr e3) ++ (getPosFromExpr e4) ++ (getPosFromExpr e5) ++ (getPosFromExpr e6) ++ (getPosFromExpr e7) ++ (getPosFromExpr e8) ++ (getPosFromExpr e9)
    getPosFromExpr (EPack10 s e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 pos)= pos : (getPosFromExpr e1) ++ (getPosFromExpr e2) ++ (getPosFromExpr e3) ++ (getPosFromExpr e4) ++ (getPosFromExpr e5) ++ (getPosFromExpr e6) ++ (getPosFromExpr e7) ++ (getPosFromExpr e8) ++ (getPosFromExpr e9) ++ (getPosFromExpr e10)

    getPosLit (LitInt _ pos)      = pos
    getPosLit (LitFloat _ pos)    = pos
    getPosLit (LitChar _ pos)     = pos
    getPosLit (LitString _ pos)   = pos
    getPosLit (LitBool _ pos)     = pos

    foldlLam :: [Pattern] -> Expr -> Expr
    foldlLam [] e = e
    foldlLam (p:ps) e = ELam [p] (foldlLam ps e)

    foldlApp :: [Expr] -> Expr -> Expr
    foldlApp [] e = e 
    foldlApp (x:xs) e = EApp x (foldlApp xs e)

    desugar :: Expr -> Expr
    desugar (EApp fun arg) = EApp (desugar fun) (desugar arg)   
    desugar (ELam ps body) =
        foldlLam ps (desugar body)   
    desugar (ELetIn xs body) = foldl EApp (desugar (ELam pats body)) (map desugar es)
        where (pats, es) = unzip xs
    desugar (EFix e) = EApp (EVar (Name "$FIX" (0,0))) (desugar e)    
    desugar (ECase e matches) = buildCase (map desugarMatch matches) (desugar e)
    desugar (EIf cond tr fl) =
        foldl EApp (EVar (Name "$IF" (0,0))) args
            where args = map desugar [cond, tr, fl]
    desugar (EOp op a b) = 
        foldl EApp (EVar n) args
        where
            args = map desugar [a, b]
            n = case op of
                (Operator Add pos) -> Name "$ADD" pos
                (Operator Sub pos) -> Name "$SUB" pos
                (Operator Mul pos) -> Name "$MUL" pos
                (Operator Div pos) -> Name "$DIV" pos
                (Operator Eq pos ) -> Name "$EQL" pos
                (Operator Neq pos) -> Name "$NEQ" pos
                (Operator Gt pos ) -> Name "$GRT" pos
                (Operator Lt pos ) -> Name "$LST" pos
                (Operator Ge pos ) -> Name "$GRE" pos
                (Operator Le pos ) -> Name "$LSE" pos
                (Operator And pos) -> Name "$AND" pos
                (Operator Or pos ) -> Name "$OR"  pos
                (Operator App pos) -> Name "$APP" pos
                (Operator Pow pos) -> Name "$POW" pos
                (Gen "Cons" _ pos) -> Name "$CONS" pos
    desugar (EFatBar e1 e2) = desugar (EIf (EOp (Operator Eq (0,0)) e1 EFail) e2 e1)
    desugar e = e

    buildCase :: [([Pattern], [Expr], Expr)] -> Expr -> Expr
    buildCase [] e = EFail
    buildCase ((ps,exprs,body):xs) e = 
        case (head ps) of
            PVar n -> EApp (ELam [PVar n] body) e
            PCon (Gen n k p) pats -> desugar (EIf (EOp (Operator Eq (0,0)) (desugarPattern (head ps)) e) (ELetIn (buildLet2 pats k 1 e) body) (buildCase xs e))
            PLit lit -> desugar (EIf (EOp (Operator Eq (0,0)) (ELit lit) e) body (buildCase xs e))

    desugarMatch :: Match -> ([Pattern], [Expr], Expr)
    desugarMatch (Match pats body guards line) = (pats, guards, desugar body)

    getName (Gen n _ pos) = (n,pos)
    getName (Name n pos) = (n,pos)

    desugarPattern :: Pattern -> Expr
    desugarPattern (PCon constr pats) = do
        let (n, pos) = getName constr
            newPats = (map desugarPattern pats)
        case (length newPats) of
            0 -> EPack0 n pos
            1 -> EPack1 n (head newPats) pos
            2 -> EPack2 n (newPats !! 0) (newPats !! 1) pos
            3 -> EPack3 n (newPats !! 0) (newPats !! 1) (newPats !! 2) pos
            4 -> EPack4 n (newPats !! 0) (newPats !! 1) (newPats !! 2) (newPats !! 3) pos
            5 -> EPack5 n (newPats !! 0) (newPats !! 1) (newPats !! 2) (newPats !! 3) (newPats !! 4) pos
            6 -> EPack6 n (newPats !! 0) (newPats !! 1) (newPats !! 2) (newPats !! 3) (newPats !! 4) (newPats !! 5) pos
            7 -> EPack7 n (newPats !! 0) (newPats !! 1) (newPats !! 2) (newPats !! 3) (newPats !! 4) (newPats !! 5) (newPats !! 6) pos
            8 -> EPack8 n (newPats !! 0) (newPats !! 1) (newPats !! 2) (newPats !! 3) (newPats !! 4) (newPats !! 5) (newPats !! 6) (newPats !! 7) pos
            9 -> EPack9 n (newPats !! 0) (newPats !! 1) (newPats !! 2) (newPats !! 3) (newPats !! 4) (newPats !! 5) (newPats !! 6) (newPats !! 7) (newPats !! 8) pos
            10 -> EPack10 n (newPats !! 0) (newPats !! 1) (newPats !! 2) (newPats !! 3) (newPats !! 4) (newPats !! 5) (newPats !! 6) (newPats !! 7) (newPats !! 8) (newPats !! 9) pos
    desugarPattern (PVar n)                 = EVar n
    desugarPattern (PLit (LitInt k pos))    = ELit (LitInt k pos)
    desugarPattern (PLit (LitFloat k pos))  = ELit (LitFloat k pos)
    desugarPattern (PLit (LitChar c pos))   = ELit (LitChar c pos)
    desugarPattern (PLit (LitString s pos)) = ELit (LitString s pos)
    desugarPattern (PLit (LitBool b pos))   = ELit (LitBool b pos)
    desugarPattern (PWild pos)              = EVar (Name "_" pos)

    getStrFromName :: Name -> String
    getStrFromName (Gen n _ _) = n
    getStrFromName (Name n _)  = n

    compile :: Expr -> CExpr
    compile (EVar (Name n p))                                = CVar n [p]
    compile (EVar (Gen n k p))                               = CVar n [p]
    compile (EApp fun arg)                                   = CApp (compile fun) (compile arg) 
    compile (ELam ps body)                                   = foldr abstract (compile body) ps 
    compile (ELit (LitInt k pos))                            = CInt k [pos]
    compile (ELit (LitFloat k pos))                          = CFloat k [pos]
    compile (ELit (LitString s pos))                         = CString s [pos]
    compile (ELit (LitChar s pos))                           = CChar s [pos]
    compile (ELit (LitBool k pos))                           = CBool k [pos]
    compile (EFail)                                          = CFail []
    compile (EPack0 n pos)                                   = Pack0 n [pos]
    compile e@(EPack1 n e1 pos)                              = Pack1 n (compile e1) (pos : (getPosFromExpr e))
    compile e@(EPack2 n e1 e2 pos)                           = Pack2 n (compile e1) (compile e2) (pos : (getPosFromExpr e))
    compile e@(EPack3 n e1 e2 e3 pos)                        = Pack3 n (compile e1) (compile e2) (compile e3) (pos : (getPosFromExpr e))
    compile e@(EPack4 n e1 e2 e3 e4 pos)                     = Pack4 n (compile e1) (compile e2) (compile e3) (compile e4) (pos : (getPosFromExpr e))
    compile e@(EPack5 n e1 e2 e3 e4 e5 pos)                  = Pack5 n (compile e1) (compile e2) (compile e3) (compile e4) (compile e5) (pos : (getPosFromExpr e))
    compile e@(EPack6 n e1 e2 e3 e4 e5 e6 pos)               = Pack6 n (compile e1) (compile e2) (compile e3) (compile e4) (compile e5) (compile e6) (pos : (getPosFromExpr e))
    compile e@(EPack7 n e1 e2 e3 e4 e5 e6 e7 pos)            = Pack7 n (compile e1) (compile e2) (compile e3) (compile e4) (compile e5) (compile e6) (compile e7) (pos : (getPosFromExpr e))
    compile e@(EPack8 n e1 e2 e3 e4 e5 e6 e7 e8 pos)         = Pack8 n (compile e1) (compile e2) (compile e3) (compile e4) (compile e5) (compile e6) (compile e7) (compile e8) (pos : (getPosFromExpr e))
    compile e@(EPack9 n e1 e2 e3 e4 e5 e6 e7 e8 e9 pos)      = Pack9 n (compile e1) (compile e2) (compile e3) (compile e4) (compile e5) (compile e6) (compile e7) (compile e8) (compile e9) (pos : (getPosFromExpr e))
    compile e@(EPack10 n e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 pos) = Pack10 n (compile e1) (compile e2) (compile e3) (compile e4) (compile e5) (compile e6) (compile e7) (compile e8) (compile e9) (compile e10) (pos : (getPosFromExpr e))

    getPatternName :: Pattern -> String 
    getPatternName (PCon (Gen n k p) pats) = n
    getPatternName (PVar (Name n p))       = n
    getPatternName (PVar (Gen n k p))      = n
    getPatternName (PWild p)               = "_"
    getPatternName (PLit (LitInt k pos))   = show k
    getPatternName (PLit (LitChar k pos))   = show k

    abstract :: Pattern -> CExpr -> CExpr
    abstract p (CApp fun arg) = combS [] (abstract p fun) (abstract p arg)
    abstract p (CVar n pos) | (getPatternName p) == n = combI pos
    abstract p k = combK k

    combS :: [Pos] -> CExpr -> CExpr -> CExpr
    combS pos f g = CApp (CApp (CVar "$S" pos) f) g

    combK :: CExpr -> CExpr
    combK k = CApp (CVar "$K" []) k 

    combI :: [Pos] -> CExpr
    combI pos = CVar "$I" pos

    primitives :: [(String, CExpr)]
    primitives =
        [ ("$I", CLam [] $ \x -> x)
        , ("$K", CLam [] $ \x -> CLam [] $ \_ -> x)
        , ("$S", CLam [] $ \f -> CLam [] $ \g -> CLam [] $ \x ->  (f ! x ! (g ! x)))
        , ("$IF", CLam [] $ \(CBool cond l) -> CLam [] $ \tr -> CLam [] $ \fl -> if cond then (appendLineInCExpr tr l) else (appendLineInCExpr fl l))
        , ("$FIX", CLam [] $ \(CLam p f) -> fix f)
        , ("$ADD", CLam [] $ \a -> CLam [] $ \b -> add a b)
        , ("$SUB", CLam [] $ \a -> CLam [] $ \b -> sub a b)
        , ("$MUL", CLam [] $ \a -> CLam [] $ \b -> mul a b)
        , ("$DIV", CLam [] $ \a -> CLam [] $ \b -> division a b)
        , ("$POW", CLam [] $ \a -> CLam [] $ \b -> pow a b)
        , ("$EQL", CLam [] $ \a -> CLam [] $ \b -> eq a b)
        , ("$NEQ", CLam [] $ \a -> CLam [] $ \b -> neq a b)
        , ("$GRT", CLam [] $ \a -> CLam [] $ \b -> grt a b)
        , ("$LST", CLam [] $ \a -> CLam [] $ \b -> lst a b)
        , ("$GRE", CLam [] $ \a -> CLam [] $ \b -> gre a b)
        , ("$LSE", CLam [] $ \a -> CLam [] $ \b -> lse a b)
        , ("$AND", CLam [] $ \a -> CLam [] $ \b -> logical_and a b)
        , ("$OR", CLam [] $ \a -> CLam [] $ \b -> logical_or a b)
        , ("$APP", CLam [] $ \a -> CLam [] $ \b -> append a b)
        , ("$CONS", CLam [] $ \a -> CLam [] $ \b -> cons a b)
        , ("Tuple-2", pack_2 "Tuple-2" [])
        , ("Tuple-3", pack_3 "Tuple-3" [])
        , ("Tuple-4", pack_4 "Tuple-4" [])
        , ("Tuple-5", pack_5 "Tuple-5" [])
        , ("Tuple-6", pack_6 "Tuple-6" [])
        , ("Tuple-7", pack_7 "Tuple-7" [])
        , ("Tuple-8", pack_8 "Tuple-8" [])
        , ("Tuple-9", pack_9 "Tuple-9" [])
        , ("Tuple-10", pack_10 "Tuple-10" [])
        , ("True", true [])
        , ("False", false [])
        , ("Cons", pack_2 "Cons" [])
        , ("Nil", Pack0 "Nil" [])
        , ("$SEL_1_1", CLam [] $ \x -> sel_1_1 x)
        , ("$SEL_1_2", CLam [] $ \x -> sel_1_2 x)
        , ("$SEL_2_2", CLam [] $ \x -> sel_2_2 x)
        , ("$SEL_1_3", CLam [] $ \x -> sel_1_3 x)
        , ("$SEL_2_3", CLam [] $ \x -> sel_2_3 x)
        , ("$SEL_3_3", CLam [] $ \x -> sel_3_3 x)        
        , ("$SEL_1_4", CLam [] $ \x -> sel_1_4 x)
        , ("$SEL_2_4", CLam [] $ \x -> sel_2_4 x)
        , ("$SEL_3_4", CLam [] $ \x -> sel_3_4 x)
        , ("$SEL_4_4", CLam [] $ \x -> sel_4_4 x)        
        , ("$SEL_1_5", CLam [] $ \x -> sel_1_5 x)
        , ("$SEL_2_5", CLam [] $ \x -> sel_2_5 x)
        , ("$SEL_3_5", CLam [] $ \x -> sel_3_5 x)
        , ("$SEL_4_5", CLam [] $ \x -> sel_4_5 x)
        , ("$SEL_5_5", CLam [] $ \x -> sel_5_5 x)        
        , ("$SEL_1_6", CLam [] $ \x -> sel_1_6 x)
        , ("$SEL_2_6", CLam [] $ \x -> sel_2_6 x)
        , ("$SEL_3_6", CLam [] $ \x -> sel_3_6 x)
        , ("$SEL_4_6", CLam [] $ \x -> sel_4_6 x)
        , ("$SEL_5_6", CLam [] $ \x -> sel_5_6 x)
        , ("$SEL_6_6", CLam [] $ \x -> sel_6_6 x)        
        , ("$SEL_1_7", CLam [] $ \x -> sel_1_7 x)
        , ("$SEL_2_7", CLam [] $ \x -> sel_2_7 x)
        , ("$SEL_3_7", CLam [] $ \x -> sel_3_7 x)
        , ("$SEL_4_7", CLam [] $ \x -> sel_4_7 x)
        , ("$SEL_5_7", CLam [] $ \x -> sel_5_7 x)
        , ("$SEL_6_7", CLam [] $ \x -> sel_6_7 x)
        , ("$SEL_7_7", CLam [] $ \x -> sel_7_7 x)        
        , ("$SEL_1_8", CLam [] $ \x -> sel_1_8 x)
        , ("$SEL_2_8", CLam [] $ \x -> sel_2_8 x)
        , ("$SEL_3_8", CLam [] $ \x -> sel_3_8 x)
        , ("$SEL_4_8", CLam [] $ \x -> sel_4_8 x)
        , ("$SEL_5_8", CLam [] $ \x -> sel_5_8 x)
        , ("$SEL_6_8", CLam [] $ \x -> sel_6_8 x)
        , ("$SEL_7_8", CLam [] $ \x -> sel_7_8 x)
        , ("$SEL_8_8", CLam [] $ \x -> sel_8_8 x)        
        , ("$SEL_1_9", CLam [] $ \x -> sel_1_9 x)
        , ("$SEL_2_9", CLam [] $ \x -> sel_2_9 x)
        , ("$SEL_3_9", CLam [] $ \x -> sel_3_9 x)
        , ("$SEL_4_9", CLam [] $ \x -> sel_4_9 x)
        , ("$SEL_5_9", CLam [] $ \x -> sel_5_9 x)
        , ("$SEL_6_9", CLam [] $ \x -> sel_6_9 x)
        , ("$SEL_7_9", CLam [] $ \x -> sel_7_9 x)
        , ("$SEL_8_9", CLam [] $ \x -> sel_8_9 x)
        , ("$SEL_9_9", CLam [] $ \x -> sel_9_9 x)        
        , ("$SEL_1_10", CLam [] $ \x -> sel_1_10 x)
        , ("$SEL_2_10", CLam [] $ \x -> sel_2_10 x)
        , ("$SEL_3_10", CLam [] $ \x -> sel_3_10 x)
        , ("$SEL_4_10", CLam [] $ \x -> sel_4_10 x)
        , ("$SEL_5_10", CLam [] $ \x -> sel_5_10 x)
        , ("$SEL_6_10", CLam [] $ \x -> sel_6_10 x)
        , ("$SEL_7_10", CLam [] $ \x -> sel_7_10 x)
        , ("$SEL_8_10", CLam [] $ \x -> sel_8_10 x)
        , ("$SEL_9_10", CLam [] $ \x -> sel_9_10 x)
        , ("$SEL_10_10", CLam [] $ \x -> sel_10_10 x)
        ]
    
    type TermEnv = Map.Map String CExpr

    emptyTmenv :: TermEnv
    emptyTmenv = Map.fromList primitives

    link :: TermEnv -> CExpr -> CExpr    
    link bs (CApp fun arg) = link bs fun ! link bs arg
    link bs (CVar n l) = appendLineInCExpr (Maybe.fromJust (Map.lookup n bs)) l
    link _ e = e

    eval :: TermEnv -> Expr -> CExpr
    eval env = link env . compile . desugar

    runEval :: TermEnv -> String -> Expr -> (CExpr, TermEnv)
    runEval env nm ex =
        let res = eval env ex in
            (res, Map.insert nm res env)

    getPosFromName :: Name -> Pos 
    getPosFromName (Gen _ _ p)     = p
    getPosFromName (Name n p)      = p
    getPosFromName (Operator _ p)  = p

    insertPos :: Name -> Pos -> Name
    insertPos (Gen n k _) p = Gen n k p
    insertPos (Name n _ ) p = Name n p

    -- substitui segunda pela primeira 
    subst :: Expr -> Name -> Pattern -> Expr
    subst e v u = 
        case u of 
            PWild _ -> e 
            PLit k -> EIf (EOp (Operator Eq (0,0)) (ELit k) (EVar v)) e EFail
            _ -> case e of 
                EApp fun arg -> EApp (subst fun v u) (subst arg v u) 
                EFix e -> EFix (subst e v u)
                EOp op e1 e2 -> EOp op (subst e1 v u) (subst e2 v u)
                EIf cond tr fl -> EIf (subst cond v u) (subst tr v u) (subst fl v u)
                ELetIn pairs body -> ELetIn newPats (subst body v u)
                                        where (l1,l2) = unzip pairs
                                              es = mapSubst l2 v u
                                              newPats = zip l1 es
                ELam ps body -> ELam ps (subst body v u)
                ECase e matches -> ECase (subst e v u) (substM matches v u)
                EVar n
                    | (getPatternName u) == (getStrFromName n) -> EVar (insertPos v (getPosFromName n)) 
                EPack1 str exp pos -> EPack1 str (subst exp v u) pos
                _ -> e

    substM [] _ _     = []
    substM ((Match pats body guards p):es) v u = (Match pats (subst body v u) (mapSubstG guards v u) p) : substM es v u

    mapSubst :: [Expr] -> Name -> Pattern -> [Expr] 
    mapSubst [] n p = []
    mapSubst (e:es) n p = subst e n p : mapSubst es n p

    mapSubstG :: [Expr] -> Name -> Pattern -> [Expr] 
    mapSubstG [] _ _ = []
    mapSubstG (e:es) n p = substitui e n p ++ mapSubstG es n p

    substitui :: Expr -> Name -> Pattern -> [Expr]
    substitui e n (PCon _ []) = []
    substitui e n (PCon c (p:ps)) = do 
        let newExp = substGuards e n p
        if newExp == EFail
            then substitui e n (PCon c ps)
            else newExp : substitui e n (PCon c ps)

    substGuards :: Expr -> Name -> Pattern -> Expr
    substGuards e v u@(PVar n) = 
        case e of 
            EApp fun arg -> EApp (substGuards fun v u) (substGuards arg v u) 
            EFix e -> EFix (substGuards e v u)
            EOp op e1 e2 -> EOp op (substGuards e1 v u) (substGuards e2 v u)
            EIf cond tr fl -> EIf (substGuards cond v u) (substGuards tr v u) (substGuards fl v u)
            ELetIn pairs body -> ELetIn newPats (substGuards body v u)
                                    where (l1,l2) = unzip pairs
                                          es = mapSubstG l2 v u
                                          newPats = zip l1 es
            ELam ps body -> ELam ps (substGuards body v u)
            EVar n1 | n == n1 -> EVar (insertPos v (getPosFromName n1)) 
            _ -> EFail

    makeVar :: Int -> Name
    makeVar k = Name ("_u" ++ show k) (0,0)

    makeVarList :: Int -> [Name]
    makeVarList k = [makeVar i | i <- [1..k]]

    isVar :: ([Pattern],[Expr],Expr) -> Bool
    isVar (PVar{} : ps, _, e)  = True
    isVar (PWild{} : ps, _, e) = True
    isVar (PLit{} : ps,_, e) = True
    isVar _ = False

    isCon :: ([Pattern],[Expr],Expr) -> Bool
    isCon (PCon{} : ps, _, e) = True
    isCon _ = False    

    getCon :: ([Pattern],[Expr],Expr) -> Constr
    getCon (PCon c _ :ps,_,e) = c

    arity :: Name -> Int
    arity (Gen n k _) = fromInteger k

    constructors :: Map.Map String [Name] -> Constr -> [Constr]
    constructors env c@(Gen n k p) = 
        case n of 
            "Cons" -> c : [Gen "Nil" 0 p]
            "Nil"  -> c : [Gen "Cons" 2 p]
            _ -> if Maybe.isJust (Map.lookup n env)
                    then Maybe.fromJust (Map.lookup n env)
                    else [c]

    partition f [ ] = [ ]
    partition f [x] = [ [x] ]
    partition f ( x : x' : xs)
        | f x == f x' = tack x (partition f (x':xs))
        | otherwise   = [x] : partition f (x' : xs)
        
    tack x xss = (x : head xss) : tail xss      

    matchVarCon env k us qs def  
        | isVar (head qs) = matchVar env k us qs def
        | isCon (head qs) = matchCon env k us qs def  

    matchVar env k (u:us) qs def = 
         match env k us [(ps, mapSubst guards u v,subst e u v) | (v : ps, guards, e) <- qs] def    

    matchCon env k (u:us) qs def = 
         ECase (EVar u) [matchClause env c k (u:us) (choose c qs) def | c <- cs]
                where cs = concat (map (constructors env) (map getCon qs))

    matchClause env c k (u:us) qs def = 
        Match [PCon c (transformNames us')] (
                    match env (k' + k)
                    (us'++ us)
                    [(ps' ++ ps,gs,e) | (PCon c ps':ps,gs,e)<-qs] 
                    def)
                    (concat pus)                   
                    (0,0)
            where k' = arity c 
                  us' = [makeVar (i+k) | i <- [1..k']]
                  pus = [mapSubstG gs u p | (p:ps,gs,_) <- qs]    

    choose c qs = [q | q <- qs, getCon q == c]

    buildGuard :: [Expr] -> Expr
    buildGuard [] = ELit (LitBool True (0,0))
    buildGuard (e:es) = if null es 
        then do if e == EVar (Name "otherwise" (0,0))
                    then ELit (LitBool True (0,0))
                    else e
        else EOp (Operator And (0,0)) e (buildGuard es)

    match :: Map.Map String [Name] -> Int -> [Name] -> [([Pattern],[Expr],Expr)] -> Expr -> Expr
    match env k [ ] qs def
        = foldr EFatBar def [hasGuard g e | ([],g,e) <- qs]
    match env k (u:us) qs def
        = foldr (matchVarCon env k (u:us)) def (partition isVar qs)

    hasGuard [] e = e 
    hasGuard g e = EIf (buildGuard g) e EFail

    transformNames :: [Name] -> [Pattern]
    transformNames [] = [] 
    transformNames (n:ns) = PVar n : transformNames ns

    compileDecl :: Map.Map String [Name] -> Decl -> (Name, Expr)
    compileDecl env decl = do 
                     let (BindGroup name pats ty line) = head (fgroup decl)
                         matches = map desugarMatch pats
                         (t,_,_) = head matches
                         names = (makeVarList (length t))
                         ex = match env (length t) names matches EFail
                         newPats = transformNames names
                      in (name, EFix (ELam (PVar name:newPats) ex))                    

    callRunEval :: [(Name, Expr)] -> TermEnv -> (CExpr, TermEnv)
    callRunEval exps env = if length exps == 1
        then let ((Name name p), e) = head exps
                in runEval env name e 
        else let ((Name name p), e) = head exps
                 (res, newEnv) = runEval env name e in callRunEval (tail exps) newEnv   
