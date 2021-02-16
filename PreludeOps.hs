module PreludeOps where 
    import Name
    import Syntax
    import Debug.Trace

    data CExpr
        = CVar String [Pos]
        | CApp CExpr CExpr
        | CLam [Pos] (CExpr -> CExpr) 
        | CBool Bool [Pos]
        | CInt Integer [Pos]
        | CFloat Float [Pos]
        | CString String [Pos]
        | CChar Char [Pos]
        | CFail [Pos]
        | Pack0 String [Pos]
        | Pack1 String CExpr [Pos]
        | Pack2 String CExpr CExpr [Pos]
        | Pack3 String CExpr CExpr CExpr [Pos]
        | Pack4 String CExpr CExpr CExpr CExpr [Pos]
        | Pack5 String CExpr CExpr CExpr CExpr CExpr [Pos]
        | Pack6 String CExpr CExpr CExpr CExpr CExpr CExpr [Pos]
        | Pack7 String CExpr CExpr CExpr CExpr CExpr CExpr CExpr [Pos]
        | Pack8 String CExpr CExpr CExpr CExpr CExpr CExpr CExpr CExpr [Pos]
        | Pack9 String CExpr CExpr CExpr CExpr CExpr CExpr CExpr CExpr CExpr [Pos]
        | Pack10 String CExpr CExpr CExpr CExpr CExpr CExpr CExpr CExpr CExpr CExpr [Pos]

    instance Show CExpr where
        show (CVar n l) = show n ++ " " 
        show (CApp c1 c2) = show c1 ++ " " ++ show c2 
        show CLam{} = "<<closure>>"
        show (CBool n l) = show n 
        show (CInt n l) = show n 
        show (CFloat n l) = show n 
        show (CString n l) = "\"" ++ n ++ "\""
        show (CChar c l) = show c
        show (CFail p) = "Fail" 
        show (Pack0 nm pos) = nm
        show (Pack1 nm c1 pos) = nm ++ " " ++ show c1
        show (Pack2 nm c1 c2 pos) = "(" ++ nm ++ " " ++ show c1 ++ " " ++ show c2 ++ ")"
        show (Pack3 nm c1 c2 c3 pos) = "(" ++ nm ++ " " ++ show c1 ++ " " ++ show c2 ++ " " ++ show c3 ++ ")" -- " " ++ show pos
        show (Pack4 nm c1 c2 c3 c4 pos) = "(" ++ nm ++ " " ++ show c1 ++ " " ++ show c2 ++ " " ++ show c3 ++ show c4 ++ ")" --" " ++ show pos
        show (Pack5 nm c1 c2 c3 c4 c5 pos) = "(" ++ nm ++ " " ++ show c1 ++ " " ++ show c2 ++ " " ++ show c3 ++ " " ++ show c4 ++ " " ++ show c5 ++ ")" -- " " ++ show pos
        show (Pack6 nm c1 c2 c3 c4 c5 c6 pos) = "(" ++ nm ++ " " ++ show c1 ++ " " ++ show c2 ++ " " ++ show c3 ++ " " ++ show c4 ++ " " ++ show c5 ++ " " ++ show c6 ++ ")" -- " " ++ show pos
        show (Pack7 nm c1 c2 c3 c4 c5 c6 c7 pos) = "(" ++ nm ++ " " ++ show c1 ++ " " ++ show c2 ++ " " ++ show c3 ++ " " ++ show c4 ++ " " ++ show c5 ++ " " ++ show c6 ++ " " ++ show c7 ++ ")" -- " " ++ show pos
        show (Pack8 nm c1 c2 c3 c4 c5 c6 c7 c8 pos) = "(" ++ nm ++ " " ++ show c1 ++ " " ++ show c2 ++ " " ++ show c3 ++ " " ++ show c4 ++ " " ++ show c5 ++ " " ++ show c6 ++ " " ++ show c7 ++ " " ++ show c8 ++ ")" -- " " ++ show pos
        show (Pack9 nm c1 c2 c3 c4 c5 c6 c7 c8 c9 pos) = "(" ++ nm ++ " " ++ show c1 ++ " " ++ show c2 ++ " " ++ show c3 ++ " " ++ show c4 ++ " " ++ show c5 ++ " " ++ show c6 ++ " " ++ show c7 ++ " " ++ show c8 ++ " " ++ show c9 ++ ")" -- " " ++ show pos
        show (Pack10 nm c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 pos) = "(" ++ nm ++ " " ++ show c1 ++ " " ++ show c2 ++ " " ++ show c3 ++ " " ++ show c4 ++ " " ++ show c5 ++ " " ++ show c6 ++ " " ++ show c7 ++ " " ++ show c8 ++ " " ++ show c9 ++ " " ++ show c10 ++ ")" -- " " ++ show pos

    instance Eq CExpr where
        (Pack0 n1 pos) == (Pack0 n2 _) = (n1 == n2) 
        (Pack1 n1 a pos) == (Pack1 n2 b _) = (==) n1 n2 && (validatePattern a b)
        (Pack2 n1 a b pos) == (Pack2 n2 c d _) = (==) n1 n2 && (validatePattern a c) && (validatePattern b d)
        (Pack3 n1 a b c pos) == (Pack3 n2 d e f _) = (==) n1 n2 && (validatePattern a d) && (validatePattern b e) && (validatePattern c f)
        (Pack4 n1 a b c d pos) == (Pack4 n2 e f g h _) = (==) n1 n2 && (validatePattern a e) && (validatePattern b f) && (validatePattern c g) && (validatePattern d h)
        (Pack5 n1 a b c d e pos) == (Pack5 n2 f g h i j _) = (==) n1 n2 && (validatePattern a f) && (validatePattern b g) && (validatePattern c h) && (validatePattern d i) && (validatePattern e j)
        (Pack6 n1 a b c d e f pos) == (Pack6 n2 g h i j k l _) = (==) n1 n2 && (validatePattern a g) && (validatePattern b h) && (validatePattern c i) && (validatePattern d j) && (validatePattern e k) && (validatePattern f l)
        (Pack7 n1 a b c d e f g pos) == (Pack7 n2 h i j k l m n _) = (==) n1 n2 && (validatePattern a h) && (validatePattern b i) && (validatePattern c j) && (validatePattern d k) && (validatePattern e l) && (validatePattern f m) && (validatePattern g n)
        (Pack8 n1 a b c d e f g h pos) == (Pack8 n2 i j k l m n o p _) = (==) n1 n2 && (validatePattern a i) && (validatePattern b j) && (validatePattern c k) && (validatePattern d l) && (validatePattern e m) && (validatePattern f n) && (validatePattern g o) && (validatePattern h p)
        (Pack9 n1 a b c d e f g h i pos) == (Pack9 n2 j k l m n o p q r _) = (==) n1 n2 && (validatePattern a j) && (validatePattern b k) && (validatePattern c l) && (validatePattern d m) && (validatePattern e n) && (validatePattern f o) && (validatePattern g p) && (validatePattern h q) && (validatePattern i r)
        (Pack10 n1 a b c d e f g h i j pos) == (Pack10 n2 k l m n o p q r s t _) = (==) n1 n2 && (validatePattern a k) && (validatePattern b l) && (validatePattern c m) && (validatePattern d n) && (validatePattern e o) && (validatePattern f p) && (validatePattern g q) && (validatePattern h r) && (validatePattern i s) && (validatePattern j t)
        (CFail _) == (CFail _) = True
        _ == _ = False

    validatePattern :: CExpr -> CExpr -> Bool
    validatePattern c1 c2 = case c1 of
        CVar _ _ -> True 
        _ -> (==) c1 c2

    infixl 0 !
    (!) :: CExpr -> CExpr -> CExpr
    (CLam p f) ! x = appendLineInCExpr (f x) p
    (CFail _) ! x = x

    appendLineInCExpr :: CExpr -> [Pos] -> CExpr
    appendLineInCExpr (CVar str _li)    line = CVar str     (line ++ _li)
    appendLineInCExpr (CApp c1 c2)      line = CApp (appendLineInCExpr c1 line) (appendLineInCExpr c2 line)
    appendLineInCExpr (CBool b _li)     line = CBool b      (line ++ _li)
    appendLineInCExpr (CInt i _li)      line = CInt i       (line ++ _li)
    appendLineInCExpr (CFloat i _li)    line = CFloat i     (line ++ _li)
    appendLineInCExpr (CString s _li)   line = CString s    (line ++ _li)
    appendLineInCExpr (CChar c _li)     line = CChar c      (line ++ _li)
    appendLineInCExpr (Pack0 n pos) line = Pack0 n (line ++ pos)
    appendLineInCExpr (Pack1 n e1 pos) line = Pack1 n e1 (line ++ pos)
    appendLineInCExpr (Pack2 n e1 e2 pos) line = Pack2 n e1 e2 (line ++ pos)
    appendLineInCExpr (Pack3 n e1 e2 e3 pos) line = Pack3 n e1 e2 e3 (line ++ pos)
    appendLineInCExpr (Pack4 n e1 e2 e3 e4 pos) line = Pack4 n e1 e2 e3 e4 (line ++ pos)
    appendLineInCExpr (Pack5 n e1 e2 e3 e4 e5 pos) line = Pack5 n e1 e2 e3 e4 e5 (line ++ pos)
    appendLineInCExpr (Pack6 n e1 e2 e3 e4 e5 e6 pos) line = Pack6 n e1 e2 e3 e4 e5 e6 (line ++ pos)
    appendLineInCExpr (Pack7 n e1 e2 e3 e4 e5 e6 e7 pos) line = Pack7 n e1 e2 e3 e4 e5 e6 e7 (line ++ pos)
    appendLineInCExpr (Pack8 n e1 e2 e3 e4 e5 e6 e7 e8 pos) line = Pack8 n e1 e2 e3 e4 e5 e6 e7 e8 (line ++ pos)
    appendLineInCExpr (Pack9 n e1 e2 e3 e4 e5 e6 e7 e8 e9 pos) line = Pack9 n e1 e2 e3 e4 e5 e6 e7 e8 e9 (line ++ pos)
    appendLineInCExpr (Pack10 n e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 pos) line = Pack10 n e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 (line ++ pos)
    appendLineInCExpr (CFail pos) line = CFail (line ++ pos)
    appendLineInCExpr (CLam pos f) line = CLam (pos ++ line) f

    getPosFromCExpr :: CExpr -> [Pos]
    getPosFromCExpr (CVar _ pos)    = pos
    getPosFromCExpr (CApp e1 e2)    = (getPosFromCExpr e1) ++ (getPosFromCExpr e2) 
    getPosFromCExpr (CBool _ pos)   = pos
    getPosFromCExpr (CInt _ pos)    = pos
    getPosFromCExpr (CString _ pos) = pos
    getPosFromCExpr (CChar _ pos)   = pos
    getPosFromCExpr (CFloat _ pos)  = pos
    getPosFromCExpr (Pack0 _ pos)   = pos
    getPosFromCExpr (Pack1 _ e1 pos) = (getPosFromCExpr e1) ++ pos
    getPosFromCExpr (Pack2 _ e1 e2 pos) = (getPosFromCExpr e1) ++ (getPosFromCExpr e2) ++ pos
    getPosFromCExpr (Pack3 _ e1 e2 e3 pos) = (getPosFromCExpr e1) ++ (getPosFromCExpr e2) ++ (getPosFromCExpr e3) ++ pos
    getPosFromCExpr (Pack4 _ e1 e2 e3 e4 pos) = (getPosFromCExpr e1) ++ (getPosFromCExpr e2) ++ (getPosFromCExpr e3) ++ (getPosFromCExpr e4) ++ pos 
    getPosFromCExpr (Pack5 _ e1 e2 e3 e4 e5 pos) = (getPosFromCExpr e1) ++ (getPosFromCExpr e2) ++ (getPosFromCExpr e3) ++ (getPosFromCExpr e4) ++ (getPosFromCExpr e5) ++ pos
    getPosFromCExpr (Pack6 _ e1 e2 e3 e4 e5 e6 pos) = (getPosFromCExpr e1) ++ (getPosFromCExpr e2) ++ (getPosFromCExpr e3) ++ (getPosFromCExpr e4) ++ (getPosFromCExpr e5) ++ (getPosFromCExpr e6) ++ pos
    getPosFromCExpr (Pack7 _ e1 e2 e3 e4 e5 e6 e7 pos) = (getPosFromCExpr e1) ++ (getPosFromCExpr e2) ++ (getPosFromCExpr e3) ++ (getPosFromCExpr e4) ++ (getPosFromCExpr e5) ++ (getPosFromCExpr e6) ++ (getPosFromCExpr e7) ++ pos
    getPosFromCExpr (Pack8 _ e1 e2 e3 e4 e5 e6 e7 e8 pos) = (getPosFromCExpr e1) ++ (getPosFromCExpr e2) ++ (getPosFromCExpr e3) ++ (getPosFromCExpr e4) ++ (getPosFromCExpr e5) ++ (getPosFromCExpr e6) ++ (getPosFromCExpr e7) ++ (getPosFromCExpr e8) ++ pos
    getPosFromCExpr (Pack9 _ e1 e2 e3 e4 e5 e6 e7 e8 e9 pos) = (getPosFromCExpr e1) ++ (getPosFromCExpr e2) ++ (getPosFromCExpr e3) ++ (getPosFromCExpr e4) ++ (getPosFromCExpr e5) ++ (getPosFromCExpr e6) ++ (getPosFromCExpr e7) ++ (getPosFromCExpr e8) ++ (getPosFromCExpr e9) ++ pos
    getPosFromCExpr (Pack10 _ e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 pos) = (getPosFromCExpr e1) ++ (getPosFromCExpr e2) ++ (getPosFromCExpr e3) ++ (getPosFromCExpr e4) ++ (getPosFromCExpr e5) ++ (getPosFromCExpr e6) ++ (getPosFromCExpr e7) ++ (getPosFromCExpr e8) ++ (getPosFromCExpr e9) ++ (getPosFromCExpr e10) ++ pos
    getPosFromCExpr (CFail pos)               = pos
    getPosFromCExpr _ = []

    cons :: CExpr -> CExpr -> CExpr
    cons c l@(Pack2 n a b pos) = if n == "Cons"
        then Pack2 n c l (getPosFromCExpr c ++ pos)
        else CFail (getPosFromCExpr c ++ pos)
    cons c nil@(Pack0 n pos) = Pack2 "Cons" c nil (getPosFromCExpr c ++ pos)
    cons (CChar c pos) (CString s pos2) = CString (c : s) (pos ++ pos2)

    append :: CExpr -> CExpr -> CExpr
    append (CString a line1) (CString b line2)            = CString (a ++ b) (line1 ++ line2)
    append p1@(Pack0 n1 pos1) p2@(Pack2 n2 c d pos)       = if (n2 == "Cons" && n1 == "Nil")
        then Pack2 n2 c d (pos1 ++ pos)
        else CFail (pos1 ++ pos)
    append p2@(Pack2 n2 c d pos) p1@(Pack0 n1 pos1)       = if (n2 == "Cons" && n1 == "Nil")
        then Pack2 n2 c d (pos1 ++ pos)
        else CFail (pos1 ++ pos)
    append p1@(Pack2 n1 a b pos1) p2@(Pack2 n2 c d pos)   = if (n2 == "Cons" && n1 == "Cons")
        then case b of 
            Pack0 _ _ -> Pack2 n1 a p2 (pos1 ++ pos)
            _ -> Pack2 n1 a (append b p2) (pos1 ++ pos)
        else CFail (pos1 ++ pos)
    append p1@(Pack0 n1 pos1) p2@(Pack0 n2 pos)       = if (n2 == "Nil" && n1 == "Nil")
        then Pack0 n2 (pos1 ++ pos)
        else CFail (pos1 ++ pos)
    append c1 c2 = CFail (getPosFromCExpr c1 ++ getPosFromCExpr c2)

    add :: CExpr -> CExpr -> CExpr
    add (CInt a line1) (CInt b line2) = CInt (a + b) (line1 ++ line2)
    add (CFloat a line1) (CFloat b line2) = CFloat (a + b) (line1 ++ line2)
    add (CInt a line1) (CFloat b line2) = CFloat ((fromInteger a) + b) (line1 ++ line2)
    add (CFloat a line1) (CInt b line2) = CFloat (a + (fromInteger b)) (line1 ++ line2)

    sub :: CExpr -> CExpr -> CExpr
    sub (CInt a line1) (CInt b line2) = CInt (a - b) (line1 ++ line2)
    sub (CFloat a line1) (CFloat b line2) = CFloat (a - b) (line1 ++ line2)
    sub (CInt a line1) (CFloat b line2) = CFloat ((fromInteger a) - b) (line1 ++ line2)
    sub (CFloat a line1) (CInt b line2) = CFloat (a - (fromInteger b)) (line1 ++ line2)

    mul :: CExpr -> CExpr -> CExpr
    mul (CInt a line1) (CInt b line2) = CInt (a * b) (line1 ++ line2)
    mul (CFloat a line1) (CFloat b line2) = CFloat (a * b) (line1 ++ line2)
    mul (CInt a line1) (CFloat b line2) = CFloat ((fromInteger a) * b) (line1 ++ line2)
    mul (CFloat a line1) (CInt b line2) = CFloat (a * (fromInteger b)) (line1 ++ line2)

    division :: CExpr -> CExpr -> CExpr
    division (CInt a line1) (CInt b line2) = CInt (div a b) (line1 ++ line2)
    division (CFloat a line1) (CFloat b line2) = CFloat (a / b) (line1 ++ line2)
    division (CInt a line1) (CFloat b line2) = CFloat ((fromInteger a) / b) (line1 ++ line2)
    division (CFloat a line1) (CInt b line2) = CFloat (a / (fromInteger b)) (line1 ++ line2)    

    pow :: CExpr -> CExpr -> CExpr
    pow (CInt a line1) (CInt b line2) = CInt (a ^ b) (line1 ++ line2)
    pow (CFloat a line1) (CInt b line2) = CFloat (a ^ b) (line1 ++ line2)

    eq :: CExpr -> CExpr -> CExpr  
    eq (CInt a line1) (CInt b line2) = if (==) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    eq (CFloat a line1) (CFloat b line2) = if (==) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    eq (CInt a line1) (CFloat b line2) = if (==) (fromInteger a) b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    eq (CFloat a line1) (CInt b line2) = if (==) a (fromInteger b) then (true (line1 ++ line2)) else (false (line1 ++ line2)) 
    eq (CString a line1) (CString b line2) = if (==) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    eq (CString a line1) (CChar b line2) = if (length a == 1) && (==) a (b:[])
        then (true (line1 ++ line2))
        else (false (line1 ++ line2))
    eq (CChar a line1) (CString b line2) = if (length b == 1) && (==) (a:[]) b
        then (true (line1 ++ line2))
        else (false (line1 ++ line2))
    eq (CString a line1) (CInt b line2) = if (==) a (show b)
        then (true (line1 ++ line2))
        else (false (line1 ++ line2))
    eq (CInt a line1) (CString b line2) = if (==) (show a) b
        then (true (line1 ++ line2))
        else (false (line1 ++ line2))

    eq (CFail line1) (CFail line2) = true (line1 ++ line2)
    eq (CChar a line1) (CChar b line2) = if (==) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    eq p1@(Pack0 _ pos1) p2@(Pack0 _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1@(Pack1 _ _ pos1) p2@(Pack1 _ _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1@(Pack2 _ _ _ pos1) p2@(Pack2 _ _ _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1@(Pack3 _ _ _ _ pos1) p2@(Pack3 _ _ _ _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1@(Pack4 _ _ _ _ _ pos1) p2@(Pack4 _ _ _ _ _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1@(Pack5 _ _ _ _ _ _ pos1) p2@(Pack5 _ _ _ _ _ _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1@(Pack6 _ _ _ _ _ _ _ pos1) p2@(Pack6 _ _ _ _ _ _ _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1@(Pack7 _ _ _ _ _ _ _ _ pos1) p2@(Pack7 _ _ _ _ _ _ _ _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1@(Pack8 _ _ _ _ _ _ _ _ _ pos1) p2@(Pack8 _ _ _ _ _ _ _ _ _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1@(Pack9 _ _ _ _ _ _ _ _ _ _ pos1) p2@(Pack9 _ _ _ _ _ _ _ _ _ _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1@(Pack10 _ _ _ _ _ _ _ _ _ _ _ pos1) p2@(Pack10 _ _ _ _ _ _ _ _ _ _ _ pos2) = if (==) p1 p2 then (true (getPosFromCExpr p1 ++ getPosFromCExpr p2)) else (false (getPosFromCExpr p1 ++ getPosFromCExpr p2))
    eq p1 p2 = false (getPosFromCExpr p1 ++ getPosFromCExpr p2)

    neq :: CExpr -> CExpr -> CExpr
    neq (CInt a line1) (CInt b line2) = if (/=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    neq (CFloat a line1) (CFloat b line2) = if (/=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    neq (CInt a line1) (CFloat b line2) = if (/=) (fromInteger a) b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    neq (CFloat a line1) (CInt b line2) = if (/=) a (fromInteger b) then (true (line1 ++ line2)) else (false (line1 ++ line2)) 
    neq (CString a line1) (CString b line2) = if (/=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    neq (CChar a line1) (CChar b line2) = if (/=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    neq a b = true ((getPosFromCExpr a) ++ (getPosFromCExpr b))

    grt :: CExpr -> CExpr -> CExpr
    grt (CInt a line1) (CInt b line2) = if (>) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    grt (CFloat a line1) (CFloat b line2) = if (>) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    grt (CInt a line1) (CFloat b line2) = if (>) (fromInteger a) b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    grt (CFloat a line1) (CInt b line2) = if (>) a (fromInteger b) then (true (line1 ++ line2)) else (false (line1 ++ line2)) 
    grt (CString a line1) (CString b line2) = if (>) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    grt (CChar a line1) (CChar b line2) = if (>) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    grt a b = CBool True ((getPosFromCExpr a) ++ (getPosFromCExpr b))

    lst :: CExpr -> CExpr -> CExpr
    lst (CInt a line1) (CInt b line2) = if (<) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    lst (CFloat a line1) (CFloat b line2) = if (<) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    lst (CInt a line1) (CFloat b line2) = if (<) (fromInteger a) b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    lst (CFloat a line1) (CInt b line2) = if (<) a (fromInteger b) then (true (line1 ++ line2)) else (false (line1 ++ line2)) 
    lst (CString a line1) (CString b line2) = if (<) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    lst (CChar a line1) (CChar b line2) = if (<) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))

    gre :: CExpr -> CExpr -> CExpr
    gre (CInt a line1) (CInt b line2) = if (>=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    gre (CFloat a line1) (CFloat b line2) = if (>=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    gre (CInt a line1) (CFloat b line2) = if (>=) (fromInteger a) b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    gre (CFloat a line1) (CInt b line2) = if (>=) a (fromInteger b) then (true (line1 ++ line2)) else (false (line1 ++ line2)) 
    gre (CString a line1) (CString b line2) = if (>=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    gre (CChar a line1) (CChar b line2) = if (>=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))

    lse :: CExpr -> CExpr -> CExpr
    lse (CInt a line1) (CInt b line2) = if (<=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    lse (CFloat a line1) (CFloat b line2) = if (<=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    lse (CInt a line1) (CFloat b line2) = if (<=) (fromInteger a) b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    lse (CFloat a line1) (CInt b line2) = if (<=) a (fromInteger b) then (true (line1 ++ line2)) else (false (line1 ++ line2)) 
    lse (CString a line1) (CString b line2) = if (<=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))
    lse (CChar a line1) (CChar b line2) = if (<=) a b then (true (line1 ++ line2)) else (false (line1 ++ line2))

    logical_and :: CExpr -> CExpr -> CExpr
    logical_and (CBool a line1) (CBool b line2) = CBool (a && b) (line1 ++ line2)

    logical_or :: CExpr -> CExpr -> CExpr
    logical_or (CBool a line1) (CBool b line2) = CBool (a || b) (line1 ++ line2)

    true, false :: [Pos] -> CExpr
    true pos = CBool True pos
    false pos = CBool False pos

    pack_0 nm pos = Pack0 nm pos
    pack_1 nm pos = CLam [] $ \a -> (Pack1 nm a pos) 
    pack_2 nm pos = CLam [] $ \a -> CLam [] $ \b -> Pack2 nm a b pos 
    pack_3 nm pos = CLam [] $ \a -> CLam [] $ \b -> CLam [] $ \c -> Pack3 nm a b c pos 
    pack_4 nm pos = CLam [] $ \a -> CLam [] $ \b -> CLam [] $ \c -> CLam [] $ \d -> Pack4 nm a b c d pos 
    pack_5 nm pos = CLam [] $ \a -> CLam [] $ \b -> CLam [] $ \c -> CLam [] $ \d -> CLam [] $ \e -> Pack5 nm a b c d e pos
    pack_6 nm pos = CLam [] $ \a -> CLam [] $ \b -> CLam [] $ \c -> CLam [] $ \d -> CLam [] $ \e -> CLam [] $ \f -> Pack6 nm a b c d e f pos
    pack_7 nm pos = CLam [] $ \a -> CLam [] $ \b -> CLam [] $ \c -> CLam [] $ \d -> CLam [] $ \e -> CLam [] $ \f -> CLam [] $ \g -> Pack7 nm a b c d e f g pos
    pack_8 nm pos = CLam [] $ \a -> CLam [] $ \b -> CLam [] $ \c -> CLam [] $ \d -> CLam [] $ \e -> CLam [] $ \f -> CLam [] $ \g -> CLam [] $ \h -> Pack8 nm a b c d e f g h  pos
    pack_9 nm pos = CLam [] $ \a -> CLam [] $ \b -> CLam [] $ \c -> CLam [] $ \d -> CLam [] $ \e -> CLam [] $ \f -> CLam [] $ \g -> CLam [] $ \h -> CLam [] $ \i -> Pack9 nm a b c d e f g h i pos
    pack_10 nm pos = CLam []  $ \a -> CLam [] $ \b -> CLam [] $ \c -> CLam [] $ \d -> CLam [] $ \e -> CLam [] $ \f -> CLam [] $ \g -> CLam [] $ \h -> CLam [] $ \i -> CLam [] $ \j -> Pack10 nm a b c d e f g h i j pos

    sel_1_1 (Pack1 _ e1 _) = e1
    sel_1_2 (Pack2 _ e1 _ _) = e1
    sel_1_2 (CString (x:xs) pos) = CChar x pos
    sel_2_2 (Pack2 _ _ e2 _) = e2    
    sel_2_2 (CString (x:xs) pos) = CString xs pos
    sel_1_3 (Pack3 _ e1 _ _ _) = e1
    sel_2_3 (Pack3 _ _ e2 _ _) = e2
    sel_3_3 (Pack3 _ _ _ e3 _) = e3    
    sel_1_4 (Pack4 _ e1 _ _ _ _) = e1
    sel_2_4 (Pack4 _ _ e2 _ _ _) = e2
    sel_3_4 (Pack4 _ _ _ e3 _ _) = e3
    sel_4_4 (Pack4 _ _ _ _ e4 _) = e4
    sel_1_5 (Pack5 _ e1 _ _ _ _ _) = e1
    sel_2_5 (Pack5 _ _ e2 _ _ _ _) = e2
    sel_3_5 (Pack5 _ _ _ e3 _ _ _) = e3
    sel_4_5 (Pack5 _ _ _ _ e4 _ _) = e4
    sel_5_5 (Pack5 _ _ _ _ _ e5 _) = e5
    sel_1_6 (Pack6 _ e1 _ _ _ _ _ _) = e1
    sel_2_6 (Pack6 _ _ e2 _ _ _ _ _) = e2
    sel_3_6 (Pack6 _ _ _ e3 _ _ _ _) = e3
    sel_4_6 (Pack6 _ _ _ _ e4 _ _ _) = e4
    sel_5_6 (Pack6 _ _ _ _ _ e5 _ _) = e5
    sel_6_6 (Pack6 _ _ _ _ _ _ e6 _) = e6    
    sel_1_7 (Pack7 _ e1 _ _ _ _ _ _ _) = e1
    sel_2_7 (Pack7 _ _ e2 _ _ _ _ _ _) = e2
    sel_3_7 (Pack7 _ _ _ e3 _ _ _ _ _) = e3
    sel_4_7 (Pack7 _ _ _ _ e4 _ _ _ _) = e4
    sel_5_7 (Pack7 _ _ _ _ _ e5 _ _ _) = e5
    sel_6_7 (Pack7 _ _ _ _ _ _ e6 _ _) = e6
    sel_7_7 (Pack7 _ _ _ _ _ _ _ e7 _) = e7    
    sel_1_8 (Pack8 _ e1 _ _ _ _ _ _ _ _) = e1
    sel_2_8 (Pack8 _ _ e2 _ _ _ _ _ _ _) = e2
    sel_3_8 (Pack8 _ _ _ e3 _ _ _ _ _ _) = e3
    sel_4_8 (Pack8 _ _ _ _ e4 _ _ _ _ _) = e4
    sel_5_8 (Pack8 _ _ _ _ _ e5 _ _ _ _) = e5
    sel_6_8 (Pack8 _ _ _ _ _ _ e6 _ _ _) = e6
    sel_7_8 (Pack8 _ _ _ _ _ _ _ e7 _ _) = e7
    sel_8_8 (Pack8 _ _ _ _ _ _ _ _ e8 _) = e8    
    sel_1_9 (Pack9 _ e1 _ _ _ _ _ _ _ _ _) = e1
    sel_2_9 (Pack9 _ _ e2 _ _ _ _ _ _ _ _) = e2
    sel_3_9 (Pack9 _ _ _ e3 _ _ _ _ _ _ _) = e3
    sel_4_9 (Pack9 _ _ _ _ e4 _ _ _ _ _ _) = e4
    sel_5_9 (Pack9 _ _ _ _ _ e5 _ _ _ _ _) = e5
    sel_6_9 (Pack9 _ _ _ _ _ _ e6 _ _ _ _) = e6
    sel_7_9 (Pack9 _ _ _ _ _ _ _ e7 _ _ _) = e7
    sel_8_9 (Pack9 _ _ _ _ _ _ _ _ e8 _ _) = e8
    sel_9_9 (Pack9 _ _ _ _ _ _ _ _ _ e9 _) = e9    
    sel_1_10 (Pack10 _ e1 _ _ _ _ _ _ _ _ _ _) = e1
    sel_2_10 (Pack10 _ _ e2 _ _ _ _ _ _ _ _ _) = e2
    sel_3_10 (Pack10 _ _ _ e3 _ _ _ _ _ _ _ _) = e3
    sel_4_10 (Pack10 _ _ _ _ e4 _ _ _ _ _ _ _) = e4
    sel_5_10 (Pack10 _ _ _ _ _ e5 _ _ _ _ _ _) = e5
    sel_6_10 (Pack10 _ _ _ _ _ _ e6 _ _ _ _ _) = e6
    sel_7_10 (Pack10 _ _ _ _ _ _ _ e7 _ _ _ _) = e7
    sel_8_10 (Pack10 _ _ _ _ _ _ _ _ e8 _ _ _) = e8
    sel_9_10 (Pack10 _ _ _ _ _ _ _ _ _ e9 _ _) = e9
    sel_10_10 (Pack10 _ _ _ _ _ _ _ _ _ _ e10 _) = e10

    getCExpr :: Integer -> String -> Pos -> CExpr
    getCExpr k nm pos = case k of 
        0 -> pack_0 nm [pos]
        1 -> pack_1 nm [pos]
        2 -> pack_2 nm [pos]
        3 -> pack_3 nm [pos]
        4 -> pack_4 nm [pos]
        5 -> pack_5 nm [pos]
        6 -> pack_6 nm [pos]
        7 -> pack_7 nm [pos]
        8 -> pack_8 nm [pos]
        9 -> pack_9 nm [pos]
        10 -> pack_10 nm [pos]

    modifyPos :: Expr -> Expr
    modifyPos (EApp   e1 e2) = EApp (modifyPos e1) (modifyPos e2)
    modifyPos (EVar   n) = EVar (modifyName n)
    modifyPos (ELam   ps e) = ELam (map modifyPat ps) (modifyPos e)
    modifyPos (ELit   lit) = ELit (modifyLit lit)
    modifyPos (ELetIn pairs e1) =  ELetIn newPats (modifyPos e1)
        where (l1,l2) = unzip pairs
              ps = map modifyPat l1
              es = map modifyPos l2
              newPats = zip ps es
    modifyPos (EFix   e1) = EFix (modifyPos e1)
    modifyPos (EIf    e1 e2 e3) = EIf (modifyPos e1) (modifyPos e2) (modifyPos e3)
    modifyPos (ECase  e1 mats) = ECase (modifyPos e1) (map modifyMatch mats)
    modifyPos (EAnn   e1 t) = EAnn (modifyPos e1) t
    modifyPos (EOp    n e1 e2) = EOp (modifyName n) (modifyPos e1) (modifyPos e2)
    modifyPos (EPack0 s _) = (EPack0 s (0,0))
    modifyPos (EPack1 s e1 _) = (EPack1 s e1 (0,0))
    modifyPos (EPack2 s e1 e2 _) = (EPack2 s e1 e2 (0,0))
    modifyPos (EPack3 s e1 e2 e3 _) = (EPack3 s e1 e2 e3 (0,0))
    modifyPos (EPack4 s e1 e2 e3 e4 _) = (EPack4 s e1 e2 e3 e4 (0,0))
    modifyPos (EPack5 s e1 e2 e3 e4 e5 _) = (EPack5 s e1 e2 e3 e4 e5 (0,0))
    modifyPos (EPack6 s e1 e2 e3 e4 e5 e6 _) = (EPack6 s e1 e2 e3 e4 e5 e6 (0,0))
    modifyPos (EPack7 s e1 e2 e3 e4 e5 e6 e7 _) = (EPack7 s e1 e2 e3 e4 e5 e6 e7 (0,0))
    modifyPos (EPack8 s e1 e2 e3 e4 e5 e6 e7 e8 _) = (EPack8 s e1 e2 e3 e4 e5 e6 e7 e8 (0,0))
    modifyPos (EPack9 s e1 e2 e3 e4 e5 e6 e7 e8 e9 _) = (EPack9 s e1 e2 e3 e4 e5 e6 e7 e8 e9 (0,0))
    modifyPos (EPack10 s e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 _) = (EPack10 s e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 (0,0))
    modifyPos e = e

    modifyName (Gen s k _) = Gen s k (0,0)
    modifyName (Name s _) = Name s (0,0)
    modifyName (Qualified (s1, s2) _) = Qualified (s1, s2) (0,0)
    modifyName (Operator op _) = Operator op (0,0)

    modifyLit (LitInt k _) = LitInt k (0,0)
    modifyLit (LitFloat f _) = LitFloat f (0,0)
    modifyLit (LitChar c _) = LitChar c (0,0)
    modifyLit (LitString s _) = LitString s (0,0)
    modifyLit (LitBool b _) = LitBool b (0,0)

    modifyPat (PVar n) = PVar (modifyName n)
    modifyPat (PCon c ps) = PCon (modifyName c) (map modifyPat ps)
    modifyPat (PLit lit) = PLit (modifyLit lit)
    modifyPat (PWild _) = PWild (0,0)

    modifyMatch (Match ps body g _) = Match (map modifyPat ps) (modifyPos body) (map modifyPos g) (0,0)
