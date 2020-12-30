{
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Parser where

import Lexer
import Syntax
import Name

import SrcLoc
import DynFlags
import FastString
import BasicTypes
import Control.Monad.State
import Debug.Trace

import StringBuffer (stringToStringBuffer)

import qualified Data.Map as M
import qualified Data.Maybe as Maybe

}

%token
 '_'            { L _ ITunderscore }            -- Haskell keywords
 'as'           { L _ ITas }
 'case'         { L _ ITcase }
 'class'        { L _ ITclass }
 'data'         { L _ ITdata }
 'deriving'     { L _ ITderiving }
 'do'           { L _ ITdo }
 'else'         { L _ ITelse }
 'hiding'       { L _ IThiding }
 'if'           { L _ ITif }
 'import'       { L _ ITimport }
 'in'           { L _ ITin }
 'infix'        { L _ ITinfix }
 'infixl'       { L _ ITinfixl }
 'infixr'       { L _ ITinfixr }
 'instance'     { L _ ITinstance }
 'let'          { L _ ITlet }
 'module'       { L _ ITmodule }
 'newtype'      { L _ ITnewtype }
 'of'           { L _ ITof }
 'qualified'    { L _ ITqualified }
 'then'         { L _ ITthen }
 'type'         { L _ ITtype }
 'where'        { L _ ITwhere }

 '..'           { L _ ITdotdot }                        -- reserved symbols
 ':'            { L _ ITcolon }
 '::'           { L _ ITdcolon }
 '='            { L _ ITequal }
 '\\'           { L _ ITlam }
 '|'            { L _ ITvbar }
 '<-'           { L _ ITlarrow }
 '->'           { L _ ITrarrow }
 '@'            { L _ ITat }
 '~'            { L _ ITtilde }
 '=>'           { L _ ITdarrow }
 '!'            { L _ ITbang }
 '.'            { L _ ITdot }

 '{'            { L _ ITocurly }                        -- special symbols
 '}'            { L _ ITccurly }
 vocurly        { L _ ITvocurly } -- virtual open curly (from layout)
 vccurly        { L _ ITvccurly } -- virtual close curly (from layout)
 '['            { L _ ITobrack }
 ']'            { L _ ITcbrack }
 '('            { L _ IToparen }
 ')'            { L _ ITcparen }
 ';'            { L _ ITsemi }
 ','            { L _ ITcomma }
 '`'            { L _ ITbackquote }
 '+'            { L _ ITplus    }
 '-'            { L _ ITminus   }
 '*'            { L _ ITmult    }
 '/'            { L _ ITdiv     }
 '>'            { L _ ITgreater }
 '<'            { L _ ITless    }
 '/='           { L _ ITdiff    }
 '=='           { L _ ITeqcomp  }
 '>='           { L _ ITgt      }
 '<='           { L _ ITlt      }
 '||'           { L _ ITor      }
 '&&'           { L _ ITand     } 
 '++'           { L _ ITappend  } 
 '^'            { L _ ITpow     } 

 VARID          { L _ (ITvarid    _) }          -- identifiers
 CONID          { L _ (ITconid    _) }
 VARSYM         { L _ (ITvarsym   _) }
 CONSYM         { L _ (ITconsym   _) }
 QVARID         { L _ (ITqvarid   _) }
 QCONID         { L _ (ITqconid   _) }
 QVARSYM        { L _ (ITqvarsym  _) }
 QCONSYM        { L _ (ITqconsym  _) }

 CHAR           { L _ (ITchar   _ _) }
 STRING         { L _ (ITstring _ _) }
 INTEGER        { L _ (ITinteger _) }
 RATIONAL       { L _ (ITrational _) }

%monad { P } { >>= } { return }
%lexer { (lexer True) } { L _ ITeof }
%tokentype { (Located Token) }

-- Exported parsers
%name parseModule Module
%name parseDeclaration Topdecls
%name parseExpression Exp
%%

Module : 'module' Modid 'where' '{' Body '}'                 { $5 }
       | 'module' Modid 'where' vocurly Body vccurly         { $5 }
       | 'module' Modid '(' Exports ')' 'where' '{' Body '}' { $8 }
       | 'module' Modid '(' Exports ')' 'where' vocurly Body vccurly { $8 }
       | Body { $1 }

Exports : Export                              { [] }
        | Export ',' Exports                  { [] }
        | {- empty -}                         { [] }

Export : Qvar                                 { [] }
       | Modid '(' '..' ')'                   { [] }
       | Modid '(' Cnames ')'                 { [] }
       | Modid '(' ')'                        { [] }
       | Modid                                { [] }
       | Modid '(' Qvars ')'                  { [] }
       | 'module' Modid                       { [] }

Cnames : Cname                                { [] }
       | Cnames ',' Cname                     { [] }

Qvars : Qvar                                  { [] }
      | Qvars ',' Qvar                        { [] }    

Cname : Var                                  { [] }
      | Con                                  { [] }   

Body : Impdecls ';' Topdecls         { reverse $3 }      
     | Topdecls                      { reverse $1 }      

Impdecls : Impdecl                            { [] }
         | Impdecls ';' Impdecl               { [] }

Impdecl : 'import' 'qualified' Modid 'as' Modid Impspec   { []}
        | 'import' 'qualified' Modid 'as' Modid           { [] }
        | 'import' 'qualified' Modid Impspec              { [] }
        | 'import' Modid 'as' Modid Impspec               { [] }
        | 'import' Modid 'as' Modid                       { [] }
        | 'import' 'qualified' Modid                      { [] }
        | 'import' Modid Impspec                          { [] }
        | 'import' Modid                                  { [] }

Impspec : '(' Imports ')'                                 { [] }
        | 'hiding' '(' Imports ')'                        { [] }

Imports : Import                                          { [] }
        | Import ',' Imports                              { [] }
        | {- empty -}                                     { [] }

Import : Var                                 { [] }
       | CONID                               { [] }
       | CONID '(' '..' ')'                  { [] }
       | CONID '(' Cnames ')'                { [] }
       | CONID '(' ')'                       { [] }                  

Topdecls : Topdecl                           { [ $1 ] }
         | Topdecls ';' Topdecl              { ($3 : $1) }

Topdecl : -- 'type' Simpletype '=' Type                               { buildDataDecl $2 [ConDecl (Name "" (mkPos $1)) [$4]] (mkPos $3)}
         'data' Simpletype '=' Constrs Deriving                    { buildDataDecl $2 (reverse $4) (mkPos $3) }
        | 'data' Simpletype '=' Constrs                            { buildDataDecl $2 (reverse $4) (mkPos $3) }
        | 'newtype' Simpletype '=' Newconstr Deriving              { buildDataDecl $2 [$4] (mkPos $3) }
        | 'newtype' Simpletype '=' Newconstr                       { buildDataDecl $2 [$4] (mkPos $3) }
        | Decl                                                     { $1 }

Decl : Gendecl                        { TypeDecl $1 }
     | Funlhs Rhs                     { FunDecl (bindGroupBuilder $1 $2) }
     | Pat Rhs                        { FunDecl (bindGroupBuilder (Name "" (0,0) ,[$1]) $2) }

Gendecl : Vars '::' MaybeCtx          { TForall (reverse $1) $3 }

MaybeCtx : '(' Classes ')' '=>' Type { $5 }
         | Modid VARID '=>' Type     { $4 }
         | Type                      { $1 }
         | Modid '(' Ambig           { $3 }

Ambig : VARID Btype ')' '=>' Type     { $5 }
      | ')' '->' Type                 { $3 }
      | ')' Btype                     { $2 }
      | ')'                           { tyUnit }
      | '->' ')' '->' Type            { $4 }
      | '->' ')' Btype                { $3 }
      | '->' ')'                      { tyArrow }
      | Types ')' '->' Type           { $4 }
      | Types ')' Btype               { $3 }
      | Types ')'                     { mkTTuple $1 }
      | Commas ')' '->' Type          { $4 }
      | Commas ')' Btype              { $3 }
      | Commas ')'                    { tyTuple }

Vars : Var                            { [ TV $1 ] }
     | Vars ',' Var                   { ((TV $3) : $1) }

Type : Btype                          { $1 }
     | Btype '->' Type                { TArr $1 $3 }

Btype : Atype                         { $1 }
      | Btype Atype                   { TApp $1 $2 }

Atype : Gtycon                        { $1 }
      | VARID                         { TVar (TV (Name (getVARID $1) (mkPos $1) ) ) }
      | '(' Type ')'                  { $2 }
      | '(' Type ',' Types ')'        { mkTTuple ($2 : $4) }
      | '[' Type ']'                  { mkTList $2 }

Gtycon : Modid                        { TCon (AlgTyCon $1) }
       | '(' ')'                      { tyUnit }
       | '[' ']'                      { tyList }
       | '(' '->' ')'                 { tyArrow }
       | '(' Commas ')'               { tyTuple }

Commas : ','                          { [","] }
       | Commas ','                   { "," : $1 }

Classes : Class                       { [] }
        | Classes ',' Class           { [] }

Class : Modid ClassAux                { [] }

ClassAux : VARID                      { [] }
         | '(' VARID Btype ')'        { [] }

Simpletype : CONID Tyvarsno           { (Name (getCONID $1) (mkPos $1), reverse $2) }

Modid : QCONID                        { Qualified (getQCONID $1) (mkPos $1) }
      | CONID                         { Name (getCONID $1) (mkPos $1) }

Tyvarsno : Tyvarsno VARID             { ((Name (getVARID $2) (mkPos $2)):$1) }
         | {- empty -}                { [] }

Constrs : Constr                      { [ $1 ] }
        | Constrs '|' Constr          { ($3 : $1) }

Constr : Con Constrrec                { ConDecl (nameToGen $1 (fromIntegral (length $2))) $2 }
       | Con '{' Fielddecls '}'       { ConDecl (nameToGen $1 (fromIntegral (length $3))) $3 }

Fielddecls : Fielddecl                { [ $1 ] }
           | Fielddecls ',' Fielddecl { reverse ($3 : $1) }

Constrrec : Atype Constrrec           { ($1 : $2) }
          | {- empty -}               { [] }

Newconstr : Con Atype                 { ConDecl (nameToGen $1 1) [$2] }
          | Con '{' Var '::' Type '}' { ConDecl (nameToGen $1 1) [TForall [TV $3] $5] }

Fielddecl : Vars '::' Type            { TForall (reverse $1) $3 }

Deriving : 'deriving' Modid           { [] }
         | 'deriving' '(' Dclasses ')'{ [] }

Dclasses : Modid                      { [] }
         | Dclasses ',' Modid         { [] }

Funlhs : Var Apats                     { ($1, reverse $2) }
       | Pat Varop Pat                 { ($2, ($1:[$3])) }

Apats : Apat                           { [ $1 ] }
      | Apats Apat                     { ($2 : $1 ) }

Rhs : '=' Exp                                  { ([ Match [] $2 [] (mkPos $1) ], mkPos $1 ) }
    | '=' Exp 'where' '{' RecLet '}'           { ([ Match [] (ELetIn (buildLet $5) $2) [] (mkPos $1) ], mkPos $1 ) }
    | '=' Exp 'where' vocurly RecLet vccurly   { ([ Match [] (ELetIn (buildLet $5) $2) [] (mkPos $1) ], mkPos $1 ) }
    | Gdrhs                                    { ($1, (0,0)) }
    | Gdrhs 'where' '{' RecLet '}'             { ((insertExpInMatchBody $4 $1), (0,0)) }
    | Gdrhs 'where' vocurly RecLet vccurly     { ((insertExpInMatchBody $4 $1), (0,0)) }

Gdrhs : Guards '=' Exp                 { [ Match [] $3 $1 (mkPos $2) ] }
      | Guards '=' Exp Gdrhs           { (Match [] $3 $1 (mkPos $2)) : $4 }

Guards : '|' Guardsaux                 { reverse $2 }

Guardsaux : Guard                      { [ $1 ] }
          | Guardsaux ',' Guard        { ($3 : $1) }

Guard : Infixexp                       { $1 }

Exp : Infixexp '::' Type               { EAnn $1 $3 }
    | Infixexp                         { $1 }

Infixexp : Lexp Qop Infixexp           { EOp $2 $1 $3 }
         | Lexp                        { $1 }

Lexp : '\\' Apats '->' Exp                            { ELam (reverse $2) $4 }
     | 'let' '{' RecLet '}' 'in' Exp                  { ELetIn (buildLet $3) $6 }
     | 'let' vocurly RecLet vccurly 'in' Exp          { ELetIn (buildLet $3) $6 }
     | 'if' Exp ';' 'then' Exp ';' 'else' Exp         { EIf $2 $5 $8 }
     | 'if' Exp 'then' Exp ';' 'else' Exp             { EIf $2 $4 $7 }
     | 'if' Exp ';' 'then' Exp 'else' Exp             { EIf $2 $5 $7 }
     | 'if' Exp 'then' Exp 'else' Exp                 { EIf $2 $4 $6 }
     | 'case' Exp 'of' '{' Alts '}'                   { ECase $2 $5 }
     | 'case' Exp 'of' vocurly Alts vccurly           { ECase $2 $5 }
     | 'do' '{' Stmts '}'                             { EDo $3 }
     | 'do' vocurly Stmts vccurly                     { EDo $3 }
     | Fexp                                           { $1 }

RecLet : Apat '=' Exp ';' RecLet        { (($1,$3):$5) }
       | Apat '=' Exp                   { [($1, $3)] }

Fexp : Aexp                             { $1 }
     | Fexp Aexp                        { EApp $1 $2 }

Aexp : Qvar                             { EVar $1 }
     | Gcon                             { EVar (nameToGen $1 0) }
     | Literal                          { ELit $1 }
     | '(' Exp ')'                      { $2 }
     | '(' Exp Exps ')'                 { mkTuple (toInteger (length ($2: $3))) ($2 : (reverse $3)) }
     | '[' Exp Exps ']'                 { mkList ($2 : (reverse $3)) }
     | '(' Infixexp Qop ')'             { EApp $2 (EVar $3) }
     | '(' Qop Infixexp ')'             { EApp (EVar $2) $3 }

Exps : {- empty -}                      { [] }
     | Exps ',' Exp                     { ($3 : $1) }

Literal : INTEGER                       { LitInt (fromInteger (il_value (getINTEGER $1))) (mkPos $1) }
        | RATIONAL                      { LitFloat (fromRational (fl_value (getRATIONAL $1))) (mkPos $1) }
        | CHAR                          { LitChar (getCHAR $1) (mkPos $1) }
        | STRING                        { LitString (getSTRING $1) (mkPos $1) }

Types : Type                            { [ $1 ] }
      | Types ',' Type                  { reverse ($3 : $1) }

Alts : Alt                              { $1 }
     | Alts ';' Alt                     { $1 ++ $3 }

Alt : Pat '->' Exp                                  { [Match [$1] $3 [] (mkPos $2)] }
    | Pat '->' Exp 'where' '{' RecLet '}'           { [Match [$1] (ELetIn (buildLet $6) $3) [] (mkPos $2)] }
    | Pat '->' Exp 'where' vocurly RecLet vccurly   { [Match [$1] (ELetIn (buildLet $6) $3) [] (mkPos $2)] }
    | Pat Gdpat                                     { buildAlt [$1] $2 }
    | Pat Gdpat 'where' '{' RecLet '}'              { buildAltWhere [$1] $2 $5 }
    | Pat Gdpat 'where' vocurly RecLet vccurly      { buildAltWhere [$1] $2 $5 }

Gdpat : Guards '->' Exp                  { [ ($1,$3,(mkPos $2)) ] }
      | Gdpat Guards '->' Exp            { reverse (($2,$4,(mkPos $3)):$1) }

Stmts : StmtsAux Exp ';'                 { reverse ((Qualifier $2) : $1) }
      | StmtsAux Exp                     { reverse ((Qualifier $2) : $1) }

StmtsAux : StmtsAux Stmt                 { reverse ($2 : $1) }
         | {- empty -}                   { [] }

Stmt : Exp ';'                                { Qualifier $1 }
     | Pat '<-' Exp ';'                       { Generator $1 $3 }
     | 'let' '{' Apat '=' Exp '}' ';'         { Qualifier (ELet $3 $5) }
     | 'let' vocurly Apat '=' Exp vccurly ';' { Qualifier (ELet $3 $5 ) }

Pat : Lpat                               { $1 }
    | Pat Qconop Lpat                    { PCon (nameToGen $2 (fromIntegral (length ($1 : [$3])))) ($1 : [$3]) }

Lpat : Gcon Apats                        { PCon (nameToGen $1 (fromIntegral (length $2))) (reverse $2) }
     | Apat                              { $1 }

Apat : Var '@' Apat                      { PCon (nameToGen $1 0) [$3] }
     | Var                               { PVar $1 }
     | '(' Pat ')'                       { $2 }
     | '(' Pat ',' Pats ')'              { PCon (makeName (toInteger (length ($2: $4))) (mkPos $1)) ($2: (reverse $4)) }
     | Gcon                              { PCon (nameToGen $1 0) []}
     | Literal                           { PLit $1 }
     | '_'                               { PWild (mkPos $1) }
     | '[' Pats ']'                      { PCon (Gen "Cons" 2 (mkPos $1)) ((reverse $2) ++ [PCon (Gen "Nil" 0 (mkPos $3)) []]) }

Pats : Pat                               { [ $1 ] }
     | Pats ',' Pat                      { ($3 : $1) }

Gcon : '(' ')'                           { Name "Unit" (mkPos $1) }
     | '[' ']'                           { Gen "Nil" 0 (mkPos $1) }
     | '(' Commas ')'                    { makeName (toInteger (length $2)) (mkPos $1) }
     | Qcon                              { $1 }

Var : VARID                              { Name (getVARID $1) (mkPos $1) }
    | '(' VARSYM ')'                     { Name ("(" ++ (getVARSYM $2) ++ ")") (mkPos $2) }

Qvar : QvaridAux                         { $1 }
     | '(' QvarsymAux ')'                { $2 }

QvaridAux : QVARID                       { Qualified (getQVARID $1) (mkPos $1) }
          | VARID                        { Name (getVARID $1) (mkPos $1) }

QvarsymAux : QVARSYM                     { Qualified (getQVARSYM $1) (mkPos $1) }
           | VARSYM                      { Name (getVARSYM $1) (mkPos $1) }

Con : CONID                              { Name (getCONID $1) (mkPos $1) }
    | '(' CONSYM ')'                     { Name (getCONSYM $2) (mkPos $2) }

Qcon : Modid                             { $1 }
     | '(' Gconsym ')'                   { $2 }

Varop : VARSYM                           { Name (getVARSYM $1) (mkPos $1) }
      | '`' VARID '`'                    { Name (getVARID $2) (mkPos $2) }

Qvarop : QvarsymAux                      { $1 }
       | '`' QvaridAux '`'               { $2 }

Qconop : Gconsym                         { $1 }
       | '`' Modid '`'                   { $2 }

Qop : Qvarop                             { $1 }
    | Qconop                             { $1 }
    | '-'                                { Operator Sub (mkPos $1) }
    | '+'                                { Operator Add (mkPos $1) }
    | '*'                                { Operator Mul (mkPos $1) }
    | '/'                                { Operator Div (mkPos $1) }
    | '>'                                { Operator Gt (mkPos $1) }
    | '<'                                { Operator Lt (mkPos $1) }
    | '/='                               { Operator Neq (mkPos $1) }
    | '=='                               { Operator Eq (mkPos $1) }
    | '>='                               { Operator Ge (mkPos $1) }
    | '<='                               { Operator Le (mkPos $1) }
    | '&&'                               { Operator And (mkPos $1) }
    | '||'                               { Operator Or (mkPos $1) }
    | '++'                               { Operator App (mkPos $1) }
    | '^'                                { Operator Pow (mkPos $1) }

Gconsym : ':'                            { Gen "Cons" 2 (mkPos $1) }
        | QCONSYM                        { Qualified (getQCONSYM $1) (mkPos $1) }
        | CONSYM                         { Name (getCONSYM $1) (mkPos $1) }

{

happyError :: P a
happyError = srcParseFail

getVARID        (dL->L _ (ITvarid    x)) = x
getCONID        (dL->L _ (ITconid    x)) = x
getVARSYM       (dL->L _ (ITvarsym   x)) = x
getCONSYM       (dL->L _ (ITconsym   x)) = x
getQVARID       (dL->L _ (ITqvarid   x)) = x
getQCONID       (dL->L _ (ITqconid   x)) = x
getQVARSYM      (dL->L _ (ITqvarsym  x)) = x
getQCONSYM      (dL->L _ (ITqconsym  x)) = x
getCHAR         (dL->L _ (ITchar   _ x)) = x
getSTRING       (dL->L _ (ITstring _ x)) = x
getINTEGER      (dL->L _ (ITinteger x))  = x
getRATIONAL     (dL->L _ (ITrational x)) = x

getSrcSpanPos (dL-> L pos _) = pos

getPos :: SrcSpan -> Pos 
getPos src = (_line, _col)
     where _line = getLineNumber src
           _col = getCol src

mkPos tok = getPos $ getSrcSpanPos tok 

makeName i pos = Gen ("Tuple-" ++ (show i)) i pos

insertExpInMatchBody _ [] = []
insertExpInMatchBody e ((Match ps body gs pos):xs) = (Match ps (ELetIn (buildLet e) body) (map (insertLetinGuards e) gs) pos) : insertExpInMatchBody e xs

insertLetinGuards :: [(Pattern,Expr)] -> Expr -> Expr
insertLetinGuards ex guard = if guard == (EVar (Name "otherwise" (0,0)))
  then guard
  else (ELetIn (buildLet ex) guard)

parseThing :: Lexer.P thing -> DynFlags -> String -> Lexer.ParseResult thing
parseThing parser dflags stmt = do
  let buf = stringToStringBuffer stmt
      loc = mkRealSrcLoc (FastString.fsLit "<interactive>")  1 1

  Lexer.unP parser (Lexer.mkPState buf loc)

parse :: String -> [Decl]
parse str = case (parseThing parseModule DynFlags.unsafeGlobalDynFlags str) of
  Lexer.POk _ a -> a
  Lexer.PFailed _ _ -> []

parseExp :: String -> Expr
parseExp str = case (parseThing parseExpression DynFlags.unsafeGlobalDynFlags str) of
  Lexer.POk _ a -> a
  Lexer.PFailed _ _ -> EFail

testParser fileName = do 
     inStr <- readFile fileName
     print $ parse inStr

bindGroupBuilder :: (Name,[Pattern]) -> ([Match], Pos) -> BindGroup
bindGroupBuilder ((Name name p),pats) (matches,line) = 
     if null name 
          then let newMatches = insertPatternInMatchList (tail pats) matches
                    in BindGroup {_matchName=(extractNameFromPat (head pats)),_matchPats=newMatches,_matchType=Nothing,_line=line}
          else do BindGroup {_matchName=(Name name p),_matchPats=newMatches,_matchType=Nothing,_line=line}
                    where newMatches = insertPatternInMatchList pats matches

extractNameFromPat :: Pattern -> Name 
extractNameFromPat (PVar n) = n

insertPatternInMatchList :: [Pattern] -> [Match] -> [Match]
insertPatternInMatchList _ [] = []
insertPatternInMatchList pats (Match {_matchBody=body,_matchGuard=guards,_codeLine=line}:xs) = 
     Match {_matchBody=body,_matchGuard=guards,_codeLine=line,_matchPat=pats} : insertPatternInMatchList pats xs

buildDataDecl :: (Name, [Name]) -> [ConDecl] -> Pos -> Decl 
buildDataDecl (name, names) cons line = DataDecl name names cons line

buildAlt :: [Pattern] -> [([Expr],Expr,Pos)] -> [Match]
buildAlt _ [] = []
buildAlt pat ((guards,body,pos):xs) = (Match pat body guards pos) : buildAlt pat xs 

buildAltWhere _ [] wh = []
buildAltWhere pat ((guards,body,pos):xs) wh = (Match pat (ELetIn wh body) guards pos) : buildAlt pat xs 

buildLet :: [(Pattern,Expr)] -> [(Pattern,Expr)]
buildLet [] = [] 
buildLet ((p, e):ps) = case p of 
  PCon (Gen n k p) pats -> buildLet2 pats k 1 e ++ buildLet ps
  _ -> (p,e) : buildLet ps

buildLet2 :: [Pattern] -> Integer -> Int -> Expr -> [(Pattern,Expr)]
buildLet2 [] _ _ _ = [] 
buildLet2 (p:ps) n i e = (p, EApp (EVar (Name ("$SEL_" ++ (show i) ++ "_" ++ (show n) ) (0,0))) e) : (buildLet2 ps n (i+1) e)

nameToGen :: Name -> Integer -> Name
nameToGen (Name n pos) k = Gen n k pos 
nameToGen n k = n

}
