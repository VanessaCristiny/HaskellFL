{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax 
(
  Expr(..),
  Decl(..),
  Match(..),
  BindGroup(..),
  Pattern(..),
  ConDecl(..),
  Stmt(..),
  Literal(..),
  Constr,
  mkEApp,
  mkTuple,
  mkList,
  fgroup,
  getConstrMap,
  groupDecls
) 
where

import Prelude hiding (foldr, foldr1, concatMap)

import Name

import Data.Foldable
import Data.Function (on)
import Data.List (groupBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Constr = Name

data Expr
  = EApp   Expr Expr        -- ^ a b
  | EVar   Name             -- ^ x
  | ELam   [Pattern] Expr   -- ^ \\x . y
  | ELit   Literal          -- ^ 1, 'a'
  | ELetIn [(Pattern, Expr)] Expr -- ^ let x = y in x
  | ELet   Pattern Expr
  | EFix   Expr
  | EIf    Expr Expr Expr   -- ^ if x then tr else fl
  | ECase  Expr [Match]     -- ^ case x of { p -> e; ... }
  | EAnn   Expr Type        -- ^ ( x :: Int )
  | EDo    [Stmt]           -- ^ do { ... }
  | EOp    Name Expr Expr
  | EFail
  | EPack0 String Pos
  | EPack1 String Expr Pos
  | EPack2 String Expr Expr Pos
  | EPack3 String Expr Expr Expr Pos
  | EPack4 String Expr Expr Expr Expr Pos
  | EPack5 String Expr Expr Expr Expr Expr Pos
  | EPack6 String Expr Expr Expr Expr Expr Expr Pos
  | EPack7 String Expr Expr Expr Expr Expr Expr Expr Pos
  | EPack8 String Expr Expr Expr Expr Expr Expr Expr Expr Pos
  | EPack9 String Expr Expr Expr Expr Expr Expr Expr Expr Expr Pos
  | EPack10 String Expr Expr Expr Expr Expr Expr Expr Expr Expr Expr Pos
  | EFatBar Expr Expr 
  deriving (Eq, Show)

data Stmt
  = Generator Pattern Expr -- ^ pat <- exp
  | Qualifier Expr         -- ^ exp
  deriving (Eq, Show)

data Pattern
  = PVar Name              -- ^ x
  | PCon Constr [Pattern]  -- ^ C x y
  | PLit Literal           -- ^ 3
  | PWild Pos              -- ^ _
  deriving (Eq, Show)

data BindGroup = BindGroup
  { _matchName  :: Name
  , _matchPats  :: [Match]
  , _matchType  :: Maybe Type
  , _line       :: Pos
  } deriving (Eq, Show)

data Match = Match
  { _matchPat :: [Pattern]
  , _matchBody :: Expr
  , _matchGuard :: [Expr]
  , _codeLine    :: Pos
  } deriving (Eq, Show)

data Literal
  = LitInt Integer Pos     -- ^ 1
  | LitFloat Float Pos     -- ^ 2.5
  | LitChar Char Pos       -- ^ 'a'
  | LitString String Pos   -- A primitive C-style string, type Addr#
  | LitBool Bool Pos
  deriving (Eq, Ord, Show)

data ConDecl
  = ConDecl Constr [Type]              -- ^ T :: a -> T a
  | UnitDecl Constr                    -- ^ data Bool = True | False 
  | RecDecl Constr [(Name, Type)] Type -- ^ T :: { label :: a } -> T a
  deriving (Eq, Show, Ord)

data Decl
  = FunDecl BindGroup                    -- ^ f x = x + 1
  | TypeDecl Type                        -- ^ f :: Int -> Int
  | DataDecl Constr [Name] [ConDecl] Pos -- ^ data T a = { ... }
  deriving (Eq, Show)

fgroup :: Decl -> [BindGroup]
fgroup (FunDecl xs) = [xs]
fgroup _ = []

getConstrMap :: [Decl] -> Map.Map String [Name] -> Map.Map String [Name]
getConstrMap [] mapa = mapa
getConstrMap ((DataDecl _ _ xs _):ds) mapa = 
  let   names = (map getConDeclName xs)
        k = (length names) - 1
        newMap = buildDataMapInstance names k mapa 
      in getConstrMap ds newMap

getConDeclName :: ConDecl -> Name
getConDeclName (ConDecl name _) = name

buildDataMapInstance :: [Name] -> Int -> Map.Map String [Name] -> Map.Map String [Name]
buildDataMapInstance names k mapa = if k >= 0
  then let (Gen n i p) = (names !! k)
           newMap = Map.insert n names mapa 
         in buildDataMapInstance names (k-1) newMap
  else mapa

groupBindings :: [BindGroup] -> [BindGroup]
groupBindings = fmap joinBindings . groupBy ((==) `on` _matchName)

joinBindings :: [BindGroup] -> BindGroup
joinBindings xs@(x:_) =
  BindGroup (_matchName x) (concatMap _matchPats xs) (_matchType x) (_line x)
joinBindings [] = error "empty binding group"

groupDecls :: [Decl] -> [Decl]
groupDecls decls = fmap FunDecl $ groupBindings (concatMap fgroup decls)

_tuplecon :: Integer -> Name 
_tuplecon i = Gen ("Tuple-" ++ (show i)) i (0,0)
_conscon, _nilcon :: Name
_conscon = Gen "Cons" 2 (0,0)
_nilcon  = Gen "Nil" 0 (0,0)

mkEApp :: Expr -> [Expr] -> Expr
mkEApp = foldl' EApp

mkList :: [Expr] -> Expr
mkList = foldr cons nil
  where
    cons x y = mkEApp (EVar _conscon) [x,y]
    nil      = EVar _nilcon

mkTuple :: Integer -> [Expr] -> Expr
mkTuple i es = foldl EApp (EVar (_tuplecon i)) es
