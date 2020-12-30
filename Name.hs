{-# LANGUAGE OverloadedStrings #-}

module Name (
  Name(..),
  Op(..),
  Pos,
  Type(..),
  TVar(..),
  TyCon(..),
  unName,
  prefix,
  letters,
  mkTTuple,
  mkTList,
  tyArrow,
  tyList,
  tyUnit,
  tyTuple,
  listTyCon,
  tupleTyCon,
  unitTyCon
) where

import Data.String
import Control.Monad
import Data.List (foldl')

type Pos = (Int,Int) -- (line,column)

data Name
  = Gen String Integer Pos
  | Name String Pos
  | Qualified (String, String) Pos
  | Operator Op Pos
  deriving (Ord, Show, Read)

instance Eq Name where
  (Name str1 _)              == (Name str2 _)               = str1  == str2
  (Gen s1 k1 _)              == (Gen s2 k2  _)              = (s1 == s2) && (k1 == k2)
  (Operator op1 _)           == (Operator op2 _)            = op1   == op2
  (Qualified (s1_1,s1_2) _)  == (Qualified (s2_1,s2_2) _)   = (s1_1 == s2_1) && (s1_2 == s2_2)
  _ == _ = False

data Op
  = Add | Sub | Mul | Div 
  | Gt | Lt | Neq | Eq | App 
  | Ge | Le | And | Or | Pow
  deriving (Eq, Ord, Show, Read)

data Type
  = TVar TVar
  | TCon TyCon
  | TApp Type Type
  | TArr Type Type
  | TForall [TVar] Type
  deriving (Show, Eq, Ord)

data TyCon
  = AlgTyCon { tyId :: Name }
  deriving (Show, Eq, Ord)

data TVar = TV
  { tvName   :: Name
  } deriving (Show, Eq, Ord)

instance IsString Name where
  fromString s = Name s (0,0)

instance IsString TVar where
  fromString x = TV (fromString x)

instance IsString TyCon where
  fromString = AlgTyCon . fromString

prefix :: String -> Name -> Name
prefix p (Name nm pos) = Name (p <> nm) pos
prefix p (Qualified (nm1,nm2) pos) = Qualified (p <> nm1, p <> nm2) pos
prefix p (Gen nm i pos) = Gen (p <> nm) i pos

unName :: IsString a => Name -> a
unName (Name s l) = fromString s
unName (Qualified (s1,s2) pos) = fromString (s1 ++ "." ++ s2)
unName (Gen s n p) = fromString (s ++ show n)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

mkTTuple :: [Type] -> Type
mkTTuple args
  = foldl' TApp (TCon (AlgTyCon "Tuple")) args

mkTList :: Type -> Type
mkTList tp
  = TApp (TCon (AlgTyCon "List")) tp

-- | @ \[\] @
tyList :: Type
tyList = TCon listTyCon

-- | @ (,) @
tyTuple :: Type
tyTuple = TCon tupleTyCon

-- | @ () @
tyUnit :: Type
tyUnit = TCon unitTyCon

-- | List
listTyCon :: TyCon
listTyCon = AlgTyCon "List"

-- | Tuple
tupleTyCon :: TyCon
tupleTyCon = AlgTyCon "Tuple"

unitTyCon :: TyCon
unitTyCon = AlgTyCon "Unit"

-- | (->)
tyArrow :: Type
tyArrow = TCon (AlgTyCon "->")
