{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module FixNat where

data Fix f = Fix {unFix :: f (Fix f)}
deriving instance (Show (f (Fix f))) => Show (Fix f)

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . unFix

data Nat n = Zero | Succ n deriving Show
instance Functor Nat where
  fmap _ Zero     = Zero
  fmap f (Succ n) = Succ (f n)

fromNat :: Fix Nat -> Int
fromNat = cata f where
  f :: Nat Int -> Int
  f Zero     = 0
  f (Succ n) = n + 1

toNat :: Int -> Fix Nat
toNat 0 = Fix Zero
toNat n = Fix (Succ (toNat (n-1)))

oneMore :: Fix Nat -> Fix Nat
oneMore = cata f where
  f :: Nat (Fix Nat) -> Fix Nat
  f Zero = Fix (Succ (Fix Zero))
  f (Succ n) = Fix (Succ n)

double :: Fix Nat -> Fix Nat
double = cata f where
  f :: Nat (Fix Nat) -> Fix Nat
  f Zero = Fix Zero
  f (Succ n) = Fix (Succ (Fix (Succ n)))

add :: Fix Nat -> Fix Nat -> Fix Nat
add n = cata f where
  f :: Nat (Fix Nat) -> Fix Nat
  f Zero     = n
  f (Succ m) = Fix (Succ m)

mul :: Fix Nat -> Fix Nat -> Fix Nat
mul n = cata f where
  f :: Nat (Fix Nat) -> Fix Nat
  f Zero     = Fix Zero
  f (Succ m) = add n m

run :: Int
run = do
  let a = toNat 3
  let b = toNat 2
  fromNat (double (add (mul (oneMore a) b) a)) -- 22
