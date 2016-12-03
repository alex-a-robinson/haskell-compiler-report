 # F-Algebras, Catamorphisms & Category Theory
**Aim**: Implment and understand catamorphisms

-  [Understanding F-Algebras
](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras)
- [Catamorphisms in 15 Minutes!](http://chrislambda.github.io/blog/2014/01/30/catamorphisms-in-15-minutes/)
- [The Functor typeclass](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass)
- [Creative catamorphisms](http://blog.ezyang.com/2010/04/creative-catamorphisms/)
- [Catamorphisms F#](https://sidburn.github.io/blog/2016/05/28/catamorphisms)
- [Catamorphism](https://en.wikipedia.org/wiki/Catamorphism)
- [Catamorphisms](https://wiki.haskell.org/Catamorphisms)
- [Constructing_Language_Processors_with_Algebra_Combinators](http://www.ittc.ku.edu/~nfrisby/papers/drafts/Constructing_Language_Processors_with_Algebra_Combinators.pdf)
- [Catamorhpism Code examples in haskell](https://github.com/frerich/catamorphism)
- [Category theory in haskell](https://en.wikibooks.org/wiki/Haskell/Category_theory)
- [Category Theory + Catamorhisms presentation](http://yogsototh.github.io/Category-Theory-Presentation/#slide-85)
- [Category Theory](http://www.alpheccar.org/content/74.html)
- [A Gentle Introduction to Category
Theory](https://maartenfokkinga.github.io/utwente/mmf92b.pdf)
- [Fix and recursion](https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion)

 ## TODO
- Build standard catamorphisms for bool, maybe, either and foldr
- Build custom catamorphism for custom recursive data structure

 ## Plan
- Introduce category theory
- Introduce F-Algebras
- Introduce Catamorhpisms
- Explain why they are used
- Practical examples in haskell

 ### Category Theory
A categroy is a simple collection made up of:
- objects
- morphisms - links two objects `f : A->B`

Composition of these morphisms `g: A->B`, `f: B->C` `f ∘ g: A->C`

**laws:**
- composition of morphism is associative
    `f ∘ (g ∘ h) = (f ∘ g) ∘ h`
- catogires are closed under composition
    `f: B->C, g: A->B, then there exists a h: A->C s.t. h = f∘g`
- Every object has an identify morphism s.t. `id_A: A->A. g: A->B, g ∘ id_A = id_B ∘ g = g`


Moving onto functors, a functor is a transformation between categories. Given Categories `C` and `D` we define a functor `F: C->D` which we can apply to any object in C to map into D i.e. object `A` in C maps to `F(A)` in D. We can also apply this functor to morphisms, `f:A->B` in C becomes `F(f): F(A)->F(B)` in D. We can define a identiy functor which maps objects and morphisms in a category to themselves.

**laws of functors**:
- Given an identiy morphism `id_A` on object `A`, `F(id_A) = id_[F(A)]`
- Functors distribute over morphism composition
    `F(f∘g) = F(f)∘F(g)`

this second law is useful, Picturing the functor as a list or similar container, the right-hand side is a two-pass algorithm: we map over the structure, performing g, then map over it again, performing f. The functor axioms guarantee we can transform this into a single-pass algorithm that performs f . g. This is a process known as fusion.


**Why is it useful?**


NOTE: More material on wikibook however moves into monards. Maybe look at `Hask` in haskell

 ## Introduction

What is an algebra? It a vector space with a Fixltiplication operation. An F-algebra takes this down to the bare basics, a functor, a type and a function. Algebras have teh ability to form expressions and the ability to evavalute these expressions.

 ## Recursion can be abrastracted away

 ## Catamorphisms
A catamorphism is a generalisation of the fold on lists. It computes a value from a container like strucutre and a function. Formaly it deconstructs a value using an F-algebra for its underlying functor. [Source](https://www.quora.com/What-is-a-catamorphism)

Catamorphisms use a F-algraba on a data strucutre to release its underlying functor.

> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module Cata where
>
> --data Fix f = In (f (Fix f))
>
> --deriving instance (Show (f (Fix f))) => Show (Fix f)
>
> --Fix :: Fix f -> f (Fix f)
> --Fix (In x) = x
>
> -- Algebra
> type Alg f a = f a -> a
>
> -- Fix is FixedPoint
> -- Fix also know as Mu
> data Fix f = Fix {unFix :: f (Fix f)}
> deriving instance (Show (f (Fix f))) => Show (Fix f)
>
> cata :: Functor f => Alg f a -> Fix f -> a
> cata f = f . fmap (cata f) . unFix
>
> data Nat n = Zero | Succ n deriving Show
>
> instance Functor Nat where
>   fmap _ Zero     = Zero
>   fmap f (Succ n) = Succ (f n)
>
> out :: Fix Nat -> Int
> out = cata f where
>   f :: Nat Int -> Int
>   f Zero     = 0
>   f (Succ n) = n + 1
>
> in' :: Int -> Fix Nat
> in' 0 = Fix Zero
> in' n = Fix (Succ (in' (n-1)))
>
> oneMore :: Fix Nat -> Fix Nat
> oneMore = cata f where
>   f :: Nat (Fix Nat) -> Fix Nat
>   f Zero = Fix (Succ (Fix Zero))
>   f (Succ n) = Fix (Succ n)
>
> add :: Fix Nat -> Fix Nat -> Fix Nat
> add a = cata (f a) where
>   f :: Fix Nat -> Nat (Fix Nat) -> Fix Nat
>   f b Zero     = s b
>   f _ (Succ n) = Fix (Succ n)
>
> --add' :: Fix Nat -> Fix Nat -> Fix Nat
> --add' a = cata (f . s) where
> --  f :: Nat (Fix Nat) -> Fix Nat
> --  f b Zero     = Fix Zero
> --  f _ (Succ n) = Fix (Succ n)
>
> mul :: Fix Nat -> Fix Nat -> Fix Nat
> mul a = cata (f a) where
>   f :: Fix Nat -> Nat (Fix Nat) -> Fix Nat
>   f _ Zero = Fix Zero
>   f _ (Succ n) = add a n
>
> s :: Fix Nat -> Fix Nat
> s = cata f where
>   f :: Nat (Fix Nat) -> Fix Nat
>   f Zero     = Fix Zero
>   f (Succ n) = Fix (Succ n)
>
>
> --plus' :: Nat -> Fix Nat ->
>
> double :: Fix Nat -> Fix Nat
> double = cata f where
>   f :: Nat (Fix Nat) -> Fix Nat
>   f Zero = Fix Zero
>   f (Succ n) = Fix (Succ (Fix (Succ n)))
>
>
> --plus :: Fix Nat -> Fix Nat -> Fix Nat
> --plus f = cata f where
>  -- f :: Fix Nat -> Fix Nat
>  -- f Zero     = Zero
>  -- f (Succ n) = Fix (Succ n)

 > value (x:xs) = (Succ x ): value xs

 > type Algebra f a = f a -> a
 > newtype Fix f = Fix { outF :: f (Fix f) }
 > cata :: Functor f => Algebra f a -> Fix f -> a
 > cata f = f . fmap (cata f) . outF
 >
 > data NatF a = S a | Z deriving (Eq,Show)
 > type Nat = Fix NatF
 > instance Functor NatF where
 >    fmap f (S z) = S (f z)
 >    fmap _ Z = Z
