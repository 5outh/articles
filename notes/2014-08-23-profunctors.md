---
title: I love Profunctors. They're so easy.
author: Ben Kovach
tags: haskell, profunctors, category theory
---

Notes on [I love Profunctors. They're so easy.](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors).

Normal Haskell functors are *covariant*. Example of a contravariant functor:

```haskell
newtype Predicate a = Predicate{ getPredicate :: a -> Bool } 

class Contravariant f where
    contramap :: (b -> a) -> f a -> f b

instance Contravariant Predicate where
    -- p :: a -> Bool
    -- g :: b -> a
    contramap g (Predicate p) = Predicate (p . g)

veryOdd :: Predicate Integer
veryOdd = contramap (`div` 2) (Predicate odd)

-- True iff n `div` 2 is odd.
main = print $ getPredicate veryOdd <$> [0..11]
```

Here are a couple more contravariant functors:

```haskell
newtype Const a b = Const a
instance Contravariant (Const a) where
    -- _ :: c -> b
    -- a :: a
    contramap _ (Const a) = Const a

newtype Comparison a = Comparison (a -> a -> Ordering)
instance Contravariant Comparison where
    -- g :: b -> a
    -- c :: a -> a -> Ordering
    contramap g (Comparison c) = Comparison (comp `on` g) 

newtype Op a b = Op (b -> a)
instance Contravariant (Op a) where
    -- g :: b -> a
    -- f :: a -> c
    contramap g (Op f) = Op (f . g)
```

Next, bifunctors. EZPZ.

A profunctor is a bifunctor that is contravariant in the first argument and covariant in the second.

```haskell
class Profunctor f where
    dimap ∷ (c → a) → (b → d) → f a b → f c d

-- exercise
instance Profunctor (->) where
    -- dimap :: (c → a) → (b → d) → (a → b) → (c → d)
    -- f :: c -> a
    -- g :: b -> d
    -- h :: a -> b
    -- want : c -> d
    dimap f g h = g . h . f
```

Here's another example; `check` checks user input, and `step` mutates state by clicking an up/down button or pressing the value directly.

```haskell
data Limits a = 
  Limits 
    { step :: a -> (a, a)
    , check :: a -> a -> Bool
    }
```

By generalizing this, we get a profunctor:

```haskell
type Limits a = Limits' a a
data Limits' a = 
  Limits
    { step :: a -> (b, b)
    , check :: a -> a -> Bool 
    }

instance Profunctor Limits' where
  dimap g h Limits{..} = Limits
    { step = (h *** h) . step . g
    -- ^ Regular old covariance (mostly)
    , check = check `on` g
    -- ^ Looks exactly like Comparison from before.
    }
```

Another example: Containers with Keys

```haskell
M.map        ::      (a -> b) -> Map i a -> Map i b
M.mapWithKey :: (i -> a -> b) -> Map i a -> Map i b
```

Let's use `Indexed`:

```haskell
newtype Indexed i a b = Indexed{ runIndexed :: i -> a -> b }

-- exercise (?????)
instance Profunctor (Indexed i) where
  -- dimap :: (c -> a) -> (b -> d) -> Indexed i a b -> Indexed i c d
  -- g :: c -> a
  -- h :: b -> d
  -- f :: i -> a -> b
  dimap g h (Indexed f) = Indexed (dimap g h . f)
```

With `Indexable`, we can unify `M.map` and `M.mapWithKey`:

```haskell
class Indexable i a p where indexed :: p a b -> i -> a b
instance Indexable i (Indexed i) where indexed = runIndexed
instance Indexible i (->)        where indexed = const

mapIndexable :: Indexable i p  => p a b -> Map i a -> Map i b
             ≈  Indexed i a p  -> Map i a -> Map i b
             ≈        (a -> b) -> Map i a -> Map i b
mapIndexable :: Indexable i p => p a b -> Map i a -> Map i b
mapIndexable idx m = M.mapWithKey . indexed
```

Author also mentions `UpStar`, `DownStar`, `Strong` and `Choice` to look into.

Discussion on [reddit](http://www.reddit.com/r/haskell/comments/1bg21c/so_i_accidentally_a_profunctors_tutorial/).