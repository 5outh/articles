---
title: Profunctors in Haskell
author: Ben Kovach
tags: profunctors, category-theory, haskell
---

Notes on [Profunctors in Haskell](http://blog.sigfpe.com/2011/07/profunctors-in-haskell.html)

`difunctors` from [this post](http://blog.sigfpe.com/2009/03/dinatural-transformations-and-coends.html) are actually profunctors.

The most natural definition of [Hughes Arrows](http://www.haskell.org/arrows/) is via profunctors. A bit about tensor calculus: profunctors use similar terms in similar ways.

For categories $C$, $D$, a profunctor is a functor $D^{op} \times C \rightarrow Set$ and is written $C \not\to D$.

Here's a Haskell approximation:

```haskell
class Profunctor h where
    lmap :: (d' -> d) -> h d c -> h d' c
    rmap :: (c -> c') -> h d c -> h d c'
```

NB. `lmap` "goes backward" while `rmap` "goes forward".

Laws: cofunctoriality in the first argument, functoriality in the second (similar to what I just said informally above):

```haskell
lmap (f . g) = lmap g . lmap f
rmap (f . g) = rmp f . rmap g
```

Note: really these are "endprofunctors" (since we're only working in $Hask$).

Many people think of profunctors generalizing functors in the same way relations generalize functions. Specifically, given a function $f : A \to B$, $f$ associates to each element of $A$, a single element of $B$. But if we want $f$ to associate elements of $A$ with elements of $B$ more freely (for example, "mapping" elements of $A$ to multiple elements of $B$) then we instead use a relation which can be written as a function $f: A\times B \to {0,1}$ where we say $xfy$ iff $f(x,y) = 1$. In this case, profunctors map to $Set$ rather than ${0,1}$.

NB. The above can be thought of as a mapping of pairs from $(Dom, Cod)$ to ${True, False}$. If a pair $(x, y) \in f$, then they are "related". This is just relations, though. Not sure about "In this case, profunctors map to $Set$ rather than ${0, 1}$" yet...

An example: `(->)`

```haskell
instance Profunctor (->) where
    lmap f g = g . f
    rmap f g = f . g
```

The first argument of a profunctor (often) describes how an element related to a type is "sucked in," where the second describes what is "spit out". `a -> b` sucks in an `a` and spits out a `b`.

Given a function $f$, we can turn it into a relation by saying $xfy \iff y=f(x)$. Similarly, given a functor $F:C \to D$, we can define a profunctor $F^*:C \not\to D$ by (annotated):

```haskell
data UpStar f d c = UpStar (d -> f c)

instance Functor f => Profunctor (UpStar f) where
    lmap :: (d' -> d) -> UpStar f d c -> UpStar f d' c
    lmap k (UpStar f) = UpStar $
          f {- :: d -> f c -} 
        . k {- :: d' -> d -}
    rmap :: (c -> c') -> UpStar f d c -> UpStar f d c'
    rmap k (UpStar f) = UpStar $
        fmap k {- :: f c -> f c' -} 
        . f    {- :: d -> f c -}
```

The second argument to a profunctor plays a similar role to the return value of a functor, just like the second argument to a relation plays a role similar to the return value of a functor (sometimes).

NB. These look a lot (exactly) like Kleisli arrows:

```haskell
data Kleisli m a b = UpStar (a -> m b)
-- or, generalized:
data (a :-> b) = forall m. UpStar (a -> m b)
```

There's also an opposing way $F_*:C \not\to D$:

```haskell
data DownStar f d c = DownStar (f d -> c)

instance Functor f => Profunctor (DownStar f) where
  lmap k (DownStar f) = DownStar (f . fmap k)
  rmap k (DownStar f) = DownStar (k . f)
```

(Not gonna annotate this one, it didn't help that much. I understand it can be done)

NB. These look a lot like Cokleisli arrows:

```haskell
data Cokleisli w a b = DownStar (w a -> b)
-- or generalized:
data (a :-< b) = forall w. DownStar (w a -> b) 
```

The `Identity` functor gives something isomorphic to `(->)` when using `UpStar` or `DownStar` (which makes sense, since they're like `Kleisli` and `Cokleisli`).

Dinatural transformations exist between profunctors. First, for $F,G$ functors, with $h$ a natural transformation $F \Rightarrow G$, we have:

```haskell
h . fmap f = fmap f . h
```

If we think of $F$ and $G$ as containers, the rule says that a natural transformation relates the structure, not the contents. So using $f$ to replace the elemnts with others should be invisible to $h$ and thus commute with it.

With dinatural transformations, instead of relating the argument to a natural transformation to its return result, it instead relates the two arguments of a profunctor.

Given $F,G$ profunctors, a dinatural transformation is a polymorphic function of the form:

```haskell
type Dinatural f g = forall a. f a a -> g a a
-- or
type f :-<> g = forall a. f a a -> g a a
```

But we also want something analogous to natural transformation. if $\phi : Dinatural \, F \, G$, then $phi$ shouldn't see the elements of `F a a` or `G a a`. Say $\phi$ is a natural transformation (as above). Say we also have a function $f : X \to X'$. Then we can use `lmap` to apply $f$ on the left or right of $F$ and $G$. The definition of dinaturals demands that ``rmap f . `$\phi$` . lmap f = lmap f . `$\phi$` . rmap f`. i.e we can sandwich $\phi$ (through `{l,r}map f`) and achieve the same result (probably comes as a free theorem).

Composing functors is easy. It's not that easy to compose profunctors, because profunctors don't necessarily map back to the same space you started with (unlike functors). Using the relation analogy as before, and given composition of relations:

Suppose $R,S$ relations. Then $S \circ R$ means that $xTz \iff \exists y : xRy \wedge ySz$. Noting that existential types are somewhat of an analogue to "there exists" in logic, we can define:

```haskell
data Compose f g d c = forall a. Compose (f d a) (g a c)
```

Functors give rise to profunctors, so we want composition of functors to be compatible with composition of profunctors. Consider `Compose (UpStar F) (UpStar G`) for some `F, G`. This is essentially the same as `exists a. (f :: d -> F a, g :: a -> G c)`.

NB. This looks super simple, but it's not. Remember how complex the definitions are: $F^* \circ G^*$

What does this mean? If we look at the types we get some long chain of logic that finally gives us the following construction:

```haskell
fmap g . f :: d -> F (G c)
```

but this is all we can obtain from thie construction! We can replace `exists a` with the coend operator, though! We also implicitly used the product operation in the constructor `Compose` so this definition will work in all symmetric monoidal categories ( (╯°□°）╯︵ ┻━┻).

Under profunctor composition, `->` is the identity (up to isomorphism). Due to this, we can't make a category out of profunctors in the obvious wan, but we can make a bicategory -- a category where we have to explicitly track isomorphisms between things that are equal in normal categories.

(Skipping Tensors; I don't know about them so I suspect I won't get much out of it)

Hughes' Arrows are profunctors. If `A` is an `Arrow`, we often think of `A d c` as consuming something related to a type `d` and emitting something related to a type `c`. The same goes for profunctors.