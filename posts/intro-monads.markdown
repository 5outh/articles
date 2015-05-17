---
title: Monads in Haskell: An Introduction
author: Ben Kovach
---

#### What is a Monad?

If you've used Haskell before, chances are that you've heard the term
"monad" a good bit. But, you might not know what they are, or what they
are really used for. It is my goal in this blog post to shed some light
on what monads are, introduce and define few simple ones, and touch on
their usefulness. I will assume basic familiarity with Haskell syntax in
this blog post. A working knowledge of monoids and functors will also be
beneficial, although not strictly necessary. Without further ado, let's
go ahead and look at the `Monad` typeclass:

```haskell
class Monad m where  
    return :: a -> m a  
    (>>=) :: m a -> (a -> m b) -> m b
```

We can see a Monad has two operations, return and `>>=` (this is
commonly pronounced "bind"). You may suspect that you know what return
does from other programming languages, but be warned: Haskell's `return`
is very different! Haskell's return *only* operates on `Monad`s, and
essentially acts as a "wrapping" function. That is, if you call `return`
on a value, it will turn it into a monadic value of that type. We will
look at an example of this shortly. The second operation, `>>=`, takes
two arguments: an `a` wrapped in a `Monad m `(I will refer to this as `m a`
from now on), and a function that converts an `a` to `b` wrapped in the same
type of `Monad`, `m`. It produces a value of type `b`, wrapped in a `Monad` `m` (I
will call this `m b` from now on). This may sound complicated, but I'll do
my best to explain it after showing the most basic `Monad` type, namely,
the `Identity` `Monad`:


```haskell
data Identity a = Identity a deriving Show

instance Monad Identity where
  return = Identity
  (Identity a) >>= f = f a
```

*Also defined in
[Control.Monad.Identity](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Identity.html)*

The `Identity` data declaration defines only one type: `Identity a`.
Basically, this is just a wrapper for a value, and nothing more. `return`
is simply defined as `Identity`. We can, for example, call `return 3` and
get `Identity 3`. Turning values into `Identity`s is as simple as that.
The bind function may look a little bit more obscure, though. Let's look
closely: We first use pattern matching to be able to operate on the type
that the Identity Monad wraps (Identity a), bind it (`>>=`) to a
function (`f`). Since `f` converts normal values into monadic ones (look at
the type declaration), we can simply apply `f` to `a`, and we've produced a
new `Identity`. Here are some examples:

```haskell
m1 = Identity 3

addThree x = return $ x+3
bindExample1 = m1 >>= addThree

m2 = Identity "salami"

getLength x = return $ length x
bindExample2 = m2 >>= getLength
```

Basically the real tough part of understanding Monads is just
understanding `>>=`. A little bit of dissection can explain a lot, but
it took me months to really grasp the concept. On the surface, you're
morphing an `m a` into an `m b`. At first, I thought that the `m`'s in the
type signature were allowed to different types of Monads. For instance,
I thought that Monads made it possible to bind `Identity Int`s to
functions that produced `[String]`s (we'll look at the `[]` Monad in a
minute). This is wrong, and for good reason! It was incredibly confusing
to think about a function that could generalize to this degree and it
is, to my knowledge, not possible to do so. The Monad (wrapper) type is
retained, but the *wrapped* type can be changed by means of the function
you bind to. The second thing that I want to stress is what `>>=` really
does. What we're doing with the `>>=` function is turning an `m a` into an
`m b`, but it's not so direct as you might want to think. 

The function
argument in `>>=` ( `(a -> m b)` ) was really confusing to me to
begin with, so I'm going to try to explain it. The function you're
binding your `m a` to must take an `a` and return an `m b`. This
means that you're acting on the *wrapped* part of the Monad in order to
produce an entirely new Monad. It took me a while to realize what
exactly was happening, that I wasn't supposed to be directly converting
`m a` to `m b` by means of my argument function (that's actually
what `>>=` is for). Just remember that the function you bind to should
operate on an `a` to produce an `m b`. With that knowledge, let's take a
look at some more examples of Monads.

#### More Monads

If you've used Haskell, you're likely already familiar with the `Maybe`
data type:

```haskell
instance Monad Maybe where
	return = Just
	Just x >>= f = Just $ f x
	Nothing >>= f = Nothing
```

*Defined in the
[Prelude](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html)*


The Maybe Monad is hardly an extension of the aforementioned
`Identity` Monad. All we are really adding is the functionality to
fail -- that is, to produce a `Nothing` value if necessary. In fact,
the `Just a` type is exactly the same as `Identity a`, except
that a `Nothing` value anywhere in the computation will produce a
`Nothing` as a result. Next, we'll look at the Monad instance for
`[]`:

```haskell
instance Monad [] where
  return x = [x]
  xs >>= f = concatMap f xs
```

*Defined in the
[Prelude](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html)*


The `[]` Monad is a little more complicated, but it's really nothing too
new. For return, we just have to wrap our value inside a list. Now,
let's look at `>>=`. `concatMap` actually *is* the `>>=` instance for
lists. Why? Well, `concatMap` is, simply, `(concat . map)`. We know that `map`
takes a list of `a`s and maps a function over them, and `concat` is a
function that flattens lists, that is, turns `[[a]]`s into `[a]`s. When we
compose these two functions (or use `concatMap`), we're making a function
that must produce `[[a]]`s out of `[a]`s, and then flatten them back to
`[a]`s. That is exactly how `>>=` works for lists! Now, let's take a look
at a slightly more complicated Monad, namely, the `Reader` Monad:

```haskell
instance Monad ((->) r) where
  return = const
  f >>= g = \r -> g (f r) r
```

*Also defined in
[Control.Monad.Instances](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Monad-Instances.html)*

This one requires some explanation, because it's a bit of a leap from
the last three, since we're defining a Monad instance for *functions*. I
have added a couple of comments in the above code in order to further
explain how everything is working in the `(->) r` Monad. Well, first
things's first. We define `return` as `const`, which takes some arbitrary
argument and produces a function that produces a constant value (what we
pass in to `return`). This is exactly what we want; a minimal context for
functions. (`>>=`) is a bit more complex. First, let's take a look at
the type signature, specific to this Monad. We're binding a function
(`r -> a`) to a function (`a -> (r -> b)`) to produce a function
of type (`r -> b`). So in the declaration of the function, `f` is
of type (`r -> a`) and `g` is of type (`a -> (r -> b)`). I've
conveniently named the argument our function is going to take `r`, for
clarity. This is what the user will pass into the function that (`>>=`)
produces. `f` takes an `r`, so we apply the value from the lambda
into it to produce an `a`. We then use that `a` to produce an (`r
-> b`), using `g`. This produces our function, but it needs to be
fed some value, so we finally apply it to `r` in order to complete
everything. If any of this sounds confusing, try to work through it on
paper and read the comments in my code -- after a bit of staring, it
should start to make sense. I've also implemented the `mean` function
here, using the `(->) r` Monad with and without do-notation.

```haskell
-- using >>= and lambda functions
mean' = (fromIntegral . length) >>= \l -> sum >>= \s -> return $ s / l

-- translation using do notation
mean = do
  l <- (fromIntegral . length)
  s <- sum
  return $ s / l

-- these are equivalent
ans  = mean  [1, 2, 3, 9, 2] -- 3.4
ans' = mean' [1, 2, 3, 9, 2] -- 3.4
```

We've only seen the tip of the
iceberg when it comes to Monads, but this should serve as a good basic
introduction. Once you get used to seeing Monads, they become a bit less
scary. The more time you spend with them, the more comfortable you'll
feel with them. Before I end this post, I want to leave a question for
the reader, as follows: 

> Given the general `Tree` data type represented below, can you intuitively make a Monad instance?


```haskell
data Tree a = Node a [Tree a] | Leaf a deriving (Show, Eq)
```

I'll explain why this is difficult when we talk about a similar
mathematical type in the next post. 


Thanks for reading, 

Ben 

*For more
introductory monad material, I suggest the following resources:*

-   [Learn You a Haskell For Great Good! A Fistful of
    Monads](http://learnyouahaskell.com/a-fistful-of-monads)
-   [A Neighborhood of Infinity - You Could Have Invented
    Monads](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
-   [Brian Beckman - Don't Fear the
    Monad](http://channel9.msdn.com/Shows/Going+Deep/Brian-Beckman-Dont-fear-the-Monads)

