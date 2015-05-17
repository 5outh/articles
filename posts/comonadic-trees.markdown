---
title: Comonadic Trees
author: Ben Kovach
---
We've seen in the previous post what monads are and how they work. We
saw a few monad instances of some common Haskell structures and toyed
around with them a little bit. Towards the end of the post, I asked
about making a Monad instance for a generalized tree data type. My guess
is that it was relatively difficult. But why? Well, one thing that I
didn't touch on in my last post was that the monadic `>>=` operation can
also be represented as `(join . fmap f)`, that is, `a >>= f = join $
fmap f a`, as long as `a` has a `Functor` instance. `join` is a generalized
function of the following type: 

```haskell
join :: (Monad m) => m (m a) -> m a
```

Basically, this function takes a nested
monad and "joins" it with the top level in order to get a regular `m a`
out of it. Since you bind to functions that take as and produce `m a`s,
`fmap f` actually makes `m (m a)`s and `join` is just the tool to fix up the
`>>=` function to produce what we want. Keep in mind that `join` is not a
part of the `Monad` typeclass, and therefore the above definition for
`>>=` will not work. However, if we are able to make a specific `join`
function (named something else, since `join` is taken!) for whatever type
we are making a `Monad` instance for, we can certainly use the above
definition. I don't want to spend too much time on this, but I would
like to direct the reader to the `Monad` instance for `[]` that I mentioned
in the last post -- can you see any similarities between this and the
way I structured `>>=` above? Now back to the `Tree` type. Can you devise
some way to join `Tree`s? That is, can you think of a way to flatten a
`Tree` of `Tree a`s into a `Tree a`? This actually turns out to be quite
difficult, and up to interpretation as to how you want to do it. It's
not as straightforward as moving `Maybe (Maybe a)`s into `Maybe a`s or
`[[a]]`s into `[a]`s. Maybe if we put a `Monoid` restriction on our `a` type, as
such... 

```haskell
data Tree a = Node a [Tree a] | Leaf a deriving (Show, Eq)

instance (Monoid a) => Monad (Tree a) where ...
```
...we could use `mappend` in some way in order to
concatenate all of the nested elements using some joining function.
While this is a valid way to define a `Tree Monad`, it seems a little bit
"unnatural." I won't delve too deeply into that, though, because that's
not the point of this post. Let's instead take a look at a structure
related to monads that may make more sense for our generalized `Tree`
type.

#### What is a comonad?

Recall the type signature of `>>=` for Monads: 

```haskell
(>>=) :: (Monad m) => (m a) -> (a -> m b) -> m b
```

That is, we're taking an m a and converting it to an m b by means
of some function that operates on the contained type. In other words,
we're producing a new value using elements contained inside the Monad.
Comonads have a similar function:

```haskell
(=>>) :: (Comonad w) => w a -> (w a -> b) -> w b
```
The difference here is that the function that we use to produce a
new value operates on the whole -- we're not operating on the elements
contained inside the Comonad, but the Comonad itself, to produce a new
value. We also have a function similar to the Monadic `return`: 

```haskell
coreturn :: (Comonad w) => w a -> a
```
Whereas
`return` puts a value into a Monadic context, `coreturn` extracts a value
from a Comonadic context. I mentioned the Monadic `join` function above
because I would also like to mention that there is a similar operation
for Comonads: 
```haskell
cojoin :: (Comonad w) => w a -> w (w a)
```
Instead of "removing a layer," we're "adding" a layer.
And, as it turns out, just as `a >>= f` can be represented as `join $ fmap f a`, `=>>` can be representaed as : `a =>> f = fmap f $ cojoin a`.

The full Comonad typeclass (as I like to define it) is
as follows: 

```haskell
class (Functor w) => Comonad w where
	coreturn :: (Comonad w) => w a -> a
	cojoin   :: (Comonad w) => w a -> w (w a)
	a =>> f = fmap f $ cojoin a 
```
By now, it should at least be clear that Monads and
Comonads are related -- it shouldn't be hard to see why they are so
similar in name! *Note: There is a package called `Control.Comonad` on
Hackage. It uses different names for the Comonad operations, but they do
the same things. It's a good package, but I wanted to show how Comonads
are built and use the operation names I used to make things clearer.* **

#### What can I do with Comonads?

As it turns out, the `Tree a` structure that I've been mentioning fits
into the Comonadic context quite well, and provides a simple example as
to how Comonads work.

We'll start with the implementation of the Comonad typeclass mentioned
above:

```haskell
class (Functor w) => Comonad w where
  coreturn :: w a -> a
  cojoin   :: w a -> w (w a)
  (=>>)    ::  w a -> (w a -> b) -> w b
```

Then we'll go ahead and make a `Functor` instance of our `Tree a` data type:

```haskell
data Tree a = Node a [Tree a] | Leaf a deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node a b) = Node (f a) (map (fmap f) b)
```

From here, we're able to make a Comonadic `Tree` like so:

```haskell
instance Comonad Tree where
  coreturn (Leaf a) = a
  coreturn (Node a _) = a
  cojoin l@(Leaf a)   = Leaf l
  cojoin n@(Node a b) = Node n (map cojoin b)
  x =>> f = fmap f $ cojoin x
```

The only real point of confusion here is in the `cojoin` function for
`Node`s. But, all we are doing is wrapping the entire node in a new node,
and then mapping cojoin over every child node of the current node to
produce its children. In other words, we're turning a `Node a` into a `Node
(Node a)`, which is exactly what we want to do. So what can we do with a
comonadic tree? Let's tackle a simple problem. Say we're hanging out in
California, and we want to get to the East coast as quickly as possible.
We don't really care where on the East coast we end up -- we just want
to get there. We can map out the different paths that we can take, along
with the time it takes to get to each one. In other words, we're going
to model spots on the map as `Node`s on a `Tree` and give them a weight
corresponding to the time it takes to get there. We can model this
situation with a `Tree` as follows:

```haskell
trip = 
 Node 0 
   [Node 6 
     [Node 2 
       [Node 6 
         [Leaf 1], 
        Leaf 14], 
        Node 1 
          [Leaf 19], 
        Node 8 
          [Leaf 21, 
           Node 4 
           [Leaf 2, Leaf 6]
          ]
      ],
    Node 3 
      [Node 9 
        [Leaf 12], 
       Node 14 
        [Leaf 6, 
         Node 3 
           [Leaf 1]
        ]
      ]
   ]
```

The number of each `Node` marks its distance from the previous `Node`. The
root Node of the `Tree` is the starting point, so it is 0 distance away
from itself. What we want to do to find the shortest path through the
country is essentially, as follows. First, we're going to need to check
the deepest nodes, and find their minimum distance children. We will add
the distance of the `Node` closest to the one we're examining to its own
distance, to find the shortest way to get from the node we're examining
to the destination. Once all of that has been done, we'll need to
traverse up the tree, repeating this as we go. By the end of the
algorithm, the root `Node` will be marked with the shortest distance to
the destination. Now, that may sound somewhat iterative in nature, but
we're going to morph this into a comonadic operation. First, let's take
a look at a function we can use to find the minimum distance to the next
level of our tree:

```haskell
shortest :: (Num a, Ord a) => Tree a -> a
shortest (Leaf x) = x
shortest (Node x xs) = x + (minimum $ map shortest xs)
```

This is relatively simple, and does precisely what was mentioned above:
adds the minimum value contained in the connected `Node`s to the parent
`Node`. Next, take a look at the type signature. We can see that this
function produces a new number from a tree full of numbers. This
coincides precisely with the function type we need to use with `=>>` so
we'll be able to use it to get exactly what we want. The rest is very
simple:

```haskell
minimumDist = trip =>> shortest
```

This produces a tree full of the minimum distances from each node to the
East coast. Pulling the actual value out is as easy as calling `coreturn`
on the resultant tree. 

Thanks for reading!

Ben

*For further reading on
Comonads, I recommend the article [Evaluating Cellular Automata is
Comonadic](http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html).*
