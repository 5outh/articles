---
title: Hylomorphisms and treesort
author: Ben Kovach
tags: recursion-schemes, papers
---
Consider the following data structure, representing a binary search tree:

```haskell
data  BST a = 
    Tree (BST a) a (BST a)
  | Empty deriving (Eq)
```

As it turns out, this data structure provides a nice way to introduce the concepts of different types of morphisms used all over the place in Haskell - the fold, or "catamorphism", the unfold, or "anamorphism", and compositions of the two, the "hylomorphism" and "metamorphism."

The bracket notations that I'll use below come from Meijer, Fokkinga and Patterson's excellent paper [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire](http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf). If you enjoy the article, I'd suggest giving it a look!

I'm writing this because I recall encountering these names when learning Haskell early on and being very confused, particularly by the latter two types. Folds (and to a lesser extent, unfolds) are commonplace in Haskell. Hylo- and metamorphisms are also pretty common, but they're not as easy to spot. From [Wikipedia](http://en.wikipedia.org/wiki/Hylomorphism_(computer_science)):

> In computer science, and in particular functional programming, a hylomorphism is a recursive function, corresponding to the composition of an anamorphism (which first builds a set of results; also known as 'unfolding') and a catamorphism (which then folds these results into a final return value)

The canonical example of a hylomorphism is the factorial function, which (usually) implicitly composes functions. The goal of this post is to lay out an **explicit** example of a hylo- (and meta-) morphism in a natural way. We'll start with a couple of functions:

```haskell
insert :: Ord a => a -> BST a -> BST a
insert x Empty = Tree Empty x Empty
insert x (Tree left a right)
  | x < a     = Tree (treeInsert x left) a right
  | otherwise = Tree left a (treeInsert x right)

fromList :: Ord a => [a] -> BST a
fromList xs = foldr treeInsert Empty xs
```

We have an insertion function and our first example of a catamorphism, `fromList`! We're folding all values from a list into a new structure (a `BST`) and destroying the list in the process. This function can be written in so called "banana brackets," like so: $fromList = (\!\left|treeInsert\right|\!)$.

I`fromList` can also be considered an anamorphism. Catamorphisms destroy structures to build final values, whereas anamorphisms take an initial seed value and build a new structure from it. In `fromList`, `xs` can be considered a "seed" value to build a `BST a` from, making `fromList` a perfectly valid anamorphism as well. As such, `fromList` can also be written in "lens brackets": $fromList = [\!(treeInsert)\!]$.

We can also define a new pair of cata/anamorphisms by folding the tree into a list:

```haskell
foldTree :: (a -> b -> b) -> b -> BST a -> b
foldTree f b Empty = b
foldTree f b (Tree left a right) = foldTree f ( f a (foldTree f b right) ) left

toList :: BST a -> [a]
toList t = foldTree (:) [] t
```

`foldTree` is analogous to `foldr` (and would be a fine definition for `foldr` in a `Foldable` instance), and `toList` destructs (folds) a `BST a` into an `[a]`. Thinking this way, `toList` again defines a catamorphism, this time from `BST a -> [a]`, denoted $toList = (\!\left| : \right|\!)$. But we can also think of `toList` as unfolding a `BST a` into an `[a]`, so we can analogously define an anamorphism $toList = [\!( : )\!]$. 

There's something interesting about `toList`: `foldTree` traverses a `BST` in order, so it actually produces a sorted list (given that elements are `insert`ed rather than randomly placed!). Now we have a way to construct a binary search tree from a list of elements, and destruct a binary search tree into a sorted list of elements. This gives rise to a simple definion of a sorting algorithm, namely:

```haskell
treesort :: Ord a => [a] -> [a]
treesort = toList . fromList
```

Because  `toList` and `fromList` are both cata- and anamorphims, `treesort` actually defines a hylomorphism *and* a metamorphism.

As we noted before, a hylomorphism is defined as the composition of an anamorphism (unfold) with a catamorphism (fold). If we think of `fromList` as an anamorphism and `toList` as a catamorphism, we have constructed a hylomorphism directly. Namely, the function $treesort = [\![([], (:)),(insert, null)]\!]$ (the brackets here are commonly called "envelopes"). `null` isn't explicit in the definition of `treesort` (instead, it's implicit in `foldr`), but it describes a terminating condition for `fromList`. Similarly, `[]` is just the container to fold values into.

We can once again think of this function in the opposite manner by thinking of `fromList` as a catamorphism and `toList` as an anamorphism, giving rise to a metamorphism, defined by composition in the opposite direction. Metamorphisms (as far as I know) have no bracket notation in literature, but I want to mention that we do have a name for such things. My guess is that any metamorphism can actually be thought of as a hylomorphism, since the objects being operated on must be both foldable and unfoldable, but I don't know for sure.

Finally, note that we can also create another function:

```haskell
what :: Ord a => BST a -> BST a
what = fromList . toList
```

which is also a hylo- and metamorphism. However, this isn't very useful (in fact, one might consider it counterproductive), but I'll leave it as an exercise to the reader to figure out why.

Thanks for reading!

Ben