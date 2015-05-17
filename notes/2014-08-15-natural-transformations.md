---
title: You Could have Defined Natural Transformations
author: Ben Kovach
tags: category-theory, haskell
---

Notes on [You Could have Defined Natural Transformations](http://blog.sigfpe.com/2008/05/you-could-have-defined-natural.html)

Goal:

- $f: \mathbb{Z} \times \mathbb{Z} \rightarrow \mathbb{Z} \hspace{10mm} f(x,y) = x + y$
- $g: \mathbb{Z} \times \mathbb{Z} \rightarrow \mathbb{Z} \hspace{10mm} f(x,y) = x $

are obviously different because `g` doesn't use the fact that it's acting on integers. How can we make this idea precise?

In a programming language with static types, we can do this with type signatures. How to do it with set theory? No mechanisms to do such a thing.

Consider:

$$h : \mathbb{Z} \times \mathbb{Z} \rightarrow \mathbb{Z} \times \mathbb{Z} \times \mathbb{Z} \hspace{10mm} h(a, b) = (b,a,a)$$

What does it mean to say that $h$ doesn't use specific facts about its arguments? RHS clearly depends on them, but somehow doesn't make use of their properties.

Partly, replacing one argument by another value yields the same substitution on both sides. For example:

$$h(1, 2) = (2, 1, 1) \xrightarrow{sub(1, 3)} h(3, 2) = (2, 3, 3)$$

So for any "substitution function" $k : \mathbb{Z} \times \mathbb{Z} \rightarrow \mathbb{Z}$, we want:

$$h(k(1), k(2)) = (k(2), k(1), k(2))$$

We've made use of functions; but we want to apply this to many types of mathematical "containers" like tuples or sequences; need a generalization.

The function that converts $X \rightarrow X \times X$ (e.g.) forms part of a functor. The other part maps functions between sets to functions between containers. E.g. The usual ordered pair functor $L$ with $LX = X \times X$ and $Lf(x, y) = (f(x), f(y))$

Now we can talk about functors instead of containers. Now rewriting the rule, we have

$$ h(Lk(a, b)) = Mk(h(a, b)) $$

Note: if $h$ doesn't rely on specific properties, then there is a version of $h$ for any set $X$:

$$ h_X : X \times X \rightarrow X \times X \times X \hspace{10mm} h_X(a, b) = (b, a, a) $$

So now when we use a function $k$ to substitute values in the arguments, we don't need to use the same set.

More generally, $h$ is a family of functions such that $\forall k: X \rightarrow Y$, we have:

$$h_Y \circ Lk = Mk \circ h_X$$

This would hold in any category; not just the category of sets.

A natural transformation between functors $L, M: C \rightarrow D$ is a family of morphisms $h_X$, one $\forall X \in C$, such that for all morphisms $k : X \rightarrow Y \in C$:

$$h_Y \circ Lk = Mk \circ h_X$$

sigfpe recommends looking at the category of vector spaces to start looking at this idea from a different perspective. $Set$-like categories are cool but the idea of "too many assumptions" starts to change.

Note that category theory was basically invented for natural transformations. 

Next step: [the Yoneda Lemma](http://blog.sigfpe.com/2006/11/yoneda-lemma.html). [Theorems for Free!](http://ttic.uchicago.edu/~dreyer/course/papers/wadler.pdf) is about similar substitutions on function arguments.