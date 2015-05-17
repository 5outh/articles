---
title: Symbolic Calculus in Haskell
author: Ben Kovach
---

#### Motivation

It's relatively simple to write a program to approximate derivatives. We
simply look at the limit definition of a derivative: $$ \frac{d}{dx}
f(x) = \lim_{h \rightarrow 0} \frac{f(x+h) - f(x)}{h} $$ and choose
some small decimal number to use for $h$ instead of calculating the
limit of the function as it goes to $0$. Generally, this works pretty
well. We get good approximations as long as $h$ is small enough.
However, these approximations are not quite exact, and they can only be
evaluated at specific points (for example, we can find
$\frac{d}{dx}f(5)$, but not the actual symbolic function). So, what if
we want to know what $\frac{d}{dx}f(x)$ is *for all x*? Calculating
derivatives with this approximation method won't do the trick for us --
we'll need something a little more powerful, which is what we will build
in this post. Admittedly, this idea didn't come to me through necessity
of such a system; I was just curious. It ended up being pretty
enlightening, and it's amazing how simple Haskell makes this type of
thing. Most of the code in this post didn't take too long to write, it's
relatively short (only 108 lines total, including whitespace) and has a
lot of capability. With that, let's get started!

#### The Data Type

I don't want to over-complicate the process of taking derivatives, so my
data type for algebraic expressions is kept minimal. We support six
different types of expressions:

- Variables (denoted by a single character)
- Constant values
- Addition
- Multiplication
- Exponentiation
- Division

This can be expanded upon, but this is adequate for the purpose of
demonstration.

Here is our `Expr a` type, with a sample expression representing $3x^2$:

```haskell
infixl 4 :+:
infixl 5 :*:, :/:
infixr 6 :^:

data Expr a = Var Char
             | Const a 
             | (Expr a) :+: (Expr a) 
             | (Expr a) :*: (Expr a)
             | (Expr a) :^: (Expr a)
             | (Expr a) :/: (Expr a)
             deriving (Show, Eq)

sampleExpr :: Expr Double
sampleExpr = Const 3 :*: Var 'x' :^: Const 2 --3x^2
```

Haskell allows infix operators to act as data constructors, which allows
us to express algebraic expressions cleanly and concisely without too
much mental parsing. Also note that, since we explicitly defined
operator precedence above the data type declaration, we can write
expressions without parentheses according to order of operations, like
we did in the sample expression.

#### The Algebra

Taking derivatives simply by the rules can get messy, so we might as
well go ahead and set up an algebra simplification function that cleans
up our final expressions for us. This is actually incredibly simple. As
long as you know algebraic laws, this kind of thing basically writes
itself in Haskell. We just need to pattern match against certain
expressions and meld them together according to algebraic law. This
function ends up being lengthy long due to the fact that symbolic
manipulation is mostly just a bunch of different cases, but we can
encode algebraic simplification rules for our above data type in a
straightforward way. The following simplify function takes an expression
and spits out a simpler one that means the same thing. It's really just
a bunch of pattern-matching cases, so feel free to skim it.

```haskell
simplify :: (Num a, Eq a, Floating a) => Expr a -> Expr a
simplify (Const a :+: Const b) = Const (a + b)
simplify (a       :+: Const 0) = simplify a
simplify (Const 0 :+: a      ) = simplify a

simplify (Const a :*: Const b) = Const (a*b)
simplify (a :*: Const 1)         = simplify a
simplify (Const 1 :*: a)         = simplify a
simplify (a :*: Const 0)         = Const 0
simplify (Const 0 :*: a)         = Const 0

simplify (Const a :^: Const b)       = Const (a**b)
simplify (a :^: Const 1)             = simplify a
simplify (a :^: Const 0)             = Const 1
simplify ((c :^: Const b) :^: Const a) = c :^: (Const (a*b))

simplify (Const a :*: (Const b :*: expr)) = (Const $ a*b) :*: (simplify expr)
simplify (Const a :*: expr :*: Const b) = (Const $ a*b) :*: (simplify expr)
simplify (expr :*: Const a :*: Const b) = (Const $ a*b) :*: (simplify expr)
simplify (Const a :*: (b :+: c))        = (Const a :*: (simplify b)) :+: (Const a :*: (simplify c))

simplify (Const 0 :/: a        ) = Const 0
simplify (Const a :/: Const 0)   = error "Division by zero!"
simplify (Const a :/: Const b)   | a == b = Const 1 -- only when a == b
simplify (a       :/: Const 1)   = simplify a

simplify (a :/: b)  = (simplify a) :/: (simplify b)
simplify (a :^: b)  = (simplify a) :^: (simplify b)
simplify (a :*: b)  = (simplify a) :*: (simplify b)
simplify (a :+: b)  = (simplify a) :+: (simplify b)
simplify x          = id x

fullSimplify expr = fullSimplify' expr (Const 0) -- placeholder
  where fullSimplify' cur last | cur == last = cur
                               | otherwise = let cur' = simplify cur
                                             in fullSimplify' cur' cur
```

I've also included a `fullSimplify` function that runs simplify on an
expression until the current input matches the last output of simplify
(which ensures an expression is completely simplified)\*. Note that in
the `simplify` function, I've covered a lot of bases, but not all of them.
Specifically, division simplification is lacking because it gets
complicated quickly and I didn't want to focus on that in this blog
post. We should also note that we don't have a data type expressing
subtraction or negative numbers, so we'll deal with that now. In order
to express the negation of expressions, we define the `negate'` function,
which basically multiplies expressions by $-1$ and outputs the resultant
expression.

```haskell
negate' :: (Num a) => Expr a -> Expr a
negate' (Var c)    = (Const (-1)) :*: (Var c)
negate' (Const a)  = Const (-a)
negate' (a :+: b)  = (negate' a) :+: (negate' b)
negate' (a :*: b)  = (negate' a) :*: b
negate' (a :^: b)  = Const (-1) :*: a :^: b
negate' (a :/: b)  = (negate' a) :/: b
```

Now we have a relatively robust system for computing symbolic
expressions. However, we aren't able to actually plug anything into
these expressions yet, so we'll fix that now. 

*\*Thanks to [/u/zoells](http://reddit.com/u/zoells) for the suggestion!*

#### Evaluating Expressions

The first thing we'll need to do to begin the process of evaluating
expressions is write a function to plug in a value for a specific
variable. We do this in terms of a function called `mapVar`, implemented
as follows:

```haskell
mapVar :: (Char -> Expr a) => Expr a -> Expr a
mapVar f (Var d)   = f d
mapVar _ (Const a) = Const a
mapVar f (a :+: b) = (mapVar f a) :+: (mapVar f b)
mapVar f (a :*: b) = (mapVar f a) :*: (mapVar f b)
mapVar f (a :^: b) = (mapVar f a) :^: (mapVar f b)
mapVar f (a :/: b) = (mapVar f a) :/: (mapVar f b)

plugIn :: Char -> a -> Expr a -> Expr a
plugIn c val = mapVar (\x -> if x == c then Const val else Var x)
```

`mapVar` searches through an expression for a specific variable and
performs a function on each instance of that variable in the function.
plugIn takes a character and a value, and is defined using `mapVar` to map
variables with a specific name to a constant provided by the user. Now
that we have `plugIn`, we can define two functions: One that takes an
expression full of only constants and outputs a result (`evalExpr'`), and
one that will replace a single variable with a constant and output the
result (`evalExpr`):

```haskell
evalExpr :: (Num a, Floating a) => Char -> a -> Expr a -> a
evalExpr c x = evalExpr' . plugIn c x 

evalExpr' :: (Num a, Floating a) => Expr a -> a
evalExpr' (Const a) = a
evalExpr' (Var   c) = error $ "Variables ("
                              ++ [c] ++ 
                              ") still exist in formula. Try plugging in a value!"
evalExpr' (a :+: b) = (evalExpr' a) + (evalExpr' b)
evalExpr' (a :*: b) = (evalExpr' a) * (evalExpr' b)
evalExpr' (a :^: b) = (evalExpr' a) ** (evalExpr' b)
evalExpr' (a :/: b) = (evalExpr' a) / (evalExpr' b)
```

What we're doing here is simple. With `evalExpr'`, we only need to replace
our functional types (`:+:`, `:*:`, etc) with the actual functions (`+`, `*`,
etc). When we run into a `Const`, we simply replace it with it's inner
number value. When we run into a `Var`, we note that it's not possible to
evaluate, and tell the user that there is still a variable in the
expression that needs to be plugged in. With evalExpr, we just plug in a
value for a specific variable before evaluating the expression. Simple
as that! Here are some examples of expressions and their evaluations:

```haskell
*Main> evalExpr 'x' 2 (Var 'x') 
> 2.0 
*Main> evalExpr 'a' 3 (Const 3:+: Var 'a' :*: Const 6)
>  21.0 
*Main> evalExpr 'b' 2 (Var 'b' :/: (Const 2 :*: Var 'b')) 
> 0.5 
```
We can even evaluate multivariate
expressions using `plugIn`: 
```haskell
*Main> evalExpr' . plugIn 'x' 1 $ plugIn 'y' 2 (Var 'x' :+: Var 'y') 
> 3.0 
```

Now that we've extended our symbolic
expressions to be able to be evaluated, let's do what we set out to do
-- find derivatives!

#### Derivatives

We aren't going to get into super-complicated derivatives involving
logorithmic or implicit differentiation, etc. Instead, we'll keep it
simple for now, and only adhere to some of the 'simple' derivative
rules. We'll need one of them for each of our six expression types:
constants, variables, addition, multiplication, division, and
exponentiation. We already know the following laws for these from
calculus: 


-------|---------------:  
Differentiation of a constant | $\frac{d}{dx}k = 0$  
Differentiation of a variable  | $\frac{d}{dx}x = 1$  
Addition differentiation       | $\frac{d}{dx}\left(f(x) + g(x)\right) = \frac{d}{dx}f(x) +\frac{d}{dx}g(x)$  
Power rule (/chain rule) | $\frac{d}{dx}f(x)^n = nf(x)^{n-1} \cdot \frac{d}{dx}f(x)$    
Product rule  | $\frac{d}{dx}\left(f(x) \cdot g(x)\right) = \frac{d}{dx}f(x) \cdot g(x) + f(x) \cdot \frac{d}{dx}g(x)$  
Quotient rule | $\frac{d}{dx}\frac{f(x)}{g(x)} = \frac{\frac{d}{dx}f(x) \cdot g(x)
- \frac{d}{dx}g(x) \cdot f(x)}{g(x)^2}$ 

As it turns out, we can almost directly represent this in Haskell. There should be no surprises
here -- following along with the above rules, it is relatively easy to
see how this function calculates derivatives. We will still error out if
we get something like $x^x$ as input, as it will require a technique we
haven't implemented yet. However, this will suffice for a many different
expressions.

```haskell
derivative :: (Num a) => Expr a -> Expr a
derivative (Var c)           = Const 1
derivative (Const x)         = Const 0

--product rule (ab' + a'b)
derivative (a :*: b)         = (a :*: (derivative b)) :+:  (b :*: (derivative a)) -- product rule

 --power rule (xa^(x-1) * a')
derivative (a :^: (Const x)) = ((Const x) :*: (a :^: (Const $ x-1))) :*: (derivative a)
derivative (a :+: b)         = (derivative a) :+: (derivative b)

 -- quotient rule ( (a'b - b'a) / b^2 )
derivative (a :/: b)         = ((derivative a :*: b) :+: (negate' (derivative b :*: a))) 
                               :/: 
                               (b :^: (Const 2))
derivative expr              = error "I'm not a part of your system!" -- unsupported operation

ddx :: (Floating a, Eq a) => Expr a -> Expr a
ddx = fullSimplify . derivative  

ddxs :: (Floating a, Eq a) => Expr a -> [Expr a]
ddxs = iterate ddx

nthDerivative :: (Floating a, Eq a) => Int -> Expr a -> Expr a
nthDerivative n = foldr1 (.) (replicate n ddx)
```

So, what can we do with this? Well, let's take a look:

The first, most obvious thing we'll note when running the derivative
function on an expression is that what it produces is rather ugly. To
fix this, we'll write a function ddx that will simplify the derivative
expression three times to make our output cleaner. (Remember `sampleExpr`
= $3x^2$) 

```haskell
*Main> derivative sampleExpr 
> Const 3.0 :*: ((Const 2.0 :*: Var 'x' :^: Const 1.0) :*: Const 1.0) :+: Var 'x' :^: Const 2.0
*Main> ddx sampleExpr 
> Const 6.0 :*: Var 'x' -- 6x
```
Another thing we can do is get a list of derivatives. The `iterate` method
from the `Prelude` suits this type of thing perfectly -- we can generate a
(infinite!) list of derivatives of a function just by calling `iterate
ddx`. Simple, expressive, and incredibly powerful.

```haskell
*Main> take 3 $ ddxs
sampleExpr [Const 3.0 :*: Var 'x' :\^: Const 2.0,Const 6.0 :*: Var 'x',Const 6.0] -- [3x\^2, 6x, 6] 
*Main> let ds = take 4 $ ddxs sampleExpr
*Main> fmap (evalExpr 'x' 2) ds 
> [12.0,12.0,6.0,0.0] 
```

We're also able to grab the $n^{th}$ derivative of an expression. We could
simply grab the $n^{th}$ term of `ddxs`, but we'll do it without the
wasted memory by repeatedly composing `ddx` $n$ times using `foldr1` and
`replicate`. 

```haskell
*Main> nthDerivative 2 sampleExpr 
> Const 6.0
```

There's one
last thing I want to touch on. Since it's so simple to generate a list
of derivatives of a function, why not use that to build functions'
Taylor series expansions?

#### Taylor Series Expansions

The Taylor series expansion of a function $f(x)$ about $a$ is defined as
follows: $$ \sum{n=1}^{\infty} \frac{f^{(n)}(a)}{n!} \cdot (x -
a)^n $$ The Maclaurin series expansion for a function is the Taylor
series of a function with a = 0, and we will also implement that. Given
that we can:

1. Have multivariate expressions
2. Easily generate a list of derivatives

We can actually find the Taylor series expansion of a function easily.
Again, we can almost directly implement this function in Haskell, and
evaluating it is no more difficult.

```haskell
taylor :: (Floating a, Eq a) => Expr a -> [Expr a]
taylor expr = fmap fullSimplify (fmap series exprs)
  where indices = fmap fromIntegral [1..]
        derivs  = fmap (changeVars 'a') (ddxs expr)
          where changeVars c = mapVar (\_ -> Var c)
        facts   = fmap Const $ scanl1 (*) indices
        exprs   = zip (zipWith (:/:) derivs facts) indices -- f^(n)(a)/n!
        series (expr, n) = 
          expr :*: ((Var 'x' :+: (negate' $ Var 'a')) :^: Const n) -- f^(n)(a)/n! * (x - a)^n

maclaurin = fmap (fullSimplify . plugIn 'a' 0) . taylor 

evalTaylorWithPrecision a x prec =  
  sum . map (evalExpr' . plugIn 'x' x . plugIn 'a' a) . take prec . taylor

evalTaylor a x = evalTaylorWithPrecision a x 100
        
evalMaclaurin = evalTaylor 0
evalMacLaurinWithPrecision = evalTaylorWithPrecision 0
```

To produce a Taylor series, we need a couple of things:

-   A list of derivatives
-   A list of indices
-   A list of factorials

We create these three things in where clauses in the taylor declaration.
indices are simple, `derivs` calculates a list of derivatives (using
`mapVar` again to change all variables into $a$s), and `facts` contains our
factorials wrapped in `Const`s. We generate a list of `(expression, index)`s
in `exprs`, and then map the "gluing" function `series` over `exprs` to
produce a list of expressions in the series expansion. We then `fmap
superSimplify` over the list in order to simplify down our expressions,
and we get back a list of Taylor series terms for the given expression.
The Maclaurin expansion can be defined as mentioned above in terms of
the Taylor series, and again, we basically directly encode it (though we
do have to re-simplify our expressions due to the plugging in of a
variable). Let's take a look at the Taylor expansion for $f(x) = x$. We
note that: $f(a) = a$,  $f'(a) = 1$,  $f''(a) = 0$, and the rest of the
derivatives will be 0. So our Taylor series terms $T_n$ should look
something like: $T_1 = \frac{a}{1} \cdot (x - a) = a \cdot (x-a)$
$T_2 = \frac{1}{2} \cdot (x-a)^2$ $T_3 = \frac{0}{6} \cdot
(x-a)^3 = 0$ ...and so on. Let's take a look at what `taylor` produces:

```haskell
*Main> take 3 $ taylor (Var 'x') 
> [Var 'a' :*: (Var 'x' :+: Const (-1.0) :*: Var 'a'), 
--^ a * (x-a) 
(Const 1.0 :/: Const 2.0) :*: (Var 'x' :+: Const (-1.0) :*: Var 'a') :^: Const 2.0, 
--^ 1/2 * (x-a)^2 
Const 0.0] 
```
This matches what we determined earlier. To evaluate
a Taylor expression, we need a value for $a$, a value for $x$, and a
specified number of terms for precision. We default this precision to
100 terms in the `evalTaylor` function, and the logic takes place in the
`evalTaylorWithPrecision` function. In this function, we get the Taylor
expansion, take the first `prec` terms, plug in `a` and `x` for all values of
the function, and finally sum the terms. Maclaurin evaluation is again
defined in terms of Taylor evaluation.

Taking a look at the above Taylor series expansion of $f(x) = x$, there
is only one term where a zero-valued $a$ will produce any output (namely
$\frac{1}{2} \cdot (x-a)^2$). So when we evaluate our Maclaurin
series for this function at x, we should simply get back
$\frac{1}{2}x^2$. Let's see how it works: 

```haskell
*Main> evalMaclaurin 2 (Var 'x') 
> 2.0 --1/2 2\^2 = 1/2 * 4 = 2 
*Main> evalMaclaurin 3 (Var 'x') 
> 4.5 -- 1/2 * 3^2 = 1/2 * 9 = 4.5 
*Main> evalMaclaurin 10 (Var 'x') 
> 50.0 -- 1/2 * 10^2 = 1/2 * 100 = 50 
```

Until next time,

Ben