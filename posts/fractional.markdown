---
title: Implementing a fractional data type and investigating the expansion of the square root of 2
author: Ben Kovach
---

A couple of weeks ago, I completed [Project Euler \#57: Investigating
the Expansion of the Square Root of
2](http://projecteuler.net/problem=57). I found the problem really
interesting, since it required me to write up my own fractional
operations (addition, division, etc.) 

Today, I decided that I wanted to
take my minimally defined fractional library and make it a more
full-featured one. First, I'll first walk through the building of the
data type and implementing fractional operations, and then I'll get to
my solution of the Project Euler problem. 

First thing's first. A
`Fraction` is defined as a numerator and a denominator. My fractions
consist of only Integers, and here's how the data type is defined:

```haskell
data Fraction = Frac Integer Integer -- Numerator Denominator

instance Show Fraction where
  show (Frac a b) = (show a) ++ " / " ++ (show b)
```
I wanted fractions to show up in the form `x / y`, so that is what the
instance of `Show` I defined does. So, now that we have a data type,
what can we do with it? 

Here's a list of common functions that I think
are necessary in the definition of a fraction: Addition, Subtraction,
Multiplication, Division, Exponentiation, Simplification, Negation,
getting the Numerator, and getting the Denominator. Let's start with the
simple ones:

```haskell
num :: Fraction -> Integer
num (Frac a _) = a

denom :: Fraction -> Integer
denom (Frac _ b) = b
```

Those are are `numerator` and `denominator` functions. We simply
take the first or second argument of the `Frac` in order to get the
parameter that we need, and disregard the other one (using `_`) since
it's unused. Simplifying is also simple enough. We just need to divide
the numerator and the denominator by the greatest common divisor of the
two. Since Haskell gives us a `GCD` function, this is pretty darn
simple. 

Here's how it's implemented:

```haskell
simplify :: Fraction -> Fraction
simplify (Frac a b) = Frac (a `quot` factor) (b `quot` factor)
	where factor = gcd a b
```

Easy! We just make a new fraction out of the divided values. The
function`quot` is basically just integer division that truncates
the result towards 0. The second half of that is unimportant in this
instance, since we're dividing by the `GCD` of the numbers and the
result will always be an integer value. Okay, so we have a couple of
functions. Great! But, what about implementing addition, subtraction,
etc? 

Well, basically what we want to do here is define a couple of
instances of numeric types for our Fractional data type. The first
instance we need to derive is `Num`, which has a few operations that
we want:

```haskell
instance Num Fraction where
  (-) f (Frac a b)          = f + (Frac (-a) b)
  (+) (Frac a b) (Frac c d) = Frac num denom
    where denom = lcm b d
          num   = a * (denom `quot` b) + c * (denom `quot` d)
  (*) (Frac a b) (Frac c d) = Frac (a*c) (b*d)
  negate (Frac a b)         = Frac (-a) b
  abs f                     = fmapF abs f
    where fmapF f (Frac a b) = Frac (f a) (f b)
  fromInteger x             = Frac x 1
  signum (Frac a b)         = if a == 0 then 0
                              else  if b > 0 then
                                    if a < 0 then (-1)
                                    else 1
                              else  if a < 0 then 1
                                    else (-1)
```

The three "common" operators (`+`,`-`,`*`) are defined here, which
means that we can now evaluate expressions such as `Frac 1 2 * Frac 1
2`. Cool, right? Those three operations are fairly self-explanatory,
and the code (hopefully) isn't too tough to follow. There are also three
other function definitions here, that maybe aren't quite as clear. The
function `negate` simply turns a negative fraction positive, or a
positive fraction negative. The function `abs` takes the absolute
value of the fraction. This is fairly simple; we just use a function
that maps `abs` (Integer absolute value) over the numerator and
denominator of the fraction. The last is `signum`, which looks
probably the most complicated, but all it actually does is tells us
whether the `Fraction`is less than, greater than, or equal to 0
(returning -1, 1, and 0, respectively). Cool, so since we got all of
those functions out of `Num`, where can we find the rest? We're
missing `/`, so we'll make our `Fraction` an instance of
`Fractional`. Seems appropriate, right?

```haskell
instance Fractional Fraction where
  (/) f             = (*) f . recip
  recip (Frac a b)  = Frac b a
  fromRational r    = Frac (numerator r) (denominator r)
```

Cool, we get division out of that, which is simple enough! We just take
the reciprocal of the second fraction, and multiply the two. This may
look a little funky, so I'll explain that last. The other two functions
defined here are `recip` and `fromRational`. `recip` simply flips
the numerator and denominator in a `Fraction`, and this is easily
handled with pattern matching. `fromRational` takes a rational number
(which is provided the `numerator` and `denominator` functions) and
turns it into a `Fraction`. Knowing what the `numerator` and
`denominator` functions are, this function is incredibly trivial.

Okay, so about that division function. Our division appears to take only
one argument, but it actually takes two. `(*) f f'` is just syntactic
sugar for `f * f'`. We want to compose the function `f` with the
function `recip f'`, so we use the function `(*)`, apply it to
`f`, and then apply that to the function `recip`, which is then
called on the second argument of the function.

Alright! We've got plenty of functions at our
disposal now, so what's next? Well, we want to be able to compare
`Fractions`, so let's go ahead and make it an instance of `Eq` and
`Ord`, which allow us to check equivalency and order, respectively.

```haskell
instance Eq Fraction where
  (/=) f    = not . (==) f
  (==) f f' = (x == x') && (y == y')
      where (Frac x y) = simplify f
            (Frac x' y') = simplify f'
  
instance Ord Fraction where
  compare (Frac a b) (Frac c d) = compare (a `quot` b) (c `quot` d)
  (<)  f    = (==) LT . compare f
  (>)  f    = (==) GT . compare f
  (>=) f    = not . (<) f
  (<=) f    = not . (>) f
  max  f f' = if f < f' then f' else f
  min  f f' = if f < f' then f else f'
```

There's a lot of functions that are similar in structure to our `/`
function from earlier, so understanding what is happening with those
will make these much easier to understand. Starting with `Eq`, we have
two functions, `/=` (not equals) and `==`. `==` simply checks to
see if the simplified version of `f` and the simplified version of
`f'` have the same numerators and denominators. `/=` basically just
returns the opposite of `==` by calling `not` on the result. `Ord`
isn't too tough either. 

First we have `compare`, which returns a
comparator (`LT`, `GT`, or `EQ`) based on the relationship between
two `Fractions`. The inequality functions are all designed around this
function. The `max` and `min` functions are simple, and designed
around our inequality functions. So, what's left? I decided I wanted to
experiment, so I decided to also make `Fraction` an instance of
[Monoid](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids)
and give `Fraction` a simpler constructor. Here's the rest!

```haskell
instance Monoid Fraction where
  mempty  = 0
  mappend = (+)
  mconcat = foldr mappend mempty

(%) :: Integer -> Integer -> Fraction
(%) a = simplify . Frac a
```

The instance of `Monoid` defines a couple of functions: `mempty`,
`mappend`, and `mconcat`. `mempty` is the minimal value for the
`Monoid`.I chose 0 (which gets automatically boxed into a
`Fraction`). `mappend` is a combiner function, and I thought that
addition of fractions fit this bill well enough, so I simply gave it an
alias. `mconcat` concatenates a list of fractions with some function,
and in our case, sums the entire list. Our type constructor (`%`)
takes two integers and boxes them up into a `Fraction`, which is then
simplified. Easy enough. One final note on all of this. You may have
noticed that exponentiation (`^`) isn't implemented here. But, it
actually is! Turns out, any data type that has a `Num` instance
defined (like our `Fraction`) can use the `^` operator. So things
like `(1 % 2)^2` actually get evaluated properly. (In this case, to
`1 % 4`). 

All right! Let's use this small library to solve [Project Euler
\#57](http://projecteuler.net/problem=57)! Knowing how to use the
`Fraction` library, this should be relatively easy to follow. Here we
go:

```haskell
import Fraction

main = return . length . filter moreInNum . map sqrtTwo $ [1..1000]
  where moreInNum f = length ( (show . num) f ) > length ( (show . denom) f)
        sqrtTwo = simplify . (+) 1 . sqrtTwo'
          where sqrtTwo' 1 = 1 % 2
                sqrtTwo' n = 1 / ( 2 + sqrtTwo' (pred n) )
```

Fairly simple. We directly implement the function and run it on each
iteration, building a list as we go. We then filter our list so that the
only entries left are the ones that have more digits in the numerator
than the denominator. Lastly, we evaluate the length of that list, which
gives us the answer.

[View the full Fraction.hs source on lpaste.](http://lpaste.net/97144)

Until next time!

Ben