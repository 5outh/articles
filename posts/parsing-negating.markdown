---
title: Parsing and Negating Boolean Strings in Haskell
author: Ben Kovach
tags: parsing, negating, boolean
---

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
 
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token hiding (parens)
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative hiding ((<|>))
import Control.Monad
import Prelude hiding (not)
```

It appears that [the dailyprogrammer
subreddit](http://www.reddit.com/r/dailyprogrammer/) is back after a
pretty long hiatus, and they kicked back into gear with a really
interesting problem. The problem was, paraphrasing:

> Given a Boolean expression as a string S, compute and print the
> negation of S as a string using DeMorgan's laws.

[The problem is also detailed in full
here](http://www.reddit.com/r/dailyprogrammer/comments/1qira9/111213_challenge_135_intermediate_de_morgans_law/).
I completed the challenge and posted my solution to reddit, but wanted
to share it here as well, so here it is, with slight modifications: 

This
is a problem that is heavily suited to three major things that Haskell
advocates: Algebraic Data Types, Pattern Matching, and Monadic Parsing.
First off, if you've had any experience with automata theory, it's
pretty clear that the input language of Boolean expressions can be
represented by a [context free
grammar](http://en.wikipedia.org/wiki/Context-free_grammar). It just so
happens that Haskell makes it incredibly easy to model CFGs right out of
the box using Algebraic Data Types. 

Let's take a look at this data type
representing Boolean expressions:

```haskell
data Expr = Not Expr 
          | And Expr Expr 
          | Or Expr Expr 
          | Var Char 
          | SubExpr Expr 
          deriving Eq
```

Simple. Now, the main problem of this challenge was actually performing
the simplification of the not operation. Using pattern matching, we can
*directly encode these rules* in the following manner:

```haskell
not :: Expr -> Expr
not (Not e)     = e
not (And e1 e2) = Or (not e1) (not e2)
not (Or e1 e2)  = And (not e1) (not e2)
not (Var c)     = Not (Var c)
not (SubExpr e) = not e
```

Here we're giving a literal definition of rules for negating Boolean
expressions. If you use Haskell, this is really easy to read. If you
don't: stare at it for a second; you'll see what it's doing! That's the
brunt of the challenge, right there. That's it. Encode a Boolean
expression into an `Expr` and call `not` on it, and it will spit out a
new `Expr `expressing the negation of your original expression.
DeMorgan's laws are represented in the `And` and `Or` rules.We can also
do this in a slightly modified way, using a function `simplify :: Expr
-> Expr` that simplifies expressions and another function `not = simplify
. Not` to compute the same thing. It's a similar solution so I won't
post it, but if you'd like to, feel free to experiment and/or add more
simplification rules (e.g. `simplify e@(And a b) = if a == b then a else
e`). We can also display our expressions as a string by declaring `Expr` an
instance of `Show` in the following way:

```haskell
instance Show Expr where
  show (Not e)     = "NOT " ++ show e
  show (And e1 e2) = show e1 ++ " AND " ++ show e2
  show (Or e1 e2)  = show e1 ++ " OR "  ++ show e2
  show (Var c)     = [c]
  show (SubExpr e) = "(" ++ show e ++ ")"
```

Now we can type in Boolean expressions using our data type, not them,
and print them out as nice expressions. But, now we are faced with, in
my opinion, the tougher part of the challenge. We're able to actually
compute everything we need to, but what about parsing a Boolean
expression (as a string) into an `Expr`? We can use a monadic parsing
library, namely Haskell's beloved
[Parsec](http://www.haskell.org/haskellwiki/Parsec), to do this in a
rather simple way. We'll be using Parsec's
[Token](http://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec-Token.html)
and
[Expr](http://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec-Expr.html)
libraries, as well as the base, in this example. Let's take a look.

```haskell
parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""
  where expr      = buildExpressionParser operators term <?> "compound expression"
        term      =  parens expr <|> variable <?> "full expression"
        operators = [ [Prefix (string "NOT" >> spaces >> return Not)]
                    , [binary "AND" And]
                    , [binary "OR" Or] ]
          where binary n c = Infix (string n *> spaces *> pure c) AssocLeft
        variable = Var     <$> (letter <* spaces)                              
                           <?> "variable"
        parens p = SubExpr <$> (char '(' *> spaces *> p <* char ')' <* spaces) 
                           <?> "parens"
```

We essentially define the structure of our input here and parse it into
an Expr using a bunch of case-specific parsing rules. `variable` parses
a single `char` into a `Var`, `parens` matches and returns a `SubExpr`,
and everything else is handled by using the convenience function
`buildExpressionParser` along with a list of operator strings, the types
they translate to and their operator precedence. Here we're using
applicative style to do our parsing, but monadic style is fine too.
[Check this out for more on applicative style
parsing](http://www.serpentine.com/blog/2008/02/06/the-basics-of-applicative-functors-put-to-practical-work/).


Given that, we can define a `main` function to read in a file of
expressions and output the negation of each of the expressions, like so:

```haskell
main = mapM_ printNotExpr . lines =<< readFile "inputs.txt"
  where printNotExpr e = case parseExpr e of
                          Right x -> print $ not x
                          Left  e -> error $ show e
```

Concise and to the point. We make sure that each line gets parsed
properly, not the expressions, and print them. Here's what we get when
we run the program:

```
inputs.txt                             --- output

a                                      --- NOT a
NOT a                                  --- a
a AND b                                --- NOT a OR NOT b 
NOT a AND b                            --- a OR NOT b
NOT (a AND b)                          --- a AND b
NOT (a OR b AND c) OR NOT(a AND NOT b) --- (a OR b AND c) AND (a AND NOT b)
```

[Finally, here's the full source on
Github](https://gist.github.com/5outh/7452588#file-demorgan-hs). 

Thanks for reading!