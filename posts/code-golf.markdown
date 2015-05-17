---
title: Code Golf
author: Ben Kovach
---

I've always been interested in [Code
golf](http://en.wikipedia.org/wiki/Code_golf), which essentially boils
down to creating the shortest possible program that completes some task.
I finally got around to completing one of these challenges, and though I
wasn't really that close to winning, it was fun to mess around with it and
it was one of the shorter
[Haskell](http://en.wikipedia.org/wiki/Haskell_%28programming_language%29)
solutions! 

The challenge I attempted was [Reddit's Tiny Code
Challenge
\#4](http://www.reddit.com/r/tinycode/comments/x41tf/how_about_a_challenge_4_find_the_longest_common/),
asking for the shortest program that finds the longest identical
subsequence in a string. Directly from the challenge page, for example:

```
aaabbbbccc aabccdcdd ->  aab, bcc  
adeaabbbbccc aabccdedcdd -> aab, bcc  
abcdef ghijk -> nothing    
abcdef fghijk -> f
```

So, here was my solution in Haskell:

```haskell
import Data.List
import System.Environment
f=concat.r
r []=[[]]
r s=(scanr(:)[]s):(r$init s)
g [a,b]=snd.maximum.map(\x->(length x,x))$intersect(f a)(f b)
main=getArgs>>= \z-> print.g$z 
```

Let's break this down into steps, since it probably looks like a bunch
of mumbo-jumbo!

```haskell
import Data.List 		-- gets us the function "intersect"
import System.Environment	-- gets us the function "getArgs"

f=concat.r 			-- calls "r" on some input and flattens the results (concat) 

{- The function "r" takes a list of elements and produces all of it's possible sublists.
Base case: If the input list is empty, return an empty list containing a single empty list. -}
r []=[[]]			-- base case for "r"
							
r s= (scanr(:)[]s) 		{-  recursively build substrings from the list "s" 
				and push them to an empty list                  -}
     :				-- Append the list we just built to...
     (r$init s)     		-- "r" called on the first n-1 elements of of the current list "s"

g [a,b]=snd.maximum.		-- finds the maximum by the length of strings
	map(\x->(length x, x))	-- groups lists into tuples of their lengths and the list
	$intersect(f a)(f b) 	-- The intersection* of "f" called on a, and "f" called on b
		
main= getArgs >>= 		-- get the command line arguments as a list ["firstString", "secondString"]
      \z-> print.g$z 		-- bind the argument list (z) to print "g" called on z
```

And there you have it! 193 characters of Haskell code got us a pretty
cool program. Here's a little bit of output:

```bash
>tc04.exe "aaabbbbccc" "aabccdcdd"
>"bcc"
>tc04.exe "abcdef" "ghijk"
>tc04.exe "abcdef" "fghijk"
>"f"
>tc04.exe "fhqwhgads" "where are all the dragons?"
>"wh"
```
Cool! :) -Ben
