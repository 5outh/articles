---
title: 99 Haskell Problems #13: Run-length Encoding
author: Ben Kovach 
tags: haskell, problems
---

I've been working on The Ninety-Nine Haskell Problems lately, and I've come across a particular one that seems to be practical, and was a lot of fun to implement!  


The 99 Problems start out with a lot of list-based exercises, and eventually you're asked to build up to a point where you can implement a function that performs Run-length encoding on a String. Basically what this means is, if we have a string, say "aabbcccd", we'd group together the adjacent, identical characters, count them, and then output the characters along with their counts in a new list.

Thus, the previous example `"aabbcccd"` would output something like this:

```
$ a2b2c3d
```

A couple of the 99 Problems ask you to implement this indirectly, meaning, actually group the identical characters into sub-lists and count the length of those lists. This is fairly trivial, making use of Haskell's built-in function group, which takes a list and separates it in exactly the manner we would need in order to get sub-lists of adjacent identical values.

The real fun came in Problem 13, where it asks for a direct implementation of the run-length encoding algorithm, meaning, we're no longer allowed to split up the groups in the way that we were before. Additionally, the problem asks that we use a data wrapper on each ending value so we are able to discern Single values (example in above: `d`) vs Multiple values (ex: `a2`).

Here's what I came up with:

```haskell
data Count = Multiple Int Char 
           | Single Char 
            deriving (Eq, Show)
			
encodeDirect :: [Char] -> [Count]
encodeDirect = map toCount . foldr encode []
  where toCount (x, y) = case x of
                          1 -> Single y
                          _ -> Multiple x y
        encode x [] = [(1, x)]
        encode x (y@(a, b):ys)
          | b == x = (a+1, b):ys
          | otherwise = (1, x):y:ys
```


Let's take a look at this.

So, first I defined a new data type called `Count` that can be either a `Single` that contains a character, or a `Multiple` that contains a character and it's count (as an `Int`)

What `encodeDirect` does is actually parses a list of `Char`s (which, in Haskell, can be represented as a `String`, like `"aabbcccd"`) into a list that looks something like this:

```haskell
[Multiple 2 a, Multiple 2 b, Multiple 3 c, Single d]
```

The procedure is as follows:

First, we use encode every character in our input set by using a `foldr` and an `encode` function in order to get a list that contains 2-tuples with our character as the second value, and its count as the first. `(   [(2, 'a'), (2, 'b') ... ]   )`

The encode function might look a little cryptic, but broken down, it makes sense. First, if we have an empty resultant list of `Count`s, we need to put the first character we encounter in the resultant list. Once we have a resultant list (no matter how small), we are able to encode the other values by following some logic:

1. If the first character value in the resultant list matches what we are currently trying encode, we simply need to step up the count on that entry.

2. Otherwise, we need to add the new character to the front of the list with a count of 1.

When the `foldr` is complete, we have a nice list that looks just like how I described previously.

Now, to get everything wrapped up into a `Count` declaration, we simply `map` the `toCount` function over the list we just generated. The `toCount` function takes one of our 2-tuple values and creates a `Single` value out of it if its count is equivalent to 1, otherwise it packs it into a `Multiple` value.

Once this is mapped, we have our resultant list!

```haskell
$ encodeDirect "aabbcccd" = [Multiple 2 a, Multiple 2 b, Multiple 3 c, Single d] 
```

PS:I picked up a bit of new syntax from a video I watched last night, located here:  [Sokoban Live Coding](http://www.youtube.com/watch?v=mtvoOIsN-GU). I learned a lot from it. If you're interested in Haskell, check it out.

Until next time!


5outh