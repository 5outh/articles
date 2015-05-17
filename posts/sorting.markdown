---
title: Functional Sorting in Haskell
author: Ben Kovach
---

Sorting is a fairly major topic of study when it comes to programming,
and there are [tons of ways to do
it.](http://en.wikipedia.org/wiki/Sorting_algorithm) I don't know how
interesting these algorithms are to other people, but they have always
been an interest of mine. I think they're neat because they illustrate
that there can be several ways to tackle even the simplest of tasks
(which, in my opinion, is one of the things that makes programming so
fun).


Since I'm always looking for new things to try, I've decided to take a
few of the common sorting algorithms and implement them on my own in
[Haskell](http://www.haskell.org/haskellwiki/Haskell). For those of you
who aren't familiar with it, here's a brief overview.


Most programming languages (Java, C++, Python, Ruby, Javascript, etc)
are defined as *imperative*, which basically means that programs are
executed line-by-line. Many of the aforementioned languages are also
defined as *object-oriented*, meaning that virtually everything is a
part of an *object*. I won't get into the details about
*[OOP](http://en.wikipedia.org/wiki/Object-oriented_programming)* here,
since we're talking about Haskell, but you can read about it if you'd
like. Haskell differs from these languages by conforming to a paradigm
in which everything is defined as a *function*. Objects are not present
here, and iterative steps don't quite work in the same way -- nothing is
being executed line-by-line -- all instructions are defined in
functions. This is a rather mind-blowing concept, but it's extremely
cool once you can get a mental grasp on it.


Back to the point of the blog post. I'll be covering how to implement
three sorting algorithms in a functional manner: [Bubble
Sort](http://en.wikipedia.org/wiki/Bubble_sort), [Insertion
Sort](http://en.wikipedia.org/wiki/Insertion_sort), and finally,
[Quicksort](http://en.wikipedia.org/wiki/Quicksort). If you don't have a
background in Haskell, this might all come off as programming jargon,
but if you're looking for an interesting new look at these algorithms
you've seen a billion times implemented imperatively, or are interested
in picking up Haskell as a hobby or something, I'd recommend reading on.


#### Bubble Sort
The bubble sort algorithm is as follows:

1. Take an array's first two elements (we'll call the first one x, and
the second one y)
2. If x is less than y, leave those elements alone. If not, swap y with
x, so that the array now reads
[y, x, (the rest of the array)]
3. Repeat this for each element in the array, swapping as it goes along.
4. Check to see if the list is sorted. If not, rerun the algorithm on
the result of the last iteration of the function.
5. When list is sorted, return the result.
Here it is in Haskell:

```haskell
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs) = if sorted thisSort then thisSort else bubbleSort thisSort
    where thisSort = (min x y) : bubbleSort ((max x y):xs)

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False
```

Rather long, but effective. Bubble Sort isn't the most efficient sorting
algorithm, especially since it has to do all of that checking if it's
already sorted. I'm not a big fan of this one. 

#### Insertion Sort
Insertion sort is pretty simple to follow. We start with an array, as
always, and then follow these steps: 

1. Pop the first element of the
array off of the array, and populate a new array with it. 
2. For each
element after the first in the array, insert it at it's proper sorted
location in the array. 
3. Return the final list of sorted numbers. Let's
see what this looks like in Haskell:

```haskell
import Data.List

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert [] 
```

How elegant is that? This could take many lines in an imperative
language, but we make use of Haskell's recursive `foldr` method to
populate our sorted list, and the appropriately named function `insert`
taken from the [Data.List](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-List.html) package helps us to populate it in the
exact manner we're looking for. 

#### Quicksort 
The Quicksort algorithm
can be pretty intimidating, especially at first glance. This is because
it makes use of
[recursion](http://en.wikipedia.org/wiki/Recursion_(computer_science))
to accomplish it's sorting, which is a particularly tough concept to
fully understand when it comes to programming. The instructions for
Quicksort look a little something like this: 

1. Choose any point in the
array as a "pivot" point. 
2. Create two new arrays, one populated with
the numbers in the array less than the pivot, and another with the
numbers greater than the pivot. 
3. Recursively call Quicksort on both of
the lists, until these lists (inevitably) turn up empty, at which point
the function returns an empty list. 
4. At this point, the recursion
unwinds itself, and we concatenate the recursively sorted arrays
together. 

(I would recommend [reading up on this
algorithm](http://en.wikipedia.org/wiki/Quicksort) if the above
instructions do not make sense; I am not the best at explaining things)
Sounds complicated, right? Here's a Haskell implementation:


```haskell
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs)  = quickSort smaller ++ [x] ++ quickSort larger
    where smaller = filter (<=x) xs
          larger  = filter (> x) xs
```

What? It only takes *five lines* to implement Quicksort? This is why
Haskell is so cool to me. Compare this with something like, say, a
[Java](http://gauss.ececs.uc.edu/Courses/C321/html/quicksort.java.html)
implementation, and you'll see what I mean. It's just so simple! We're
simply defining `quickSort` as a quickSorted list of smaller numbers
concatenated with a singleton list containing a pivot, concatenated with
a quickSorted list of larger numbers. Easy to read, though the algorithm
may seem pretty complicated at first glance. 

Hope you enjoyed the read! 

-Ben