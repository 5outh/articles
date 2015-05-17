---
title: Uncountable Infinity & Cantor's Diagonalization
author: Ben Kovach
---

#### Introduction

I took a class on Discrete Mathematics this past semester, and one of
the topics we covered very briefly was the concept of countable and
uncountable infinity. I didn't end up getting a great grasp on it,
however, and the topic I'm writing about today didn't make a lick of
sense to me at the time. It was never covered on exams, so I never had
to revisit the idea. But, I've been reading through Douglas R.
Hofstadter's *[Gödel, Escher,
Bach](http://en.wikipedia.org/wiki/G%C3%B6del,_Escher,_Bach)*, and it
just so happens that the topic is covered in the book. As it turns out,
it's not actually too difficult a thing to understand, so I'm hoping to
relay the ideas in this blog post. If you've heard about the concept of
multiple infinities but not understood it, hopefully this will shed some
light! If you haven't yet heard about the concept of multiple
infinities, enjoy the read! Anyway, on with the show...

#### Natural Numbers Form an Infinite Set

It is a widely known fact ([axiom of ZFC Set
Theory](http://en.wikipedia.org/wiki/Axiom_of_infinity)) that the set of
natural numbers (all non-negative integers) extends on to infinity. That
is to say, there is no "largest" natural number. We can order the set of
natural numbers in this way: $$ \begin{matrix} N\_0 & = & 0 \cr N\_1 &
= & 1 \cr N\_2 & = & 2 \cr N\_3 & = & 3 \cr N\_4 & = & 4 \cr N\_5 &
= & 5 \cr \cdots \end{matrix} $$ This ordering comes very naturally
to us. The nth term of the set of natural numbers will be, simply, n.
Because of this fact, we can tell that this set of natural numbers
contains all of them -- if you give me any natural number, it will
correspond to the nth number in the set. There is a similar way of
ordering the integers (including negatives) in this fashion. It may not
be heavily intuitive at first, because we have to extend in two
directions rather than one. But, it is apparent that this ordering of
integers will allow us to find the location of any integer in it: $$
\begin{matrix} Z\_0 & = & 0 \cr Z\_1 & = & 1 \cr Z\_2 & = & -1 \cr
Z\_3 & = & 2 \cr Z\_4 & = & -2 \cr Z\_5 & = & 3 \cr \cdots
\end{matrix} $$ Any integer, positive or negative, is contained in this
infinite set. An infinite set that can be ordered in a way such as this
is said to be *countably infinite.*

#### What About Real Numbers?

For the purposes of this post, we're going to focus on the real numbers
between 0 and 1. We can represent them in the following way: $$
\begin{matrix} R\_0 & = & 0 . & a\_0 & a\_1 & a\_2 & a\_3 & a\_4 &
\cdots \cr R\_1 & = & 0 . & b\_0 & b\_1 & b\_2 & b\_3 & b\_4 & \cdots
\cr R\_2 & = & 0 . & c\_0 & c\_1 & c\_2 & c\_3 & c\_4 & \cdots \cr
R\_3 & = & 0 . & d\_0 & d\_1 & d\_2 & d\_3 & d\_4 & \cdots \cr \cdots
\end{matrix} $$ We can see that this set goes on forever, that is,
extends infinitely, just as the set of integers and naturals does.
However, the set of real numbers is "built" in a different way. Both of
these facts are important in what we will observe next.

#### Cantor's Diagonalization Method

I claim now that I can produce a number that the above set does not
contain. To do this, I will be using Georg Cantor's famous
[Diagonalization
Method](http://en.wikipedia.org/wiki/Cantor%27s_diagonal_argument).
Here's how it works. First, I will grab the $n$th term of each $n$th
element in our set of real numbers to compose a new number, like so: $$
\begin{matrix} R\_0 & = & 0 . & \color{red}{a\_0} & a\_1 & a\_2 & a\_3
& a\_4 & \cdots \cr R\_1 & = & 0 . & b\_0 & \color{red}{b\_1} & b\_2
& b\_3 & b\_4 &\cdots \cr R\_2 & = & 0 . & c\_0 & c\_1 &
\color{red}{c\_2} & c\_3 & c\_4 & \cdots \cr R\_3 & = & 0 . & d\_0 &
d\_1 & d\_2 & \color{red}{d\_3} & d\_4 & \cdots \cr \cdots &&& &&&&
\color{red}{\ddots} \end{matrix} $$ So my new number becomes: $$
\begin{matrix} 0 . & a\_0 & b\_1 & c\_2 & d\_3 & \cdots \end{matrix}
$$ Now I'm going to perform some transformation on each digit of this
number to produce a new digit. A simple way (the simplest way?) to do
this would just be to add 1 to each digit. This will produce a new
number as follows: $$ \begin{matrix} M\_0 & = & 0 . & a\_0 + 1 & b\_1 +
1 & c\_2 + 1 & d\_3 + 1 & \cdots \end{matrix} $$ We can see $M\_0$
cannot be the same as $R\_0$ because its first term differs. Same goes
for $R\_1$ with its second digit, and so on, *ad infinitum*. Therefore,
each element of set of real numbers between 0 and 1 is going to differ
from $M\_0$ by at least one digit. We can conclude from this observation
that our (infinite!) set of real numbers excludes $M\_0$. That is to
say, our set of real numbers between 0 and 1 isn't actually complete,
and cannot actually be complete. That last part, "cannot be complete,"
may sound confusing, because, why can't we just add $M\_0$ to the set,
and call it complete? Well, let's do it! We'll tack on $M\_0$ to the set
to produce something like this: $$ \begin{matrix} M\_0 & = & 0 . & a\_0
& b\_1 & c\_2 & d\_3 & e\_4 & \cdots \cr R\_0 & = & 0 . & a\_0 & a\_1
& a\_2 & a\_3 & a\_4 & \cdots \cr R\_1 & = & 0 . & b\_0 & b\_1 & b\_2
& b\_3 & b\_4 & \cdots \cr R\_2 & = & 0 . & c\_0 & c\_1 & c\_2 & c\_3
& c\_4 & \cdots \cr R\_3 & = & 0 . & d\_0 & d\_1 & d\_2 & d\_3 & d\_4
& \cdots \cr \cdots \cr\ \end{matrix} $$ You might foresee what's
going to happen next. We can perform diagonalization again: $$
\begin{matrix} M\_0 & = & 0 . & \color{red}{a\_0} & b\_1 & c\_2 & d\_3
& e\_4 & f\_5 & \cdots \cr R\_0 & = & 0 . & a\_0 & \color{red}{a\_1}
& a\_2 & a\_3 & a\_4 & a\_5 & \cdots \cr R\_1 & = & 0 . & b\_0 & b\_1
& \color{red}{b\_2} & b\_3 & b\_4 & b\_5 & \cdots \cr R\_2 & = & 0 .
& c\_0 & c\_1 & c\_2 & \color{red}{c\_3} & c\_4 & c\_5 & \cdots \cr
R\_3 & = & 0 . & d\_0 & d\_1 & d\_2 & d\_3 & \color{red}{d\_4} & d\_5 &
\cdots \cr \cdots & & & &&&&& \color{red}{\ddots} \cr
\end{matrix} $$ ...to produce a new number... $$ \begin{matrix} 0. &
a\_0 & a\_1 & b\_2 & c\_3 & d\_4 & \cdots \end{matrix} $$ We perform
some transformation on its elements (let's add one, again) in order to
get a new number, say $M\_1$. $$ \begin{matrix} M\_1 & = & 0. & a\_0 +
1 & a\_1 + 1 & b\_2 + 1 & c\_3 + 1& d\_4 + 1 & \cdots \end{matrix}$$
However, note that $M\_1$ must differ from $M\_0$ now, as well as every
other number in the set, by at least one digit. As such, $M\_1$ must not
yet exist in the set. We can add $M\_1$ to the set, and repeat this
process as many times as we want, but we'll always be able to produce a
number outside of the set! 

We call a set with this fascinating property
*uncountably infinite*. What does this mean? We can see that the set of
integers is *countably* infinite, while the set of real numbers between
0 and 1 is *uncountably* infinite. We have successfully proven in this
blog post that there are actually more real numbers between 0 and 1 than
there are integers! 

Cantor's Diagonalization Method has been used to
prove several difficult problems in Mathematics, including the
[Church-Turing
Theorem](http://en.wikipedia.org/wiki/Church-Turing_theorem) and
[Gödel's Incompleteness
Theorems](http://en.wikipedia.org/wiki/G%C3%B6del%27s_incompleteness_theorems).
All three of these theorems have had major effects on the nature of
Mathematics, so you can see that Cantor's Diagonalization Method can be
quite useful!

Until next time,

Ben