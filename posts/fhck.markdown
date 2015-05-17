---
title: Fhck, a brainfuck interpreter written in Haskell
author: Ben Kovach
---

If you haven't heard of
[brainf\*ck](http://en.wikipedia.org/wiki/Brainfuck), you should
check it out, because it's probably the coolest programming
language of them all! Also, probably one of the simplest... That's why I
decided to write an interpreter for it. 

This is the product of about 3
days of work, give or take, but the sessions were long and I put a lot
into it. If you're a Haskeller reading this, please feel free to
critique it (I'm sure it could be improved in many places)! If you're
not, please feel free to check it out anyway! I encourage everyone to
play around with brainfuck a little bit because it's actually a lot of
fun. If you're not familiar with the language, a quick look through
brainfuck's [Wikipedia](http://en.wikipedia.org/wiki/Brainfuck) page
should help you to understand what's going on in brainfuck (it's not too
complex). 

With the interpreter, you can write a file, say
`thisisbf.b`, containing the classic "Hello, world!" brainfuck program
directly from the wikipedia page:

```brainfuck
++++++++++[>+++++++>++++++++++>+++>+<<<<-]
>++.>+.+++++++..+++.>++.<<+++++++++++++++.
>.+++.------.--------.>+.>.
```

and pass it to the interpreter: on Windows:

> $ build\windows\fhck.exe thisisbrainfuck.b

or Linux (or presumably Mac OSX, though I haven't tried):

> $ build/linux/fhck hisisbrainfuck.b

...and it should print `Hello, world!` to your console.

Download link/source is located here: [fhck](https://github.com/5outh/fhck) Enjoy!