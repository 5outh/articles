---
title: The Mother of all Monads
author: Ben Kovach
tags: haskell, continuations
---

Notes on [The Mother of all Monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html)

Goal: Find a "universal" monad that encompasses the power of all others.

```haskell
import Control.Monad.Cont
import Data.Functor.Identity

ex1 :: Cont String Integer
ex1 = do 
  a <- return 1
  b <- return 10
  return $ a + b
test1 = runCont ex1 show
```

test1 results in "11". The `Cont` monad gives us back something that takes a function and applies it to 11. We can add a "hole" in the above computation that should hold an integer; we can do this inside the monad by using a continuation (which we name `fred`).

The `Cont` monad allows you to write subexpressions that are able to "capture" the entirety of the code around them, up to the function provided to `runCont`.

```haskell
ex2 :: ContT r m Integer
ex2 = do
  a <- return 1
  b <- ContT (\fred -> fred 10)
  return $ a+b

test2 = runCont ex2 show
```

`test2` results in "11" as well. We've just shoved 10 into the computation's hole. The expression for `b` can do whatever it wants with `fred` as long as it returns a `String` (in this case)

```haskell
ex3 :: ContT Char [] Integer
ex3 = do
  a <- return 1
  b <- ContT (\fred -> "escape")
  return $ a+b

test3 = runContT ex3 show 
```

test3 returns `"escape"`; fred here is completely ignored. The computation is thrown out -- continuations provide exception handling, which is what `Maybe` provides!

```haskell
ex4 :: ContT r [] Integer
ex4 = do
  a <- return 1
  b <- ContT (\fred -> fred 10 ++ fred 20)
  return $ a+b

test4 = runContT ex4 show
```

`test4` returns "1121".

NB. I think this is what happens: We run the whole computation `ex4` for the first subexpression `fred 10`; i.e. `b` "becomes" 10, a is 1, and by adding we get 11. Then we do the same thing with `fred 20`, so `b` "becomes" 20, `a` is 1, and by adding we get `21`. Now we have both subparts of our final answer, which we then `show` and concatenate together with `runContT`.

Note that this code is a lot like this computation in the list monad:

```haskell
test5 = do
  a <- return 1
  b <- [10,20]
  return $ a+b
```

`test5` returns [11, 21]; we can emulate this in `Cont`!

```haskell
ex6 = do
  a <- return 1
  b <- ContT (\fred -> fred 10 ++ fred 20)
  return $ a+b

test6 = runContT ex6 (:[])
```

`ex6` is equivalent to (because `>>=` for lists is `concatMap`):

```haskell
ex8 = do
  a <- return 1
  b <- ContT (\fred -> [10,20] >>= fred)
  return $ a+b

test8 = runContT ex8 return
```

We have removed almost all list-specific logic in the above code! We're using monad-related functions without do notation. One last thing:

```haskell
i :: Monad m => m a -> ContT r m a
i x = ContT (\fred -> x >>= fred)

run :: Monad m => ContT r m r -> m r
run m = runContT m return
```

`i` boxes up exactly what we've been doing. `run` is just an alias for `runContT` that uses `return` as its "final" function. An example:

```haskell
test9 :: [Int]
test9 = run $ do
  a <- i [1,2]
  b <- i [10,20]
  return $ a+b
```

`test9` returns `[11,21,12,22]`, as expected. If you think about this with the same type of logic as before, i.e. each computation "hole" gets filled and run in the entire context, but applied using monadic bind. Since `run` is completely monad-agnostic, we can then even write this:

```haskell
test10 :: IO ()
test10 = run $ do
  i $ print "What is your name?"
  name <- i getLine
  i $ print $ "Merry Xmas " ++ name
```

Which produces:

```
λ> test10
"What is your name?"
Ben
"Merry Xmas Ben"
```

Interesting consequences beyond Haskell: Any language with support for continuations (with nice syntax) should have a nice syntax for monadic computations as well! Very cool.

For further reading, check [Control.Monad.Cont](http://hackage.haskell.org/package/mtl-2.0.1.0/docs/Control-Monad-Cont.html) on Hackage. In particular, keep in mind that `callCC` is a thing, which allows the user to interrupt execution of a running `Cont` block. Here's the example for reference:

```haskell
whatsYourName :: String -> String
whatsYourName name =
  (`runCont` id) $ do                    
    response <- callCC $ \exit -> do     
      validateName name exit             
      return $ "Welcome, " ++ name ++ "!"
    return response                      

validateName :: Monad m => String -> (String -> m ()) -> m ()
validateName name exit = do
  when (null name) (exit "You forgot to tell me your name!")
```

`validateName` calls `exit` implicitly when it receives a failing message. `whatsYourName` runs a continuation which calls `validateName` in order to make sure that one is provided. The above can be slightly modified using `ContT` to run in `IO`:

```haskell
whatsYourName' :: IO ()
whatsYourName' = flip runContT putStrLn $ 
    callCC $ \exit -> do
      name <- liftIO $ do
        putStrLn "What's your name?"
        getLine
      validateName name exit             
      return $ "Welcome, " ++ name ++ "!"
```

Further reading: 
- [Free Monads for Less](http://comonad.com/reader/2011/free-monads-for-less/) on `Codensity`