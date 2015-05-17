---
title: An attempt at zipper-based type inference in Molecule
author: Ben Kovach
tags: programming-languages, types, type-inference
---

### Prelude

I recently designed a very small programming language, [Molecule](https://github.com/5outh/Molecule). Molecule
is a slight extension of the [simply typed lambda calculus (STLC)](http://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) that supports type inference. It is mostly a contained experiment on how to implement type inference in an STLC-like language.

In this blog post, I want to talk about my experience with Molecule and its static type checking and inference mechanism, which works
for *most* valid programs. I do not know if this approach can be modified to work for *all* valid programs (and indeed this is a problem), but I think it's worth talking about regardless.

Molecule and its REPL, `mi`, can be downloaded [from GitHub](https://github.com/5outh/Molecule), if you'd like to play with it.

### Syntax

Molecule's syntax tries to emulate the mathematical notation used in the lambda calculus, without explicit type annotations (which aren't even allowed syntactically). A valid program can be one of the following expressions (informally): 

- `Int` literals
- `Boolean` literals (`t` and `f`)
- Addition (`1 + 2`)
- Boolean Or (`f | t`)
- Lambda abstractions (`\x. x + x`), and
- Function application (`(\x.x) 80`)

Expressions can have type `Int`, `Bool`, or any function type between types, e.g. `Int -> Bool`. Expressions are monomorphic, meaning
they cannot take more than one value. For a quick example, the polymorphic identity function `\x.x` has no type in Molecule -- only applied to another expression can it take on a type. `(\x.x) 10`, for example, has type `Int`.

The type inference mechanism that I will be describing never produces the *wrong* type for a Molecule expression, but it fails 
to realize that some expressions can be typed at all. The following expression, for example:

```haskell
((\x.x) (\x.x)) f
```

should have type `Bool`; however, Molecule's type checker cannot unify its type.

### Type Inference in Molecule

When designing a slightly more ambitious PL, I was writing a naïve type inferencer and hit the point where I was pattern matching on non-leaf expression constructors in order to determine the types of the leaves. At a basic level, there was an `Expr` data type akin to:

```haskell
data Expr = 
    Var String
  | PInt Int
  | Expr :+: Expr
   -- ... and so on
```

And I was pattern matching against, for example `(Var "x" :+: a)` as well as `(a :+: Var "x")` in order to determine that `x` was an `Int`. As you can imagine, this got tedious quickly. I wanted to be able to have some function `infer :: Expr -> Type` such that `infer (Var x)` would spit out the type for `x` in the context of its expression. After narrowing down the information that I actually needed to typecheck a `Var`, I realized all that was needed in order to infer the type of an expression was information about where the `Var` came from one level higher in the AST. For example, in the expression `(Var "x" :+: a)`, just knowing that `Var "x"` came from a `:+:` expression is enough to determine that `x : Int`, and similarly for other types of expressions.

Let's dive into the actual definitions of Molecule's data types in order to talk about how this works concretely in practice.

Types are simple, as described above. `TLam` represents function types (`Int -> Bool === TLam TInt TBool`).

```haskell
data MoleculeType =
    TBool
  | TInt
  | TLam MoleculeType MoleculeType
    deriving Eq
```

Values can be `Bool`s, `Int`s, or function values with an expression environment (mappings from variables in scope to expressions), the name of the argument it takes, and an expression body.

```haskell
type Name = String
type Env = Map Name MoleculeValue

data MoleculeValue =
    VBool Bool
  | VInt Int
  | VLam Env Name MoleculeExpr
```

Expressions are represented as you might expect. `EAbs` represents a lambda abstraction, `EApp` function application, and `:+:` and `:|:` correspond to addition and boolean `or`, respectively.

```haskell
data MoleculeExpr =
    EVar Name
  | ETrue 
  | EFalse
  | EInt Int
  | EAbs Name MoleculeExpr
  | EApp MoleculeExpr MoleculeExpr
  | MoleculeExpr :+: MoleculeExpr
  | MoleculeExpr :|: MoleculeExpr
    deriving Eq 
```

Here's the key data type we need in order to keep track of the expression we're coming from when we hit `Var` values in
the type inferencer. Each of these `Crumb`s tag the expression one level higher and carry along all information in it
that isn't already present in the expression currently being expected. This is one of the pieces of a [zipper](http://learnyouahaskell.com/zippers) for the Molecule AST.

```haskell
data MoleculeCrumb =
  | CPlus MoleculeExpr   -- Came from +
  | COr MoleculeExpr     -- Came from |
  | CAbs Name            -- Came from a lambda abstraction
  | CApp1 MoleculeExpr   -- Came from first arg of application
  | CApp2 MoleculeExpr   -- Came from second arg of application
    deriving (Show, Eq)
```

We use a basic monad transformer stack to represent the type of the type checker.

```haskell
type TypeEnv     = M.Map Name MoleculeType
type Scope       = S.Set Name
type Typechecker = ExceptT MoleculeError (ReaderT (Maybe MoleculeCrumb, Scope) (StateT TypeEnv Identity))
```

`TypeEnv`s are maps from variable names to types, and a `Scope` is a list of variable names in scope. The typechecker has
access to 3 basic (monadic) effects:

1. Error production, via `ExceptT MoleculeError`
2. Threading variable scope and a crumb through the computation, via `ReaderT (Maybe MoleculeCrumb, Scope)`, and
3. Access to a mutable type environment, via `StateT TypeEnv Identity`.

Note that we don't need a "full" zipper here, since we only care about the *single* expression that the variable came from, not the entire path from the AST root. This is all we need to implement a relatively robust type inference/checking mechanism for Molecule. The *inference* part of type checking takes place in the following branch of the `check` function, which accepts an `EVar` (Note: this is modified for brevity;
in practice, more specific type errors are thrown for failing expressions).

```haskell
check :: MoleculeExpr -> Typechecker MoleculeType
check (EVar name) = do
  let addBinding name typ = modify (M.insert name typ) >> return typ
      typeError = throwError . TypeError
  (crumb, scope) <- ask
  if S.notMember name scope
  then typeError $ "variable not in scope: " ++ name
  else do
    env <- get
    case M.lookup name env of
      Nothing -> case crumb of
        Just x -> case x of
          CPlus _ -> addBinding name TInt
          COr   _ -> addBinding name TBool
          CAbs nm -> case M.lookup nm env of
            Just t  -> addBinding name t
          CApp2 e -> do
            t <- check e
            case t of
              TLam v _ -> addBinding name v
      Just t -> case crumb of
        Nothing -> return t
        Just cb -> case cb of
          CPlus _ | t == TInt  -> return TInt
          COr   _ | t == TBool -> return TBool
          CAbs _ -> return t
          CApp1 _ -> case t of
            TLam _ _ -> return t
          CApp2 e -> do
            t' <- check e
            case t' of
              TLam typ _ | typ == t -> return t
```

That's a bit of a mouthful, but can be broken up into sections. First off, if we hit this block of code, we've hit a variable
in a program and need to unify its type. We first get the crumb and scope that has been accumulating throughout traversal of the AST. We then check 
if the variable in question is in scope; if not, we throw a `TypeError`. We next get the type environment `env`. If the variable
in question is not bound in the type environment, we bind it to the appropriate type and return it using `addBinding`. If we came
from a lambda abstraction, the (sub)expression must be `\x.x`, so we return the type of `x` in the environment, if it already exists. If we came from the second value of an application (i.e. the value being *applied* to a function), we check the type of the function it is being applied to --
if it isn't a lambda abstraction, the typechecker fails.

It's worth noting that this last `CApp2` rule is exactly *why* the expression I noted earlier -- `((\x.x) (\y.y)) 10` -- fails. The subexpression `((\x.x) (\y.y))` doesn't typecheck to a `TLam`; it fails to typecheck because `\y.y` isn't unifiable. But I digress -- the typechecker works reasonably well and I think the schema is simple enough to be interesting, even if not exactly practical/suitable for real-world usage in its current state.

If there exists a binding in the type environment for the variable in question, we just make sure that type matches what we expect and return it.

The rest of the `check` function consists of functions in the same vein as this one (the `:+:` branch, again omitting error-handling noise):

```haskell
withCrumb :: Typechecker a -> MoleculeCrumb -> Typechecker a
withCrumb action crumb = local (_1 .~ Just crumb) action

check (e1 :+: e2) = do
  e1' <- check e1 `withCrumb` CPlus e2
  e2' <- check e2 `withCrumb` CPlus e1
  case (e1', e2') of
    (TInt, TInt) -> return TInt
```

where we set the `Crumb` in the expression with a helper function `withCrumb` (which makes use of the `local` and a common `lens`/operation) and propagate typechecking through the rest of the AST.

We can run the typechecker using the final function `typecheck` (which looks complex but just runs a typechecking operation with initially empty environments and no crumb):

```haskell
typecheck :: MoleculeExpr -> Either MoleculeError MoleculeType
typecheck = runTypecheck Nothing S.empty M.empty
  where runTypecheck crumb scope te expr = 
    runIdentity $ evalStateT (runReaderT (runExceptT (check expr)) (crumb, scope)) te
```

Now we have a static typechecker for Molecule expressions, which means, most importantly, that we can run expressions safely *without the types*, which in turn removes a lot of ambiguity from the actual evaluator and allows for faster evaluation since expressions need not be typechecked at runtime.

### Evaluating Molecule Expressions

Let's get right to it -- the code for evaluation in Molecule looks like this:

```haskell
type Evaluator = ExceptT MoleculeError (Reader (M.Map Name MoleculeValue))

eval :: MoleculeExpr -> Evaluator MoleculeValue
eval e = do
  env <- ask
  case e of
    ETrue   -> return $ VBool True
    EFalse  -> return $ VBool False
    EInt x  -> return $ VInt x
    EAbs name e1 -> return $ VLam env name e1
    EVar nm -> return $ fromJust (M.lookup nm env) 
    e1 :+: e2 -> do
      [a, b] <- mapM eval [e1, e2]
      case (a, b) of
        (VInt a', VInt b') -> return . VInt $ a' + b'
    EApp e1 e2 -> do
      [e1', e2'] = mapM eval [e1, e2]
      case e1' of
        VLam env' name body -> local (const $ M.insert name e2' env') $ eval body
```

I've removed the `:|:` rule for brevity (hint: it looks just like `:+:`). Most of this is pretty straightforward because we
don't have to deal with typechecking at runtime. The most complex evaluation rule is the one for `EApp`, which applies `e2` to `e1`. This rule says to evaluate `e1` and `e2`, then take the resulting lambda abstraction, bind the argument name to `e2`'s evaluated expression, then evaluate the lambda abstraction's body with the modified environment.

Again, we can run the evaluator with a simple wrapper function `evaluate`:

```haskell
evaluate :: MoleculeExpr -> Either MoleculeError MoleculeValue
evaluate = runEval M.empty
  where runEval env e = runReader (runExceptT (eval e)) env
```

...and that's basically all there is to Molecule! The `mi` REPL is built with [haskeline](https://hackage.haskell.org/package/haskeline) and supports type checking via `:t` (a la `ghci`) and evaluation by simply typing (no pun intended) expressions.

I've still got a long way to go in the programming languages world, but I'm proud of Molecule even if its type inference is a little flawed. My next project will either have a type system closer to [Hindley Milner](http://en.wikipedia.org/wiki/Hindley–Milner_type_system) (so I can type infer with something closer to [Algorithm W](http://en.wikipedia.org/wiki/Hindley–Milner_type_system#Algorithm_W)), or just make type annotations explicit (for a different set of challenges).

Again, Molecule's full source code is [available on GitHub](https://github.com/5outh/Molecule).

Ben