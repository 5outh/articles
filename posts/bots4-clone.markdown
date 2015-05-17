---
title: Making a Bots2 clone using Lenses and Pipes
author: Ben Kovach
tags: pipes, lens, haskell
---

#### About bots2
When I was in high school, I came across an internet game called bots2. Bots2 was a multiplayer game where people can log in, customize a robot, and autonomously fight against pre-built AI and other human players. The game would progress in rounds, wherein each robot would attempt to destroy its adversary. The game would continue until one robot dropped below 0 health, at which point the other robot would emerge victorious.

Bots2 was purportedly [attacked by hackers](http://bots4.net/documentation) and never brought back up. However, [someone](http://edmazur.com/) has taken the liberty of creating a clone of the old game, named bots4. If you're interested, [you can play bots4 here](http://bots4.net/). In fact, I might recommend playing around with it for a minute or two, because we'll be building a (very) simple version of bots2 in this post!

#### Preliminaries

I only assume a basic familiarity with monad transformers in this post. If you need an introduction, [sigfpe](http://blog.sigfpe.com) has a [great introduction to them on his blog](http://blog.sigfpe.com/2006/05/grok-haskell-monad-transformers.html). We'll be using Russell O'Connor's [Lens-Family](http://hackage.haskell.org/package/lens-family) and Gabriel Gonzalez' [Pipes](http://hackage.haskell.org/package/pipes) libraries to make the implementation easy (and educational!).

I also want to give a shout out to Gabriel, who was generous enough to personally review and edit my initial draft of the code in this post -- it looks much better than it did before, thanks to him.

Without further ado, let's get started!

```haskell
import           Pipes
import qualified Pipes.Prelude as P
import qualified System.Random as R
import           Lens.Family2             
import           Lens.Family2.Stock       
import           Lens.Family2.State.Lazy 
import           Control.Monad.Trans.State
import           Control.Monad
import           Control.Concurrent(threadDelay)
```

#### Modeling Bots

The game we wish to build pits two bots against each other, who will fight to the death based on who has the better stats (and a bit of randomness). We'll need to be able to model each bot as an entity along with its specific stats. 

During each round, each bot:

1. Deals damage to its opponent
2. Has a chance to block (and take half damage)
3. Has a chance to dodge (and take no damage)

We'll model a `Bot` like so:

```haskell
data Bot = Bot
    { _name :: String
    , _str  :: Int
    , _dex  :: Int
    , _con  :: Int
    , _hp   :: Int
    } deriving (Show, Eq)
```

We'll use `str` to modify damage output, `dex` to modify dodge chance, `con` to modify block chance, and `hp` to denote the amount of health the bot has remaining. We'll also tack on a `name` so we can print out more informative messages during the game. Note the prefix `_` on each field; these are here so we can give our `Lens`es nicer names.

```haskell
name :: Lens' Bot String
str, dex, con, hp :: Lens' Bot Int
name k (Bot nm s d c h)  = fmap (\nm' -> Bot nm' s d c h) (k nm)
str  k (Bot nm s d c h)  = fmap (\s'  -> Bot nm s' d c h) (k s )
dex  k (Bot nm s d c h)  = fmap (\d'  -> Bot nm s d' c h) (k d )
con  k (Bot nm s d c h)  = fmap (\c'  -> Bot nm s d c' h) (k c )
hp   k (Bot nm s d c h)  = fmap (\h'  -> Bot nm s d c h') (k h )
```

We can now define some `Lens`es for our `Bot` fields. We do this manually because it's relatively simple and to avoid the `TemplateHaskell` requirement that comes along with Edward Kmett's larger [Lens](http://hackage.haskell.org/package/lens-3.10.0.1) library. Given a more complicated system, we might choose to use [Lens](http://hackage.haskell.org/package/lens-3.10.0.1) to automatically generate these. However, this is a small program so the overhead isn't necessary. Also, don't worry too much about the declarations of the `Lens`es above: Just know that they allow us to do some cool stuff later on.

With these `Lens`es defined and our `Bot` data type in place, we can move on to defining more of the game's semantics.

#### More Types

We'll need a data type to represent a bot's actions during a single round, and a game state representing the global state of the game:

```haskell
type Event    = (Int, Bool, Bool)
type BotState = (R.StdGen, (Bot, Bot))
```

The `Event` type defines a round of a single Bot's behavior in a 3-tuple -- the first parameter corresponds to damage dealt, the second to whether or not the bot dodged, and the third to whether or not the bot blocked. We will process these events later.

The `BotState` type boxes up a `StdGen` for us to use when generating random events, and a 2-tuple of `Bot`s -- the player character's bot and the enemy AI. This is all the global state we need in our game.

We can make some new `Lens`es for these types, given that 2-tuples are easily indexed using `Lens`:

```haskell
generator :: Lens' BotState R.StdGen
generator = _1

player :: Lens' BotState Bot
player = _2._1

enemy :: Lens' BotState Bot
enemy = _2._2
```
Here we create a `Lens` that references the `StdGen` of the `BotState`, using `_1`. We can also compose `Lens`es using `.` (from the `Prelude`!) and we use this functionality with the simple `Lens`es `_1` and `_2` to make `Lens`es referencing the player and enemy AI in a `BotState`.

Well, that's about all the type declaring we need to do. Now we can get on with piecing together the actual gameplay.


#### Generating Events

Now comes the fun part: actually programming the game mechanics. Essentially what we'd like to do is the following:

1. Generate an event for the player and enemy at the same time.
2. Process each event.
3. If either bot is dead, end the game and print an ending message.

Here we'll focus on (1), generating events. Let's take a look at some code:

```haskell
genEvent :: Bot -> StateT R.StdGen IO Event
genEvent bot = do
    [n, m, r] <- replicateM 3 $ state (R.randomR (0, 100))
    let dodge = n < 100 * bot^.dex `div` (bot^.dex + 50)
        block = m < 100 * bot^.con `div` (bot^.con + 30)
        dmg   = bot^.str + (bot^.str * r) `div` 30
    return (dmg, dodge, block)
```
In order to generate an event for a bot, we grab three numbers between 0 and 100:

- n, which helps determine if the bot dodges,
- m, which helps determine if the bot blocks,
- and r, which helps determine how much damage the bot does.

We can use the `state` combinator here to lift the computation `(R.randomR (0, 100))` to a computation in the `StateT` monad. We then perform some arithmetic using the random numbers we grabbed along with the `dex`, `con`, and `str` stats from our `Bot`. We access these using the `^.` combinator from the `lens-family`, using the `Lens`es we defined above.

Important note: We are producing the amount of damage a bot *deals* here. We will want to process events according to how much damage each bot *receives*, which we'll handle in a minute.

Now that we can generate single events, we need a way of mapping them to specific bots. We'll define a new function, `genEventPair`, to generate *two* events at once, corresponding to the player and the enemy in the game.

```haskell
genEventPair :: StateT BotState IO (Event, Event)
genEventPair = do
    p <- use player
    e <- use enemy
    zoom generator $ liftM2 switchDmgs (genEvent p) (genEvent e)
  where
    switchDmgs (a, b, c) (d, e, f) = ( (d, b, c), (a, e, f) )
```
There are a couple of new things at play here. First, on `use`:

Note the use of our `Lens`es `player` and `enemy`. To access the underlying state in a `StateT`, we typically call the function `lift`. Here instead we call the function `use` from `Lens.Family2.State`, which allows us to specify which *piece* of our `BotState` we want to get. We do just this in order to generate events for both the `player` and `enemy`, using the aforementioned `Lens`es.

Next, if you were looking closely you might have noticed that `genEvent` isn't operating in the same monad as `genEventPair`, yet we use `genEvent` inside of `genEventPair`! We are able do do this using the `zoom` combinator.

`zoom` lifts a stateful operation on one field to a stateful operation on the entire state. Here, we're zooming into `generator` (a `Lens` on our `StdGen`) and lifting the (stateful) generation of events for both the player and enemy into a (stateful) generation of *two* events while preserving player and enemy states. The fact that we can `zoom` into `genEvent` helps out the declaration immensely. It removes a lot of plumbing that we would have had to deal with in order to have `genEvent` operate on an underlying state of type `BotState` and allows the type of the computation to be more explicit.

Finally, Note the use of `switchDmgs` here: This was an ad hoc way to switch around damage dealt and damage sustained. `genEventPair` produces events harboring damage *taken*, which is what we need in order to process them in a nice way. 

#### Processing Events

Now that we're able to generate events and keep track of our game state, the next thing we need to do is actually process these events and update game state. We introduce `resolveEvent` to take care of this:

```haskell
resolveEvent :: (Monad m) => (Event, Event) -> StateT BotState m [(Bot, Event)]
resolveEvent (p_evt, e_evt) = do
    zoom player (resolve p_evt)
    zoom enemy  (resolve e_evt)
    p <- use player
    e <- use enemy
    return [(p, p_evt), (e, e_evt)]

resolve :: (Monad m) => Event -> StateT Bot m ()
resolve (dmg, ddg, blk)
    | ddg       = return ()
    | blk       = hp -= dmg `div` 2
    | otherwise = hp -= dmg
```

First let's look at the `resolve` function. Here we are taking an event and updating a `Bot`'s state based on that event. The implementation is straightforward, especially given that we have the `-=` `Lens` combinator at our disposal -- this allows us to write imperative-looking code that does exactly what you would expect it to. 

In the `resolveEvent` function, you should see some similarities to the above section. We can again use `zoom` and `use` in order to lift computations and retrieve state just as before, but we're updating the main game state now. We produce two 2-tuples containing modified bots and their attached event that was processed during each turn.

At this point, the game logic is actually finished. We have ways to produce and deal with game events that modify game state, and provided that we can actually link these functions together (which we can, as we'll see later), the game will actually run. All that we need to do now is handle IO and bot death. This is where `Pipes` comes in.

#### Pipes and IO

Let's get the boring stuff out of the way first. I mentioned above that we have yet to deal with two major components of our game: IO and bot death. Let's first define some simple functions to deal with these:

```haskell
dead :: Bot -> Bool
dead = (<= 0) . view hp

printBot :: Bot -> IO ()
printBot bot = putStrLn $ bot^.name ++ " has " ++ show (bot^.hp) ++ " hp remaining."

printEvent :: Bot -> Event -> IO ()
printEvent bot (_, True, _) = putStrLn $
  bot^.name ++ " dodges the attack and takes no damage!"
printEvent bot (d, _, True) = putStrLn $ 
  bot^.name ++ " blocks and takes half (" ++ show (d `div` 2) ++ ") damage!"
printEvent bot (d, _,    _) = putStrLn $ 
  bot^.name ++ " takes " ++ show d ++ " damage."
```

Here we again make use of our `Lens`es (these are handy, huh?). We use the `view` function in order to get the health of our bot in `dead`, and check whether its hp is less than 0 in the usual fashion. `view` is just a prefix synonym for the infix `^.` that we've been using all this time. Speaking of which, we use the `^.` combinator heavily in both `printEvent` and `printBot` to handle the string-handling plumbing. Since these functions essentially tell you what they're doing implicitly, I'll omit an extensive explanation.

Now that we have ways to print `Bot`s and `Event`s and check for bot death, the last thing we need to do is actually perform these things the context of our game. A `Consumer` from the `Pipes` library will handle this nicely for us:

```haskell
printGame :: Consumer [(Bot, Event)] (StateT BotState IO) ()
printGame = do
    botEvents@[(b1, e1), (b2, e2)] <- await
    (lift . lift) $ do
        forM_ botEvents $ \be@(bot, event) -> do
            uncurry printEvent be
            printBot bot
            threadDelay 500000
            when (dead bot) $ putStrLn $ bot^.name ++ " died!"
        putStrLn $ case (dead b1, dead b2) of
            (True , True ) -> "It was a tie!"
            (True , False) -> b2^.name ++ " wins!"
            (False, True ) -> b1^.name ++ " wins!"
            _              -> "-----------------"
    unless (any (dead . fst) botEvents) printGame
```

We `await` two `(Bot, Event)`s (from where, you might ask? We'll see in a moment.) and essentially handle all of the plumbing here. For each pair of bots and events, we print the event, print the bot, then wait half a second with `threadDelay` (otherwise the game would run too quickly and we wouldn't see it play out). With a `Consumer` like this, we need to loop until we've consumed all of the input we want, so we use the `when` and `unless` functions from `Control.Monad` as indicators of when to do so. If a bot dies, we stop -- we also print out some information about who died and who won the game.

#### Running the Game

Okay, so I say that we have everything we need now, but you might ask...how do I run this? Let's take a look at a program that pits two bots -- a good guy and a bad guy -- against one another, and we'll dissect it.

```haskell
runGame :: IO ()
runGame = do
    gen <- R.getStdGen
    let player = Bot "The Good Guy" 19 13 12 200
        enemy  = Bot "The Bad Guy"  14 6  10 200
        startState = (gen, (player, enemy))
    flip evalStateT startState 
      $ runEffect 
      $ lift (genEventPair >>= resolveEvent) >~ printGame
```

In the first few lines, we simply set up an initial state for the game. The real "stuff" happens in the line starting with `flip evalStateT`. We'll work through this at a type level from the inside-out. First, we perform `genEventPair >>= resolveEvent`, which effectively handles the logic we talked about earlier and produces a `StateT BotState IO [(Bot, Event)]`. From here, we `lift` the computation into an `Effect (StateT BotState IO) [(Bot, Event)]`, which we then repeatedly pipe into `printGame` with `>~`. After all of this, we `runEffect` to extract the `StateT BotState IO [(Bot, Event)]` from the computation, and finally evaluate the function using `evalStateT`. Phew!

In any case, we can now run our program and execute the game...

```c
*Main> runGame
The Good Guy takes 15 damage.
The Good Guy has 285 hp remaining.
The Bad Guy takes 29 damage.
The Bad Guy has 171 hp remaining.
-----------------
The Good Guy dodges the attack and takes no damage!
The Good Guy has 285 hp remaining.
The Bad Guy takes 20 damage.
The Bad Guy has 151 hp remaining.
-----------------
The Good Guy blocks and takes half (24) damage!
The Good Guy has 261 hp remaining.
The Bad Guy takes 34 damage.
The Bad Guy has 117 hp remaining.
-----------------
The Good Guy takes 46 damage.
The Good Guy has 215 hp remaining.
The Bad Guy takes 19 damage.
The Bad Guy has 98 hp remaining.
-----------------
The Good Guy takes 32 damage.
The Good Guy has 183 hp remaining.
The Bad Guy takes 37 damage.
The Bad Guy has 61 hp remaining.
-----------------
The Good Guy dodges the attack and takes no damage!
The Good Guy has 183 hp remaining.
The Bad Guy takes 12 damage.
The Bad Guy has 49 hp remaining.
-----------------
The Good Guy takes 24 damage.
The Good Guy has 159 hp remaining.
The Bad Guy takes 23 damage.
The Bad Guy has 26 hp remaining.
-----------------
The Good Guy dodges the attack and takes no damage!
The Good Guy has 159 hp remaining.
The Bad Guy takes 36 damage.
The Bad Guy has -10 hp remaining.
The Bad Guy died!
The Good Guy wins!
```

...and behold, the good guy wins (this time)!

[View full source on GitHub](https://gist.github.com/5outh/8049361).

\- Ben