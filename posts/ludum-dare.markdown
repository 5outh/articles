---
title: Ludum Dare 27: A Post-mortem
author: Ben Kovach
---

#### What is Ludum Dare? 

[Ludum Dare](http://www.ludumdare.com/compo/) is
a friendly competition among game developers to see who can make the
best game based on a given theme in 48 hours. This was the 27th
competition, and the theme was "10 seconds." 

#### Why did I want to participate?

<img src="http://2.bp.blogspot.com/-C5uKdb6VMHA/Uh0CIyE4VOI/AAAAAAAAAHg/Ws8COqbSITA/s1600/25390-shot0.png" class="right" style="width:400px"></a>
For a long time, I've been looking at the game
development community from the outside with a lens that I feel a lot of
people tend to see through. I've always considered game development an
extremely intricate and difficult field to enter, and I was scared of
trying my hand at it for a long time. I have made small clones of other
games (pong, sokoban, etc) and I have enjoyed making those games, but
they're both relatively simple (exercise-sized, almost, to a certain
extent). I have been wanting to get a little further into the game
development scene for a while, and Ludum Dare seemed like the perfect
opportunity. I've seen several Ludum Dare competitions go by over the
past couple of years and I never thought I was good enough to actually
do it. This time, I just went for it, and I'm really glad I did.

#### What did I make?

When
the theme "10 seconds" was announced, I had a billion and one ideas
going through my head, because it's so open-ended. Everyone's first idea
was to make a 10-second game, but that doesn't really work, so you had
to move on to the next thing. I bounced a couple of ideas around in my
head, but I ultimately decided to make a twin-stick roguelike-like
because:

1.  Movement and collision detection is relatively simple to implement,
    and
2.  As all of my friends know, I'm a big fan of [The Binding of
    Isaac](http://en.wikipedia.org/wiki/The_Binding_of_Isaac_(video_game)),
    and I'm always thinking about how that game and games similar in
    nature to it are put together.

I ended up calling my game "Wappansai!" because it was one of the random
strings I stuck in as a debug message as I was developing. It doesn't
mean anything other than that. Anyway, in Wappansai!, you're a big fat
square ninja man who is stuck in a dungeon with some red business-men
enemies. They don't move, but if you don't murder all of the enemies in
the room within **10 seconds**, you die. Running into enemies also kills
you, as does falling into the many pits laid out around the room. Later
on, moving spike tiles get added, and those kill you as well. When you
clear a room under 10 seconds, a door opens up and you move on to the
next room where things get progressively more difficult. I think my high
score in the final version of the game was somewhere around 17 rooms, so
I definitely challenge everyone to beat that! Oh, and the tools I used
were C++ and SDL 2.0, sfxr for sound bytes, and photoshop for graphics.
I didn't get around to making music because I don't even know where to
start with that! 

#### What did I learn?

I have been reading
[Programming Game AI by
Example](http://www.amazon.com/Programming-Game-Example-Mat-Buckland/dp/1556220782)
on and off for a little while, and I decided to use Ludum Dare as an
opportunity to start learning a little more about the [State Design
Pattern](http://en.wikipedia.org/wiki/State_pattern) and
[Singletons](http://en.wikipedia.org/wiki/Singleton_pattern), two
concepts introduced early on in that book. I worked on throwing together
a shell of a program that implemented these things before the
competition, so I wouldn't have to figure everything out as I was trying
to get things done over the 48 hours, which was definitely a good idea.
I was able to start working right when the clock started ticking instead
of worrying about getting the design right as I was trying to implement
the program. I'd recommend doing this if you plan to participate in a
game jam; it really helped me out. That said, there were still some
design pitfalls. For example, when you open chests in the game, there's
a (very slim) chance that you'll get a laser gun from inside of it
(usually it just takes all of your currently held items -- haha!), but
the gun doesn't spawn anywhere when this happens; it just magically goes
into your character's weapon slot and suddenly you're firing lasers. The
way I wanted this to work was, if you got lucky and got the laser gun
from the chest, the gun would spawn somewhere on the map and you could
go pick it up and feel awesome that that chest that normally screws you
over by stealing all of your items just happened to spawn a freakin'
laser gun for you to play with. But, the way I organized my project,
Tile objects had no way to modify Level objects, so I was unable to
trigger an event when the chest (a type of Tile) was touched to modify
the level (a Level), so I had to make a compromise there. There are
certainly workarounds that I considered, but I couldn't really do them
within the time limit. It was a design flaw, and now I know that it was
a design flaw. I can structure my code better now -- that was one major
learning point.

<img src="http://4.bp.blogspot.com/-d2Tcd-4PCiM/Uh0CI8r_h7I/AAAAAAAAAHc/NsQtn54CdGo/s400/25390-shot1.png" class="right"></a>

I also learned a lot about randomly generated content, that was crazy!
It actually went a lot smoother than I thought it would, but just seeing
everything so randomized was amazing. Items, weapons, enemies, pits, and
walls all get generated randomly based on how many rooms you've
traversed, and just seeing that in play working (relatively) nicely was
so cool. So I learned a lot there. The final, really huge thing that I
discovered was that...holy crap, there are a lot of assets that go into
games! I must have made over 50 sprites for Wappansai!, on top of tons
of different sound clips (sfxr made this easy). A lot more time went
into art than I expected, and I didn't even animate anything...you don't
really think about it, but games have tons and tons of assets involved.

#### Would I do it again? 

<img class="right" src="http://2.bp.blogspot.com/-D5-Ewsy8IWs/Uh0CI_MUg3I/AAAAAAAAAHY/TIYg51u7Xqc/s400/25390-shot2.png"></a>

Finishing a game in two days was really hard
work and it could be really frustrating at times (especially using C++
with VS, those error messages are completely incomprehensible). But, it
was totally worth the effort. I can say that I've made a game that is
completely mine, and even though it's not very good, it's something
that*I* made and I love that I made it and I'm excited to make more
things. It's also really amazing that people actually do play your
games. I've had several comments on my ludum dare game's page already
discussing the game that *I* made, and it's so cool to see that. Playing
everyone else's games is also really awesome because you know that they
went through the same thing that you did. Everyone's really proud of
their work, everyone worked hard to get something done, and it's just a
really nice community event that I'm so happy to have been a part of. I
would absolutely do it again, and plan to! 


#### Who would I recommend Ludum Dare to?

If you want to make games, do it. If you don't know where to start,
there are plenty of websites that can teach you the basics, and even if
you don't want/ know how to program, there are tools out there like
[GameMaker Studio](http://www.yoyogames.com/gamemaker/studio) that will
allow you to program your game's logic without actually writing code. If
you don't think you can do it alone, team up with someone. But if you
want to make video games and have at least a basic knowledge of how they
work, I think that Ludum Dare is an excellent way to test your skills
and build something that you'll be proud of. 

#### Where can you download my game?

You can download, rate, and comment on
[Wappansai!](http://www.ludumdare.com/compo/ludum-dare-27/?action=preview&uid=25390)
over at the [Ludum Dare
page](http://www.ludumdare.com/compo/ludum-dare-27/?action=preview&uid=25390)
for it. Thanks for reading!