# Unraveling your Nests with 'and'

Every now and again I find myself writing code that looks something like this,
```lisp
(mapcar 
   '((X Y)
      (I have
         (no
            (idea what 
               (the hell
                  (I was
                     (thinking when
                        (I wrote this 
                           (convoluted mess) ) )
                     (This function 
                        (was 
                           (probably 
                              (developed 
                                 (iteratively
                                    (at the REPL) ) ) ) )
                        (Then I barfed 
                           (this monster 
                              (into 
                                 (a 
                                    (file
                                       (and now
                                          (two weeks later
                                             (I have no idea
                                                (what any of it means) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )
   'do-we-even-care-what-we're-mapping-over-at-this-point )
```

*Siiiiigggghhhhhhhh*. Should have gone to bed.

> You just need the `->`, `->>` and `-<>-||->>>>` operators.

> -- Every Clojure enthusiast ever

Maybe I do... *maybe I do.* Alright. LET'S DO THIS!!!
```lisp
: (de -> ("X" . "A")
     (for "Form" "A"
        (setq "X" (apply (car "Form") (cdr "Form") "X")) ) )
# -> redefined
-> ->
```

Heh. So many arrows...

Wait a second!! `->` redefined? PicoLisp already has `->`? 
```lisp
: (doc '->)
```

> These aren't the functions you're looking for.

> -- Ben Kenobi

That's fine, We've got the real `->` now!
```lisp
: (-> 13 (+ 2) (+ 3) (/ 3))
-> 6
```

But if we're really gonna unravel all those nefarious levels of nesting, we're gonna need `->>` as well...
```lisp
:   # defines the '->>' operator ;)
```

And all our problems with nastily nested nesting vanish. Until we stay up too late programming again and return in the morning to find that someone hacked us and replaced our beautiful code with some sort of archaic treasure map.
```lisp
(-> X marks the spot
  (->> |
       |  ..............
 /-\   V  ..............
 | | <--  . quicksand ..
 |        ..............
 \_____ X --<<>>~> Y
            ~~~~   |
            ~~~~   \---> X
  look out for snakes!   |             
             /-  X <<----/
             |
    JK! <---<->----=>=>=>=> X ) )
```

Wouldn't it be awesome if there was a way we could eloquently un-nest our code without drawing arrows everywhere? And wouldn't it be awesome if we could arbitrarily thread our thingamajig through endless function calls and place the latest result anywhere in the next expression?! Well, it turns out there is a way to do that, and it turns out that way is called `and`. 

Check [this](http://software-lab.de/doc/ref.html#atres) out, particularly the bit on 'Flow Functions'.

That's right. PicoLisp's flow- and logic-functions hang on to the values of their controlling expressions. They can later be accessed with the `@` symbol.

```lisp
: (and 13 (+ 2 @) (+ 3 @) (/ @ 3))
-> 6
```
I read this code as saying, "Take 13, add two to it, then add three to it and finally divide it by 3."

> Who gives a fuck about an Oxford comma?

> -- Ezra Koenig

Within `and` the symbol `@` will refer to the result of your last expression. This allows us to thread it into the next expression in *any* place that we desire.
```lisp
: (and 13 
   (cons @ (14 15)) 
   (append (11 12) @) 
   (prog1 (mapcar printsp @) (println "YAY!")) 
   (mapcar inc @) 
   (place 3 @ "hey there!") 
   (mapcar printsp @) )
11 12 13 14 15 "YAY!"
12 13 "hey there!" 15 16 -> (12 13 "hey there!" 15 16)
```

I'm not even going to try to re-nest that!

### In the Wild
The other day I was working on some PicoLisp code to generate Lilypond code to generate sheet music for me. I had a function, `pack-durations` that looked something like this,
```lisp
[de pack-durations (Music)
   (let Durs (map@ Music (get-duration @))
      (mapc
         '((Obj)
            (or 
               (and (pair Obj) (eval @))
               (prin Obj " ") ) )
         (mapcar any
            (mapcar 
               '((X) (if (pre? "r" X) (pack X) (pack "b" X)))
               Durs ]
```

It's not too hairy, but it certainly isn't immediately obvious what this function does. I'll explain briefly, though it isn't terribly important. It expects a list of Lilypond chord symbols, e.g. `(aes4.:min7 b8:maj9 r c:min11 r des:maj7)`. Then it extracts the durations from those symbols, the `4.` and `8`, and finally does a bunch of nested mapping to achieve the desired result. Let's see if we can't rewrite it using our `and` technique to make the code more readable.

To do so, we have to turn it inside out; that is, we start with whatever is buried at the bottom of our nested mess. In our case it's,
```lisp
(mapcar
   '((X) (if (pre? "r" X) (pack X) (pack "b" X)))
   Durs )
```
So this is what we'll start off our call to `and` with.
```lisp
(and
   (mapcar
      '((X) (if (pre? "r" X) (pack X) (pack "b" X)))
      Durs )
```
Remember that the result of that expression will be held in the symbol `@`. Moving one level of nesting outward we find,
```lisp
(mapcar any
   ... )
```
We've got to `mapcar` `any` over the result (stored in `@`) of the previous expression. Adding that to our `and` sequence we get,
```lisp
(and
   (mapcar 
      '((X) (if (pre? "r" X) (pack X) (pack "b" X)))
      Durs )
   (mapcar any @)
```
Finally, we reach our last level of nested mapping: the call to `mapc`. Let's add that to our `and` sequence.
```lisp
(and
   (mapcar 
      '((X) (if (pre? "r" X) (pack X) (pack "b" X)))
      Durs )
   (mapcar any @)
   (mapc
      '((Obj)
         (or
            (and (pair Obj) (eval @))
            (prin Obj " ") ) )
      @ ) )
```

Yay! With that, the whole function becomes,
```lisp
[de pack-durations (Music)
   (let Durs (map@ Music (get-duration @))
      (and
         (mapcar 
            '((X) (if (pre? "r" X) (pack X) (pack "b" X)))
            Durs )
         (mapcar any @)
         (mapc
            '((Obj)
               (or
                  (and (pair Obj) (eval @))
                  (prin Obj " ") ) )
            @ ] 
```

I think this is more readable, but your results may vary. I like that each stage is laid out a little more explicitly. We see what happens, and then we see what we're doing with the result in the next part. And if you look closely, you'll notice that there are even nested `and`'s and `@`'s in there! You'll see an overwhelming amount of `@`s when you first start digging into PicoLisp code. Once you get used to the idioms, however, the code becomes both shorter and more readable. And when in doubt, check [this](http://picolisp.com/wiki/?AtMark) out! 

