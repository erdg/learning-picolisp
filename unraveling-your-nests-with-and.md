# Unraveling your Nests with `and`

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

> Bro, you just need the `->`, `->>`, `-<>-||->>>>` operators.
> - Every Clojure programmer ever

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
> Ben Kenobi

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
> - Ezra Koenig

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

### todo
add examples of cleaning up code found in the wild. 
