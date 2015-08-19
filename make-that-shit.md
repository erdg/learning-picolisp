# Make that Shit

In this tutorial, we'll go over the builtin PicoLisp function 'make' and hopefully do some cool things along the way. But let's be real, this is a tutorial for peeps thatdon't know sheet about 'make'. I am one of those people at the moment of writing this. So we probably won't be doing anything super exciting. But I'll at least try to keep the examples silly!

As always,
```lisp
: (doc 'make)
```

Right... something about list building, some other functions we haven't heard of yet. Hmm. We've got our work cut out for us. All we need to know for now is that we'll be `make`ing lists today. Better start with the example given in the documentation.
```lisp
: (make (link 1) (link 2 3) (link 4))
-> (1 2 3 4)
```

Seems like a strange way to go about building a list. Fairly verbose. We've built lists before. Why not something like,
```lisp
: (list 1 2 3 4)
-> (1 2 3 4)
```
Simple! And we don't have all that noise about *linking* and whatever.

Heck, we could even build the list this way.
```lisp
: (cons 1 (2 3 4))
-> (1 2 3 4)
```

or...
```lisp
: (cons 1 2 3 (4))
-> (1 2 3 4)
```

or if we wanted to be really verbose and overkill, like all that `link`ing, we could write:
```lisp
: (cons 1 (cons 2 (cons 3 (cons 4 NIL))))
-> (1 2 3 4)
```

In fact, we know other functions that can do the same thing!
```lisp
: (append (1 2) (3 4))
-> (1 2 3 4)
```
Boom!

```lisp
: (push 1 (2 3 4))
-> (1 2 3 4)
```
Blam!

```lisp
: (mapcar any (chop "1234"))
-> (1 2 3 4)
```
KAAPPOOOWWWWW!!!!!


Heh. See what we did there? We just used our ignorance of 'make' to fuel a little list building session. Make that shit! And you better have typed in all those examples. You're cheating if you didn't. And you better have already called 'doc' on any of those functions that you need to review... ahem... `(doc 'any)` and `(doc 'chop)` at least.

Back to 'make'. Surely there must be a reason to have yet *another* function for building lists. Well, it is lisp.

> **HINT:** The people who build programming languages know more about programming than we do. If there is a feature of the language that seems weird or redundant or silly or dumb, you're probably misusing it and don't fully understand the problem that it solves. So do everyone a favor; play around with said feature, and get yourself edumacated! 


