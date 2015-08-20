# Make that Shit

In this tutorial, we'll go over the builtin PicoLisp function `make` and hopefully do some cool things along the way. But let's be real, this is a tutorial for peeps that don't know sheet about `make`. I am one of those people at the moment of writing this, so we probably won't be doing anything super exciting. But I'll at least try to keep the examples silly!

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

or if we wanted to be really verbose and overkill, like all that *linking*, we could write:
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


Heh. See what we did there? We just used our ignorance of `make` to fuel a little list building session. Make that shit! And you better have typed in all those examples. You're cheating if you didn't. And you better have already called `doc` on any of those functions that you need to review... ahem... `(doc 'any)` and `(doc 'chop)` at least.

Back to 'make'. Surely there must be a reason to have yet *another* function for building lists. Well, it is lisp.

> **HINT:** The people who build programming languages know more about programming than we do. If there is a feature of the language that seems weird or redundant or silly or dumb, you're probably misusing it and don't fully understand the problem that it solves. So do everyone a favor; play around with said feature, and get yourself edumacated! 

Onward!
```lisp
: (make (link 'how) (link 'does) (link 'this 'work?))
-> (how does this work?)

```
That's the same as the example above, but with text instead of numbers! It seems like `link` just squishes everything together to form a list.

```lisp
: (make (link (pop (1 2 3)) (link 'thing)))
-> (1 thing)
```
Ooooh. So we can `link` more than just numbers and symbols? Indeed, we can! What exactly does `link` do?
```lisp
: (doc 'link)
```

It just sticks them all in there, one right after the other. Better practice that some more.
```lisp
: (make (link (nth 2 (1 2 3))) (link 'things))
-> (2 things)
: (make (link (find '((X) (= X 'red)) '(blue green red))) (link 'thing))
-> (red thing)
: (make (link (find '((X) (not (num? X))) (1 2 3 4 blue 5 6))) (link 'things))
-> (blue things)
```

We know that we're building lists... then we're linking different things together... but then what? What exactly is happening, especially in the first call to `link` in each of the preceeding examples? Well, the beauty of the `make` is that we can build lists that are the result of arbitrary lisp-isms. *WUT?* That means we can call any function we want to get the desired list! Check it. In the example that returned `(red thing)`, we wanted to find the word 'red' from a list of colors. We did that with another PicoLisp function that you should know, `find`.

```lisp
: (doc 'find)
```
The wording there is a bit confusing. `find` looks at each item in the list, one after the other, to see if it passes a test. It then returns the first item that passes the test. In our case, the test was, *Is this item the word 'red'?* Once it found the word 'red', `find` was all like, "Hey I got it! It's 'red'. Here you go!" And all of that was wrapped in a `link`, so we stuck it at the beginning of the list. And then we linked the word 'thing' to that list.

Similarly, in the example that returned `(blue thing)`, we wanted to find the first item that wasn't a number. We found 'blue', and then `link`ed 'thing' to it. 


### todo
- more examples with `link`
- examples and explanations for `chain`, `yoke`, and `made`.
