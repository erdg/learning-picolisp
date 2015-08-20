# The Work Flow

This little nugget will go through the basics of using Vim within PicoLisp. 

## topics
- using 'vi' and 'ld'
- using 'edit'


### Using 'vi' and 'ld'
at our command line,
```bash
$ echo "(de foo ())" > foo.l
```
This command put a function prototype in the file `foo.l`.


Now fire up PicoLisp in Debug Mode with that file loaded,
```bash
$ pil foo.l +
:
``` 

Let's edit our favorite function!
```lisp
: (vi 'foo)
```

We're are now looking at the definition of `foo` within a Vim Buffer within a running PicoLisp instance. Wow! To verify that this is indeed the truth, and nothing but the truth, we better `:q`uit Vim.

```lisp
:
```
PicoLisp was patiently waiting for us the whole time. 

Back to `foo`ling around in Vim.
```lisp
: (vi 'foo)
```

Let's change `foo` so that it prints a cute little ASCII mouse. In command mode, type `>I` to insert at the end of our existing definition.
```lisp
(de foo ()
   (println "A_A")
   (println "o o")
   (println ">v<") )
```
Satified with our new definition of `foo`, we exit insert mode and type `ZZ` in command mode, to save our changes and return to PicoLisp.


Back at the REPL now, we need to reload `foo` so we can play with the new version of it.
```lisp
: (ld)
# foo redefined
-> foo
:
```


What happened there? As always, when you're not sure what a new function does...
```lisp
: (doc 'ld)
``` 
Ah, gotcha.


Sweet! Now let's test it out.
```lisp
: (foo)
"A A"
"o o"
">v<"
-> ">v<"
```


That's pretty cool, but I don't like all the `"`'s in the output. Let's change that.
```lisp
: (vi 'foo)
```

We need to substitute all the `println`'s to `prinl`'s in our definition. We can do this in one Vim command, `:%s/println/prinl`. Enter that now. Magically, all the `println`'s have changed to `prinl`'s! That was slick. Read [this](http://vim.wikia.com/wiki/Search_and_replace) to find out what that command just did.

Our lastest `foo` should now be
```lisp
(de foo ()
   (prinl "A_A")
   (prinl "o o")
   (prinl ">v<") )
```


Exit Vim with `ZZ` and `ld` the file.
```lisp
: (ld)
# foo redefined
-> foo
:
```
Remember the difference between `println` and `prinl`? If not, now would be the time to `(doc 'println)` and `(doc 'prinl)`.


Try it out!
```lisp
: (foo)
A A
o o
>v<
-> ">v<"
```
Neato!


But I'm still not sold on the function's return value. A bit offputting to see an additional nose/whisker combo just floating there in mid air. Foo! Ideally `foo` would simply return `T` to let us know that a cute little ASCII mouse has been printed. Luckily, there's a function for that!
```lisp
: (doc 't)
```
The example is the same use case as ours! This is gonna be perfect.


Back to the `foo`...ture.  
```lisp
: (vi 'foo)
```


Change `foo` to look like this,
```lisp
(de foo ()
   (t (prinl "A A")
      (prinl "o o")
      (prinl ">v<") ) )
```
In command mode, save and quit with `ZZ`. 


`ld` the file and try it out again.
```lisp
: (ld)
# foo redefined
-> foo
: (foo)
A A
o o
>v<
-> T
:
```

Our little mouse friend is getting suspicious of all the editing...
```lisp
: (vi 'foo)
```

Let's change him one last time.
```lisp
(de foo ()
   (t (prinl "A A")
      (prinl "_ /")
      (prinl "o O")
      (prinl ">v<") ) )
```

Save and quit, reload and fire away.
```lisp
: (ld)
# foo redefined
-> foo
: (foo)
A A
_ /
o O
>v<
-> T
```

Are you feeling the flow? Simply call `vi` and then `ld`! 
