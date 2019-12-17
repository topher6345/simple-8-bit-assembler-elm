# Changelog


`end`!! was the special sauce

```elm
assembleLine "HLT AAfdsafdsa, 1"
Ok (A1 { x = "HLT", y = Register "A" })
    : Result (List Parser.DeadEnd) Opcodes
```

) :

## Sill Parsing

Parse 1 line of airty 2 opcode!

Need to limit to 3? Bytes?


## Parser


Base case

Empty string to empty ram = init


start with just `MOV [232], 'h'` (this first part of hello world)

## Why build a CPU emulator in Elm?

advent of code stuff

### A tribute project

Elm has a pastime-like quality. Compiler is so helpful that you don't have to be as 'on'.

You can have the code in a broken state for a really long time, then finally it typechecks and everything just works

Initial struggles with having `19.0` and trying to use `elm-test`

was stuck with an error message that had no google results (symlink already existed, elm-package.json everywhere)

long tail of language/platform issues is the combinatorial ways the outdated versions interact.

fun problem in functional programming

the byte type is fun to work on

A fully specified thing, a Machine with reference

Reverse-engineering? Being a software mimic? X-in-rust genre of tools?

Elm is the most lego-like I know of. (but also inline ruby)

Parser will be challenging - I miss ruby regex tricks like bacon-cannon `/(?<variable>\w+)/=~ string`



