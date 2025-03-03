> [!NOTE]
> We are in the process of transitioning work on `graphl` to this repository.
> In the meanwhile, this has been made public

## Graphl

Graphl (graph-uhl) is a visual programming language and associated embeddable visual IDE.
Currently the main execution backend is a compiler to [WebAssembly](https://webassembly.org/).

## Graphlt

Graphlt (graph-ult) is the textual programming language that is isomorphic to the Graphl
visual programming language.

It is very early and experimental but capable and we'd be happy to help you make it work for you!

## [Try it out](https://graphl.tech/app)

## [Website](https://graphl.tech)

## [Documentation](https://graphl.tech/blog/docs)

## Building

Built with zig 0.13.0

```sh
zig build graphltc
# open sourcing of the IDE is in progress!
```

## Goals:

- two-way conversion from text to visual and back
- authoritative formatter for both textual and visual representation
- lisp-like macros over graphs for things like:
    - emulating other graph-based scripting systems or workflow engines
    - visual SQL query building
    - animation and behavior graphs in games
    - etc
- lightweight backends for browser use
- embed easily anywhere: web applications, native applications, etc
- package manager
- easy interop with other webassembly binaries

## Non goals:

- the best programming language to work in textually

## WIP:

- standard library
- graph formatting
- a lot
