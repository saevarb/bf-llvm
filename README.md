# bf-llvm

Brainfuck to llvm compiler in Haskell. Messy WIP.

Build with `stack build`. Requires `llvm ==5.0.*` to be installed.

Run with `stack exec compiler < test/data/helloworld-nc.bf` or `cat - | stack exec compiler` if you want
to input code manually, then send EOF with `^D`.
