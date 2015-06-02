Installation
============

The interpreter depends on `Core` and `menhir`, you can get them by
    
    opam install core
    opam install menhir

Then you can build it at the project directory by

    corebuild -use-menhir main.native

Usage
=======
The usage for this interpreter is
    
    ./main.native [-f filename] [-t] [-d]

+ `-f` Excecute a program from source file 
+ `-t` Automantically run all test source files under the `example/` directory
+ `-d` Enable debug mode
