open Core.Std
open Jtype

let () = 
    let lexbuf = Lexing.from_channel (In_channel.create "test.java") in
    let adt = Parser.prog Lexer.token lexbuf in
        Class.set_classes adt.classes;
        Utils.traverse_adt "" adt
