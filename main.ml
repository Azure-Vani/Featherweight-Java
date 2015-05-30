open Core.Std

let () = 
    let lexbuf = Lexing.from_channel (In_channel.create "main") in
    let f = Parser.prog Lexer.token lexbuf in
        printf "OK"

