open Core.Std

type syntax_err = 
    | Lex_err of string
    | Parse_err

type fatal_err = 
    | Not_found_file

type errors = 
    | Fatal_err of fatal_err
    | Syntax_err of syntax_err

let error err_ty ~line ~col ~filename =
    (match err_ty with
        | Fatal_err sub_err ->
                printf "Fatal error: ";
                (match sub_err with
                    | Not_found_file -> printf "File \"%s\" dose not exists\n" filename
                );
                printf "  Fatal error, abort.\n";
                exit 2
        | Syntax_err sub_err ->
                printf "Syntax error: ";
                (match sub_err with
                    | Lex_err token -> printf "Unexpected identifier: \"%s\"\n" token
                    | Parse_err -> printf "Invalid syntax\n"
                )
    );
    printf "  File \"%s\", line %d, column %d\n" filename line col;
    exit 1

