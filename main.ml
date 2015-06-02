open Core.Std
open Jtype
open Lexing
open Error_handler
open Exceptions

exception Return_ty_mismatch of string

let (>>=) x f = match x with
    | Some err -> printf "[error] Return type error at method: %s\n" err
    | None -> f ()

let (>|) flag f = match flag with
    | true -> f ()
    | false -> ()

let check_method_return_ty (cl:j_class) = function
    | `Method (mtd:j_method) -> (
            Class.set_context @@ ("this", cl.name) :: List.map ~f:(fun declar ->
                match declar with
                    | {ty = ty; iden = iden} -> (iden, ty))
            mtd.params;
            match Type.infer_term mtd.return_stat with
                | Some ty -> if ty = mtd.return_type then ()
                             else raise (Return_ty_mismatch mtd.name)

                | None -> raise @@ Return_ty_mismatch mtd.name)
    | `Cons _ | `Field _ -> ()

let check_return_ty c =
    ListLabels.iter ~f:(check_method_return_ty c) c.items

let process_class cl =
    Class.set_classes cl;
    try
        ListLabels.iter ~f:check_return_ty cl;
        None
    with Return_ty_mismatch info -> Some info

let try_to_parse lexbuf filename =
    try Parser.prog Lexer.token lexbuf
    with
        | Token_exn (line, col, token) ->
                error (Syntax_err (Lex_err token)) ~line ~col ~filename

        | Parser.Error ->
                let line = Lexer.get_line_number lexbuf in
                let col = Lexer.get_col_number lexbuf
                in error (Syntax_err Parse_err) ~line ~col ~filename

let try_to_open_file filename =
    try Lexing.from_channel (In_channel.create filename)
    with
        _ -> error (Fatal_err Not_found_file) ~line:0 ~col:0 ~filename

let run debug filename =
    let lexbuf = try_to_open_file filename in
    let adt = try_to_parse lexbuf filename in
        process_class adt.classes >>= fun () ->
            debug >| (fun _ -> Utils.traverse_adt "" adt);
            match Type.infer_term adt.term with
                | Some ty ->
                        debug >| (fun _ -> printf "The result type is %s\n" ty);
                        let res = Eval.run_eval [] adt.term
                        in Utils.print_term res
                | None -> printf "Type error\n"

let test_list = ["examples/general.java"; "examples/subtype.java"]

let command =
    Command.basic
        ~summary: "An interpreter for featherweight Java"
        Command.Spec.(
            empty
            +> flag "-f" (optional string) ~doc:"string Run the FJ program from given filename"
            +> flag "-t" no_arg ~doc:"Run all test case under examples/ automatically"
            +> flag "-d" no_arg ~doc:"Print debug infomation"
        )
        (fun filename test debug() ->
            match filename with
                | Some name -> run debug name
                | None -> (match test with
                    | true -> ListLabels.iter ~f:(fun filename ->
                            printf "The result of \"%s\":\n" filename;
                            run debug filename;
                            printf "\n") test_list

                    | false -> printf "usage: ./main.native [-s filename] [-t] [-d]\n"
                    )
            )

let () = Command.run ~version:"0.1" command
