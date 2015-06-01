open Core.Std
open Jtype

exception Return_ty_mismatch of string

let (>>=) x f = match x with
    | Some err -> printf "[error] Return type error at method: %s\n" err
    | None -> f ()

let check_method_return_ty (cl:j_class) = function
    | `Method (mtd:j_method) -> (
            Class.set_context @@ ("this", cl.name) :: List.map ~f:(fun declar ->
                match declar with
                    | {ty = ty; iden = iden} -> (iden, ty))
            mtd.params;
            match Type.infer_term mtd.return_stat with
                | Some ty -> if ty = mtd.return_type then ()
                             else raise @@ Return_ty_mismatch mtd.name

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

let run filename = 
    let lexbuf = Lexing.from_channel (In_channel.create filename) in
    let adt = Parser.prog Lexer.token lexbuf in
        process_class adt.classes >>= fun () ->
            Utils.traverse_adt "" adt;
            match Type.infer_term adt.term with
                | Some ty -> 
                        printf "%s\n" ty;
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
        )
        (fun filename test () ->
            match filename with
                | Some name -> run name
                | None -> (match test with
                    | true -> ListLabels.iter ~f:run test_list
                    | false -> printf "usage: ./main.native [-s filename] [-t]\n"
                    )
            )

let () = Command.run ~version:"0.1" command
