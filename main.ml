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

let () = 
    let lexbuf = Lexing.from_channel (In_channel.create "test.java") in
    let adt = Parser.prog Lexer.token lexbuf in
        process_class adt.classes >>= fun () ->
            Utils.traverse_adt "" adt;
            match Type.infer_term adt.term with
                | Some ty -> 
                        printf "%s\n" ty;
                        let res = Eval.run_eval [] adt.term
                        in Utils.print_term res
                | None -> printf "Type error\n"

