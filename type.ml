open Jtype
open Core.Std

let ty_int = "int"
let ty_bool = "bool"
let ty_root_object = "Object"

let is_int = (=) ty_int
let is_bool = (=) ty_bool

let rec is_sub_class x y = 
    if x = y then true
    else if x = ty_root_object then false
        else try
                let super = Class.get_super_class x 
                in is_sub_class super y
            with Not_found -> false

let ( *|) f g = match (f, g) with
    | (true, true) -> Some true
    | (false, false) -> Some false
    | _ -> None

let (@=) p res = match p with
    | Some x -> x || res
    | None -> false

let check_is_subtype t1 t2 = 
    is_int t1 *| is_int t2 @=
    is_bool t1 *| is_bool t2 @=
    is_sub_class t1 t2

let 

rec (=?) x f = match infer_term x with
    | Some ty -> f ty
    | None -> false

and (|-) p ty = 
    if p then Some ty else None

and (<:) t ty = match infer_term t with
    | Some xty -> check_is_subtype xty ty
    | None -> false

and (->>) t ty = match infer_term t with
    | Some xty -> check_is_subtype ty xty
    | None -> false

and (<=>) terms declars = 
    try
        let f = fun t declar -> t <: declar.ty
        in ListLabels.for_all2 terms declars ~f
    with Invalid_argument _ -> false

and (~>) = Class.is_class

and (>>=) term f = match infer_term term with
    | Some ty -> f ty
    | None -> None

and infer_term = function
    | Plus (t1, t2) -> 
            (t1 =? is_int && t2 =? is_int) |- ty_int

    | Minus (t1, t2) ->
            (t1 =? is_int && t2 =? is_int) |- ty_int
    
    | Times (t1, t2) ->
            (t1 =? is_int && t2 =? is_int) |- ty_int

    | Cast (ty, term) ->
            ((term <: ty) || (term ->> ty)) |- ty

    | New (name, params) -> 
            (~>name && params <=> Class.get_cons_params name) |- name

    | Invoke (term, f, params) ->
            term >>= 
                fun name -> params <=> Class.get_method_params name f |- Class.get_method_returnty name f

    | Access (term, x) ->
            term >>=
                fun name -> true |- Class.get_field_ty name x

    | Value v -> match v with  
        | Primary _ -> true |- ty_int
        | Variable x -> true |- Class.get_variable_ty x

