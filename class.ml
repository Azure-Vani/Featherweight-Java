open Jtype

(* pravite variable and functions *)
let classes = ref [] 
let class_name_eq x (c:j_class) = c.name = x

(* set functions *)
let set_classes (c:j_class list) =
    classes := c

(* predicated functions *)
let is_class name =
    List.exists (class_name_eq name) !classes

(* get functions *)
let get_class name = 
    List.find (class_name_eq name) !classes

let get_cons name = 
    let items = (get_class name).items
    in match List.find (function `Cons _ -> true | _ -> false) items with
        | `Cons x -> x

let get_field name var = 
    let items = (get_class name).items
    in match List.find (function `Field x when x.iden = var -> true | _ -> false) items with
        | `Field x -> x

let get_method name f = 
    let items = (get_class name).items
    in match List.find (function `Method x when x.name = f -> true | _ -> false) items with
        | `Method x -> x

(* class meta info *)
let get_super_class x = 
    (get_class x).super 

(* constructor relevant *)
let get_cons_params x = 
    (get_cons x).params

(* method relevant *)
let get_method_params name f = 
    (get_method name f).params

let get_method_returnty name f =
    (get_method name f).return_type

(* field relevant *)
let get_field_ty name var = 
    (get_field name var).ty
