open Jtype

(* pravite variable and functions *)
let classes:j_class list ref = ref [] 
let context:(string * j_type) list ref = ref []

let class_name_eq x (c:j_class) = c.name = x

let set_context c =
    context := c

(* set functions *)
let set_classes (c:j_class list) =
    classes := c

(* predicated functions *)
let is_class name =
    List.exists (class_name_eq name) !classes

let get_variable_ty x = 
    List.assoc x !context

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
let get_cons_params_type x = 
    List.map (fun x -> x.ty) (get_cons x).params

let get_cons_params_name x =
    List.map (fun x -> x.iden) (get_cons x).params
 
let get_cons_statements x =
    (get_cons x).statements

(* method relevant *)
let get_method_params_type name f = 
    List.map (fun x -> x.ty) (get_method name f).params

let get_method_params_name name f =
    List.map (fun x -> x.iden) (get_method name f).params

let get_method_returnty name f =
    (get_method name f).return_type

let get_method_return_term name f =
    (get_method name f).return_stat

(* field relevant *)
let get_field_ty name var = 
    (get_field name var).ty
