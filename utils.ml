open Core.Std
open Core.Caml
open Jtype

let gen_param_list = Fn.compose (String.concat ", ") (List.map (function {ty = ty; iden = iden} -> ty ^ " " ^ iden))

let traverse_term _ = "term"

let traverse_stat padding = function
    | `Super x -> 
            printf "%ssuper(%s);\n" padding (String.concat ", " @@ List.map traverse_term x)

    | `Assign (iden, t) -> 
            printf "%s%s = %s;\n" padding iden @@ traverse_term t

let traverse_item padding = function
    | `Field {ty = ty; iden = iden} -> printf "%s%s %s;\n" padding ty iden

    | `Cons  {params = params; statements = statements} -> 
            printf "%sconstructor(%s) {\n" padding (gen_param_list params);
            ListLabels.iter ~f:(traverse_stat (padding ^ "\t")) statements;
            printf "%s}\n" padding

    | `Method {name = name;return_type=ty; params = params; return_stat = return_stat} -> 
            printf "%s%s %s(%s) {\n" padding ty name (gen_param_list params);
            printf "%sreturn %s;\n" (padding ^ "\t") @@ traverse_term return_stat;
            printf "%s}\n" padding

let traverse_class padding {name = name; super = super; items = items} =
    printf "%sclass %s extends %s {\n" padding name super;
    ListLabels.iter ~f:(traverse_item (padding ^ "\t")) items;
    printf "%s}\n" padding

let traverse_adt padding {classes = classes; term = _}  = 
    ListLabels.iter ~f:(fun x -> traverse_class padding x) classes

let rec print_term = function 
    | Primary x -> printf "%d\n" x
    | Variable st -> printf "%s\n" st
    | Object ("Object", _, _) -> printf "Object\n"
    | Object (name, fields, super) ->
            printf "%s->{\n" name;
            List.iter (function
                | (field, value) -> printf "%s: " field;
                                    print_term value
            ) fields;
            printf "}\n";
            printf "super is: \n";
            print_term super

