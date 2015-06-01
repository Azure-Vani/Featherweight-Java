open Jtype
open State

exception Runtime_err of string

let ty_root_object = "Object"

let add_super_to_obj (super:j_value) obj = match obj with
    | Object (_, _, Object _) -> 
            raise @@ Runtime_err "Duplicated invocation of super"

    | Object (n, xs, _) -> 
            Object (n, xs, super)

let add_field_to_obj ~name ~value obj = match obj with
    | Object (n, xs, s) -> Object (n, (name, value) :: List.remove_assoc name xs, s)

let rec new_class name values = 
    let context = Class.get_cons_params_name name <+> values in
    let empty = (Object ("name", [], Variable "Object")) in
    let statements = Class.get_cons_statements name 
    in List.fold_left (fun obj stat -> 
        match stat with
            | `Super terms -> 
                    (Class.get_super_class name ><= obj) (fun super ->
                        let super_ins = new_class super (List.map (run_eval context) terms)
                        in add_super_to_obj super_ins obj)
            | `Assign (name, term) ->
                    let value = run_eval context term 
                    in add_field_to_obj ~name ~value obj
    ) empty statements
    
and (><=) super alt f = 
    if super = ty_root_object then alt else f super

and run_eval context term = fst @@ eval term context

and apply f vx vy = match (vx, vy) with
    | (Primary v1, Primary v2) -> Primary (f v1 v2)
    | _ -> raise (Runtime_err "Type error at binary operator")

and (?-) obj = match obj with
    | Object (name, _, _) -> name

and (|.) obj name = match obj with
    | Object (_, fields, _) -> List.assoc name fields

and (<+>) xs ys = List.combine xs ys

and eval = function
    | Plus (t1, t2) ->
            eval t1 >>= fun v1 ->
                eval t2 >>= fun v2 ->
                    return @@ apply (+) v1 v2

    | Minus (t1, t2) ->
            eval t1 >>=  fun v1 ->
                eval t2 >>= fun v2 ->
                    return @@ apply (-) v1 v2

    | Times (t1, t2) ->
            eval t1 >>= fun v1 ->
                eval t2 >>= fun v2 ->
                    return @@ apply ( * ) v1 v2

    | Cast (_, t) ->
            eval t >>= fun v ->
                return @@ v

    | New (cl, terms) ->
            mapM eval terms >>= fun values ->
                return @@ new_class cl values

    | Invoke (term, func, params) ->
            eval term >>= fun obj ->
                mapM eval params >>= fun values ->
                    put (Class.get_method_params_name ?-obj func <+> values) >>= fun _ ->
                        add ("this", obj) >>= fun _ ->
                            eval @@ Class.get_method_return_term ?-obj func

    | Access (term, field) ->
            eval term >>= fun obj ->
                return (obj |. field)

    | Value v -> return v

