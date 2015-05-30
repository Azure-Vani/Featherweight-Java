type adt = {
    classes: j_class list, (* java classes declarations *)
    term: term (* the evaluated term *)
}

type j_type = string

type j_declar = { (* the type * identifier pair *)
    ty: j_type,
    iden: string
}

type j_class = {
    name: string,
    super: string, (* the super class *)
    fields: j_declar list, (* the files of this class, type * identifier *)
    cons: j_cons_method, (* the constructor method *)
    methods: j_method list (* the methods *)
}

type j_method = {
    return_type: j_type, 
    params: j_declar list,
    return_stat: term
}

type j_cons_method = {
    params: j_declar list,
    super_params: term list,
    init: string * term list (* the initialization statements *)
}

type term = 
    | Plus of term * term
    | Minus of term * term
    | Times of term * term
    | Cast of string * term
    | New of string * (term list)
    | Invoke of term * string * (term list)
    | Value of j_value

type j_value = 
    | Primary of int
    | Object of j_object (* only occurs at runtime *)

type j_object = string * j_value list
