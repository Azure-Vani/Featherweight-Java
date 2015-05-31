type adt = {
    classes: j_class list; (* java classes declarations *)
    term: term (* the evaluated term *)
} 

and j_type = string

and j_declar = { (* the type * identifier pair *)
    ty: j_type;
    iden: string
}

and j_class = {
    name: string;
    super: string;
    items: [ (* including fields, constructor and methods *)
        `Field of j_declar | 
        `Cons of j_cons_method | 
        `Method of j_method 
    ] list
}

and j_method = {
    name: string;
    return_type: j_type; 
    params: j_declar list;
    return_stat: term
}

and j_cons_method = {
    params: j_declar list;
    statements: [
        `Super of term list | 
        `Assign of string * term 
    ] list
}

and term = 
    | Plus of term * term
    | Minus of term * term
    | Times of term * term
    | Cast of string * term
    | New of string * (term list)
    | Invoke of term * string * (term list)
    | Access of term * string
    | Value of j_value

and j_value = 
    | Primary of int
    | Variable of string

