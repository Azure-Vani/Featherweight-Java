%{
    open Core.Caml
    
    let make_up_class ~name ~super ~body = 
        { name=name, 
          super=super, 
          fields = body.fields, 
          cons = body.cons, 
          methods = body.methods
        }

    let generate_class_body_of l = 
        {
            fields = List.map (function 
                |`Filed x -> x
            ) @@ List.filter (function 
                | `Field _ -> true
                | _ -> false
            ) l,
            cons = List.find (function 
                | `Cons _ ->  true
                | _ -> false
            ) l,
            methods = List.map (function 
               | `Method f -> f
            ) @@ List.filter (function
                | `Method _ -> true
                | _ -> false
            ) l
        }

    let generate_cons_body_of s = 
        let super_params = List.find (function `Super _ -> ture | _ -> false) s in
        let init = List.map (function | `Assign x -> x)
            @@ List.filter (function `Assign _ -> true | _ -> false) s
        in (super_params, init)
%}

%token <int> INT
%token <string> IDEN
%token <j_type> TYPE
%token CLASS
%token RETURN
%token THIS
%token EXTENDS
%token SUPER
%token NEW

%token <int> DIGIT
%token PLUS
%token MINUS
%token TIMES

%token L_BRACE
%token R_BRACE
%token L_PARENTHESIS
%token R_PARENTHESIS
%token COMMA
%token SEMICOLON
%token DOT
%token EQ
%token EOF

%left PLUS MINUS
%left TIMES
%nonassoc INVOKE
%nonassoc CAST
%start <Types.adt> prog
%%

prog:
    | c = class_list; t = expr; EOF {{classes = c, term = t}} 
    ;

class_list: 
    | c = list(class) {c}
    ;

class:
    CLASS; n=IDEN; EXTENDS; s=IDEN; L_BRACE; b = class_body; R_BRACE {make_up_class ~name:n ~super:s ~body:b}
    ;

class_body:
    l = list(class_body_item); {generate_class_body_of l}
    ;

declar:
    | t = TYPE; x = IDEN {{ty = t, iden = x}}
    ;

declar_params:
    l = separated_list(COMMA, declar) {l}
    ;

invok_params:
    l = separated_list(COMMA, expr) {l}
    ;

class_body_item:
    | d = declar; COMMA {`Filed d}

    | IDEN; L_PARENTHESIS; l = declar_params; R_PARENTHESIS; 
        L_BRACE; s = separated_list(SEMICOLON, statement); SEMICOLON; R_BRACE 
        {
            let super_params, init = generate_cons_body_of s
            in `Cons {params = l, super_params = super_params, init = init}
        }

    | t = TYPE; IDEN; L_PARENTHESIS; l = declar_params; R_PARENTHESIS;
        L_BRACE; RETURN; e = expr; SEMICOLON; R_BRACE; {`Method {return_type = t, params = l, return_stat = e}}

    ;

statement:
    | SUPER; L_PARENTHESIS; l = invok_params; R_PARENTHESIS {`Super l}
    
    | THIS; DOT; v = IDEN; EQ; e = expr; {`Assign (v, e)}
    ;

expr:
    | L_PARENTHESIS; e = expr; R_PARENTHESIS  {e}

    | e1 = expr; PLUS; e2 = expr {Plus (e1, e2)}

    | e1 = expr; MINUS; e2 = expr {Minus (e1, e2)}

    | e1 = expr; TIMES; e2 = expr {Times (e1, e2)} 

    | L_PARENTHESIS; v = IDEN; R_PARENTHESIS; e = expr; {Cast (v, e)} %prec CAST

    | NEW; v = IDEN; L_PARENTHESIS; l = invok_params; R_PARENTHESIS {New (v, l)}

    | e = expr; DOT; f = IDEN; L_PARENTHESIS; l = invok_params; R_PARENTHESIS {Invoke (e, f, l)} %prec INVOKE

    | x = DIGIT {Value (Primary x)}

