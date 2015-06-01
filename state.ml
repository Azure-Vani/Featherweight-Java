(* define the state type *)
type ('s, 'r) state = 's -> ('r * 's)

(* combine two state tranfer function *)
let (>>=) m f = fun s -> let (x, s') = m s in f x s'

(* return the result *)
let return a = fun s -> (a, s)

(* put a state and leave the result unit alone *)
let put s = fun _ -> ((), s)

(* get the state as result *)
let get = fun s -> (s, s)

(* add an item to the context *)
let add item = fun s -> ((), item :: s)

(* concatenate states in a list *)
let sequence l = fun c -> (List.fold_left (fun xs a -> fst (a c) :: xs) [] l, c)

(* lift the function f to list env *)
let mapM f = Core.Fn.compose sequence @@ List.map f

