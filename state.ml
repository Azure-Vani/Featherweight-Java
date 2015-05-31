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
