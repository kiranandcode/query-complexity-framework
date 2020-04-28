open Core
let rec i_to_j i j =
  if i < j then i :: (i_to_j (i + 1) j) else []

let rec rem x ls = match ls with
  | [] -> []
  | h :: t when x = h -> rem x t
  | h :: t -> h :: rem x t

let to_triple n index =
  let rem = index % (n * n) in
  let x = (index - rem ) / (n * n) in
  let z = rem % n in
  let y = (rem - z) / n in
  (x,y,z)

module Int_triple =  struct
  type t = (int * int * int)
  let sexp_of_t (v: t) = Tuple3.sexp_of_t (Int.sexp_of_t) (Int.sexp_of_t) (Int.sexp_of_t) v
  let compare v1 v2 = Tuple3.compare ~cmp1:(Int.compare) ~cmp2:(Int.compare) ~cmp3:(Int.compare) v1 v2
  let hash (s1, s2, s3) = (Int.hash s1 + Int.hash s2 * 65599 + Int.hash s3 * 131198) land 0x3FFFFFFF
end

module Int_tuple = struct
  type t = (int * int)
  let sexp_of_t (v: t) = Tuple2.sexp_of_t (Int.sexp_of_t) (Int.sexp_of_t)  v
  let compare v1 v2 = Tuple2.compare ~cmp1:(Int.compare) ~cmp2:(Int.compare)  v1 v2
  let hash (s1, s2) = (Int.hash s1 + Int.hash s2 * 65599) land 0x3FFFFFFF
end

