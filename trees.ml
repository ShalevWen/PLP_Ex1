type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;


let rec insert x t =
  match t with
  | Empty -> Node (x, Empty, Empty)
  | Node (y, l, r) ->
    if x < y then Node (y, insert x l, r)
    else if x > y then Node (y, l, insert x r)
    else t;;

let rec aux l acc =
    match l with
    | [] -> acc
    | h::t -> aux t (insert h acc);;

let construct l =
  aux l Empty;;