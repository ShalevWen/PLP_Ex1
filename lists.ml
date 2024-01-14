let rec sum_list l = match l with
  | [] -> 0
  | h::t -> h + (sum_list t);;

let rec compress l = match l with
  | [] -> []
  | h::t -> (match t with
    | [] -> [h]
    | h2::t2 -> 
      if h = h2 then (compress t)
      else h::(compress t)
  );;