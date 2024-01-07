let rec sum_list = fun l -> match l with
  | [] -> 0
  | h::t -> h + (sum_list t);;

let rec compress = fun l -> match l with
  | [] -> []
  | h::[] -> [h]
  | h::t -> (match t with
    | [] -> [h]
    | h2::t2 -> 
      if h = h2 then (compress t)
      else h::(compress t)
  );;