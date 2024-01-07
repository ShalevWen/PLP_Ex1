let pi = 3.14159;;
type shape = Circle of float | Square of float | Rectangle of float * float;;

let area s = match s with
  | Circle r -> pi *. r *. r
  | Square s -> s *. s
  | Rectangle (l, w) -> l *. w;;

let rec total_area l = match l with
  | [] -> 0.0
  | h::t -> area h +. total_area t;;