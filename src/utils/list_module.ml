let transpose ls = 
  let rec aux ls x acc  =
    match ls with
    | [] -> List.rev acc
    | h :: t -> 
      aux t  x (List.nth h x :: acc)
  in
  let rec construct ls x acc =
    if x < List.length (List.hd ls) then
      begin
      construct ls (x + 1) (aux ls x [] :: acc)
      end
    else List.rev acc in 
  construct ls 0 []

let change ls index x =
  let rec aux ls i index x acc =
    match ls with
    | [] -> acc
    | h::t -> if i = index - 1 then ((List.rev (List.tl t)) @ (x :: (h :: acc))) else aux t (i+1) index x (h::acc) in
  if index = 0 then x :: (List.tl ls) else
  List.rev (aux ls 0 index x [])

let find ls x =
  let rec aux ls x i =
    match ls with
    | [] -> -1
    | h::t -> if h = x then i else aux t x (i+1) in
  aux ls x 0

let slice ls x1 x2 =
  let rec aux ls x1 x2 x acc =
    if x1 <= x && x < x2
    then aux (List.tl ls) x1 x2 (x+1) ((List.hd ls)::acc)
    else if x < x1 then aux (List.tl ls) x1 x2 (x+1) acc
    else acc
  in
  List.rev (aux ls x1 x2 0 [])

let range x1 x2 inc =
  let rec aux x acc =
    if x < x2 then aux (x+inc) (x::acc)
    else acc
  in
  List.rev (aux x1 [])