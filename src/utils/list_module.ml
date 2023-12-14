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