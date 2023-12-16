open Solvers.Signature
open Utils.String_module
open Utils.List_module


module Solver : Solver = struct
  let parse input = 
    let lines = Str.split (Str.regexp "\n") input in
    List.map stringToList lines
  let rocks_and_walls map x =
    let rec aux y rocks walls =
      if y < (List.length map) 
      then begin
          if (List.nth (List.nth map y) x) = '#' 
          then begin 
            aux (y+1) rocks ((x, y) :: walls) end
          else if (List.nth (List.nth map y) x) = 'O' 
          then begin 
            aux (y+1) ((x, y) :: rocks) walls end
          else begin 
            aux (y+1) rocks walls end
          end
      else rocks, walls
    in
    aux 0 [] []
  
  let sort_list list =
    List.sort (fun a b -> if fst a = (fst b) then (snd a) - snd(b) else (fst a) - (fst b)) list
  let update_array arr i x =
    arr.(i) <- x;
    arr
  let rec clear_smaller ls x =
    match ls with
    | [] -> []
    | h :: t -> if fst h < x
                then clear_smaller t x
                else ls
  let check_walls walls x y =
    let rec aux walls y prev =
      match walls with
      | [] -> prev, []
      | h :: t -> if snd h < y && fst h = x
                  then aux t y (Some h)
                  else prev, walls in 
    aux walls y None
  let move x_max rocks walls =
    let rocks = sort_list rocks in
    let walls = sort_list walls in 
    let floor = (Array.make (x_max+1) (-1)) in

    let rec aux floor rocks walls new_rocks =
      match rocks with
      | [] -> new_rocks
      | (x, y) :: t -> 
        let walls = clear_smaller walls x in 
        let (smaller_wall, rest_walls) = check_walls walls x y in
        match smaller_wall with
        | Some wall ->
          let floor = update_array floor x (snd wall + 1) in
          aux floor t rest_walls ((x, floor.(x)) :: new_rocks)
        | None ->
          let floor = update_array floor x (floor.(x) + 1) in
          aux floor t walls ((x, floor.(x)) :: new_rocks) in
        
    aux floor rocks walls []
  
  let calculate_score rocks x_max =
    let rec aux rocks acc =
      match rocks with
      | [] -> acc
      | h :: t -> aux t (acc + x_max - (snd h)) in 
    aux rocks 0
  let task1 data =
    let map = parse data in
    let x_range = (range 0 (List.length (List.hd map)) 1) in 
    let ls_of_rock_and_walls = (List.map (rocks_and_walls map) x_range) in
    let (rocks, walls) = List.fold_left (fun acc (x,y) -> (fst acc @ x, snd acc @ y)) ([], []) ls_of_rock_and_walls  in

    let new_rocks = move (List.length (List.hd map)) rocks walls in

    string_of_int (calculate_score new_rocks (List.length map))
  
  let spin rocks dir (x_max, y_max) =
    let rec aux rocks acc =
      match rocks with
      | [] -> acc
      | (x, y) :: t -> let new_coord =
                        match dir with
                        | 'N' -> (y_max - y - 1, x)
                        | 'W' -> (x_max - y - 1, x)
                        | 'S' -> (y_max - y - 1, x)
                        | 'E' -> (x_max - y - 1, x)
                        | _ -> failwith "Unknown direction" in 
                      aux t (new_coord :: acc) in
    aux rocks []
  let cycle rocks walls (x_max, y_max) memory =
    let rec aux rocks walls dirs mem f =
      match dirs with
      | [] -> if f = -1
              then rocks, mem, f
              else rocks, mem, f
      | dir :: t -> 
                let max_c = if dir = 'S' || dir = 'N' then x_max else y_max in
                let new_rocks = sort_list (spin (move max_c rocks walls) dir (x_max, y_max)) in 
                let found = 
                  if f = -1 && dir = 'E'
                  then find mem new_rocks
                  else f in
                let new_walls = spin walls dir (x_max, y_max) in
                let new_mem = if dir = 'E' then mem @ [new_rocks] else mem in 
                aux new_rocks new_walls t new_mem found in
    aux rocks walls ['N'; 'W'; 'S'; 'E'] memory (-1)

  let task2 data _part1 =
    let map = parse data in
    let x_range = (range 0 (List.length (List.hd map)) 1) in 
    let ls_of_rock_and_walls = (List.map (rocks_and_walls map) x_range) in
    let (rocks, walls) = List.fold_left (fun acc (x,y) -> (fst acc @ x, snd acc @ y)) ([], []) ls_of_rock_and_walls  in
    let rec aux rocks i n memory =
      if i < n
      then begin
            let (new_rocks, mem, f) = (cycle rocks walls (List.length (List.hd map), List.length map) memory) in 
            if f = -1 then aux new_rocks (i+1) n mem
            else List.nth mem ((f + ((n-i) mod (i-f))-1))
          end
      else rocks in
    let new_rocks = sort_list (aux rocks 0 1000000000 []) in
    string_of_int (calculate_score new_rocks (List.length map))
end;;