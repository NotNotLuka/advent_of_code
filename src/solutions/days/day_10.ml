open Solvers.Signature
open Utils.String_module

let rec make_list n =
  if n <= 0 then []
  else '.' :: make_list (n - 1)
let print_char_list_list lst =
  List.iter (fun inner_list ->
    List.iter (fun c -> print_char c) inner_list;
    print_newline ()
  ) lst
module Solver : Solver = struct
  let parse data = 
    let lines = String.split_on_char '\n' data in 
    let out = (List.map (fun l -> ('.' :: (stringToList l)) @ ['.']) lines) in
    let out = (make_list (List.length (List.hd out))) :: out @ [(make_list (List.length (List.hd out)))] in
    let start = List.map (fun l -> find_char l 'S') lines in
    let start_coord = List.hd (fst (List.fold_left (fun (acc, i) x -> if x = -1 then (acc, i+1) else ((x,i) :: acc, i+1)) ([], 0) start)) in
    Array.of_list (List.map Array.of_list out), (fst start_coord + 1, snd start_coord + 1)

  let validate (prev_x, prev_y) map (x, y)  = 
    if x = prev_x && y = prev_y then false
    else if x < 0 || y < 0 then false
    else if x >= Array.length map.(0) || y >= Array.length map then false
    else if map.(y).(x) = '.' then false
    else true

  let next_moves (prev_x, prev_y) map (x, y) = 
    let aux (x, y) =
      match map.(y).(x) with
      | '|' -> [(x, y-1); (x, y+1)]
      | '-' -> [(x-1, y); (x+1, y)]
      | 'L' -> [(x+1, y); (x, y-1)]
      | 'J' -> [(x-1, y); (x, y-1)]
      | '7' -> [(x-1, y); (x, y+1)]
      | 'F' -> [(x+1, y); (x, y+1)]
      | _ -> failwith "Invalid move" in 
    (List.filter (validate (prev_x, prev_y) map) (aux (x, y)) )

  let find_loop map start = 
    let start_moves (x, y) map = 
      let options = [(x, y-1); (x, y+1); (x-1, y); (x+1, y)] in
      let valid = List.filter (validate (-1, -1) map) options in
      List.hd (List.filter (fun (x1,y1) -> List.mem (x, y) (next_moves (-1, -1) map (x1, y1))) valid) in

    let rec aux (x, y) (prev_x, prev_y) path = 
      let next = List.hd (next_moves (prev_x, prev_y) map (x, y)) in 
      if map.(snd next).(fst next) = 'S' then path
      else aux (next) (x, y) (next :: path) in
    let first_move = (start_moves start map) in
    aux first_move start [first_move]

  let task1 data =
    let input = parse data in 
    let loop = find_loop (fst input) (snd input) in
    if (List.length loop) mod 2 = 0 then string_of_int (List.length loop / 2)
    else string_of_int ((List.length loop) / 2 + 1)
  
  let find_nests map loop =
    let rec through_loop map loop (x_prev, y_prev) left right intact_loop =
      match loop with
      | [] -> left, right
      | (x, y) :: t -> 
        if (x - x_prev) = 1
          then
          begin
          let new_left = if not(List.mem (x,y-1) intact_loop) && not (List.mem (x,y-1) left) 
            then ((x, y-1) :: left) else left in
          let new_right = if not(List.mem (x,y+1) intact_loop) && not (List.mem (x,y+1) right) 
            then ((x, y+1) :: right) else right in
          let new_right = if map.(y).(x) = 'J'  && not(List.mem (x+1,y) intact_loop)  && not (List.mem (x+1,y) new_right)  
            then ((x+1, y)::new_right) else new_right in
          let new_left = if map.(y).(x) = '7'  && not(List.mem (x+1,y) intact_loop)  && not (List.mem (x+1,y) new_left)  
            then ((x+1, y)::new_left) else new_left in

          through_loop map t (x, y) new_left new_right intact_loop
          end
        else if (x - x_prev) = -1
            then
            begin
            let new_left = if not(List.mem (x,y+1) intact_loop) && not (List.mem (x,y+1) left) 
              then ((x, y+1) :: left) else left in
            let new_right = if not(List.mem (x,y-1) intact_loop) && not (List.mem (x,y-1) right) 
              then ((x, y-1) :: right) else right in
            let new_left = if map.(y).(x) = 'L' && not(List.mem (x-1,y) intact_loop)  && not (List.mem (x-1,y) left)  
              then ((x-1, y)::new_left) else new_left in
            let new_right = if map.(y).(x) = 'F' && not(List.mem (x-1,y) intact_loop)  && not (List.mem (x-1,y) new_right)  
              then ((x-1, y)::new_right) else new_right in
            through_loop map t (x, y) new_left new_right intact_loop
            end
        else if (y - y_prev) = 1
          then
          begin
          let new_left = if not(List.mem (x+1,y) intact_loop) && not (List.mem (x+1,y) left) 
            then ((x+1, y) :: left) else left in
          let new_right = if  not(List.mem (x-1,y) intact_loop) && not (List.mem (x-1,y) right) 
            then ((x-1, y) :: right) else right in
          let new_left = if map.(y).(x) = 'F' && not(List.mem (x,y+1) intact_loop)  && not (List.mem (x,y+1) left)  
            then ((x, y+1)::new_left) else new_left in
          let new_right = if map.(y).(x) = '7' && not(List.mem (x,y+1) intact_loop)  && not (List.mem (x,y+1) new_right)  
            then ((x, y+1)::new_right) else new_right in
          through_loop map t (x, y) new_left new_right intact_loop
          end
        else if (y - y_prev) = -1
          then
          begin
          let new_left = if not(List.mem (x-1,y) intact_loop)  && not (List.mem (x-1,y) left) 
            then ((x-1, y) :: left) else left in
          let new_right = if not(List.mem (x+1,y) intact_loop) && not (List.mem (x+1,y) right) 
            then ((x+1, y) :: right) else right in
          let new_left = if map.(y).(x) = 'J' && not(List.mem (x,y-1) intact_loop)  && not (List.mem (x,y-1) left)  
            then ((x, y-1)::new_left) else new_left in
          let new_right = if map.(y).(x) = 'L' && not(List.mem (x,y-1) intact_loop)  && not (List.mem (x,y-1) new_right)  
            then ((x, y-1)::new_right) else new_right in
          through_loop map t (x, y) new_left new_right intact_loop
          end
        else failwith "invalid move" in
    through_loop map (List.tl loop) (List.hd loop) [] [] loop
  
  let rec add_with_check old to_be_added acc =
    match to_be_added with
    | [] -> acc
    | h :: t -> if List.mem h old then add_with_check old t acc else add_with_check old t (h :: acc)
  
  let find_adjacent loop found map =
    let rec aux loop go_to been acc =
      match go_to with
      | [] -> acc
      | (x, y) :: t -> 
        let options = [(x, y-1); (x, y+1); (x-1, y); (x+1, y)] in
        let options =  List.filter (fun (x, y) -> 0 <= x && 0 <= y && x < (Array.length map.(0)) && y < (Array.length map)) options in
        let valid = List.filter (fun (x, y) -> not (List.mem (x, y) been) && not(List.mem (x,y) loop)) options in
        aux loop (add_with_check t valid t) ((x, y) :: been) (add_with_check acc valid acc) in
    aux loop found found found
  
  let which_one lst  =
    let rec aux lst =
      match lst with
      | [] -> true
      | (x, y) :: t -> if x = 0 && y = 0 then false else aux t in
    aux lst
    let replace_s loop map start =
      let aux (x, y) =
        if List.mem (x+1, y) loop && List.mem (x, y-1) loop then map.(y).(x) <- 'L'
        else if List.mem (x-1, y) loop && List.mem (x, y+1) loop then map.(y).(x) <- 'J'
        else if List.mem (x-1, y) loop && List.mem (x, y-1) loop then map.(y).(x) <- '7' 
        else if List.mem (x+1, y) loop && List.mem (x, y-1) loop then map.(y).(x) <- 'F'
      in
      aux start;
      map
  let task2 data _part1 =
    let input = parse data in 
    let loop = ((snd input) :: (find_loop (fst input) (snd input))) in
    let map = replace_s loop (fst input) (snd input) in
    let found_nests = find_nests map loop in
    let left_nests = find_adjacent loop (fst found_nests) map in

    let right_nests = find_adjacent loop (snd found_nests) map in
    let final = if which_one right_nests then right_nests else left_nests in 
    
    string_of_int (List.length final)
end;;
