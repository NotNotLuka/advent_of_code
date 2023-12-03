open Solvers.Signature
open Utils.String_module

let adjacent = [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)]
let digits = "0123456789"
type number = {value: int; x_range: int * int}

module Solver : Solver = struct
  let rec remove_number_pos x y x_max number_pos =
    if x <= x_max
    then let new_one = List.filter (fun pos -> pos <> (x, y)) number_pos in 
      remove_number_pos (x+1) y x_max new_one
    else number_pos;;
  
  let get_number line i =
    let rec aux line i inc acc =
      if i < 0 || i >= String.length line then acc
      else
        if String.contains digits line.[i] then aux line (i + inc) inc (line.[i] :: acc)
        else acc in 
    let first = (aux line (i - 1) (-1) []) in
    let second = List.rev (aux line (i + 1) 1 []) in
    {value = int_of_string (String.of_seq (List.to_seq (first @ [line.[i]] @ second)));
     x_range = (i - (List.length first), i + (List.length second))};;

  let get_adjacent_n data x y =
    let rec aux adj data x y acc =
      match adj with
      | [] -> acc
      | h :: t ->
        if not(List.length data <= (y + snd h) 
                || (y + snd h) < 0 || (x + fst h) < 0 
                || String.length (List.nth data (y + snd h)) <= (x + fst h))
        then
          if String.contains digits (List.nth data (y + snd h)).[x + fst h]
          then aux t data x y ((x + fst h, y + snd h) :: acc)
          else aux t data x y acc
        else aux t data x y acc in
    let out = aux adjacent data x y [] in 
    out;;
  
  let rec signs_to_numbers data sign_pos number_pos =
    match sign_pos with
    | [] -> number_pos
    | h :: t -> 
      signs_to_numbers data t ((get_adjacent_n data (fst h) (snd h)) @ number_pos);;

  let rec loop_through_char line x y sign_pos =
    match line with
    | [] -> sign_pos
    | h :: t -> 
      if not (String.contains digits h) && h <> '.'
      then loop_through_char t (x+1) y ((x, y) :: sign_pos)
      else loop_through_char t (x+1) y sign_pos;;
  
  let rec loop_through_lines lines y all_sign_pos intact_data =
    match lines with
    | [] -> 
      signs_to_numbers intact_data all_sign_pos []
    | h :: t -> loop_through_lines t (y+1) ((loop_through_char (stringToList h) 0 y []) @ all_sign_pos) intact_data;;
  

  let get_all_numbers data =
    let numbers_positions = loop_through_lines data 0 [] data in
    let rec aux data numbers_positions acc =
      match numbers_positions with
      | [] -> acc
      | h :: t -> 
        let number = get_number (List.nth data (snd h)) (fst h) in
        let new_number_pos = remove_number_pos (fst number.x_range) (snd h) (snd number.x_range) t in
        aux data new_number_pos (number.value + acc) in
    aux data numbers_positions 0;;

  let task1 data =
    string_of_int (get_all_numbers (parseData data))
  
  let rec loop_through_char2 line x y sign_pos =
    match line with
    | [] -> sign_pos
    | h :: t -> 
      if h = '*'
      then loop_through_char2 t (x+1) y ((x, y) :: sign_pos)
      else loop_through_char2 t (x+1) y sign_pos;;
  let rec numbers_from_signs sign_pos numbers_pos intact_data =
    match sign_pos with
    | [] -> numbers_pos
    | h :: t -> numbers_from_signs t ((get_adjacent_n intact_data (fst h) (snd h)) :: numbers_pos) intact_data;;
  
  let rec loop_through_lines2 lines y sign_pos =
    match lines with
    | [] -> sign_pos
    | h :: t -> 
            let sp = (loop_through_char2 (stringToList h) 0 y []) in
            loop_through_lines2 t (y+1) (sp @ sign_pos);;
  
  let rec gears data numbers_positions acc =
    match numbers_positions with
    | [] -> 
            if List.length acc = 2 
            then (List.nth acc 0) * (List.nth acc 1) else 0
    | h :: t -> 
      let number = get_number (List.nth data (snd h)) (fst h) in
      let new_number_pos = remove_number_pos (fst number.x_range) (snd h) (snd number.x_range) t in
      gears data new_number_pos (number.value :: acc)

  let get_all_numbers2 data =
    let sign_positions = loop_through_lines2 data 0 [] in
    let numbers_positions = numbers_from_signs sign_positions [] data in
    let rec aux data numbers_positions acc  =
      match numbers_positions with
      | [] -> acc
      | h :: t -> 
        let gear_number = (gears data h []) in
        aux data t (gear_number + acc) in
    aux data numbers_positions 0;;
  let task2 data _part1 =
    string_of_int (get_all_numbers2 (parseData data))
end;;