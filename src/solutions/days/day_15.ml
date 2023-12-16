open Solvers.Signature
open Utils.String_module

type lens = {label: string; focal: int}

let update_array arr i x =
  arr.(i) <- x;
  arr

module Solver : Solver = struct
  let parse input = List.map stringToList (String.split_on_char ',' input)
  let rec hash_algo characters acc =
    match characters with
    | [] -> acc
    | h :: t -> hash_algo t ((acc + (int_of_char h)) * 17 mod 256)
  let task1 data =
    let inp = parse data in 
    string_of_int (List.fold_left (fun acc x -> acc + (hash_algo x 0)) 0 inp)
  
  let add_lens lst label_value = 
    let rec aux lst label value acc =
      match lst with
      | [] -> List.rev ({label=label;focal=(int_of_string value)} :: acc)
      | h :: t -> if h.label = label
                  then List.rev ((List.rev t) @ ({label=label;focal=(int_of_string value)} :: acc))
                  else aux t label value (h :: acc) in
    aux lst (List.hd label_value) (List.nth label_value 1) []
  
  let remove_label lst label =
    let rec aux lst label acc =
      match lst with
      | [] -> List.rev acc
      | h :: t -> if h.label = label
                  then aux t label acc
                  else aux t label (h :: acc) in 
    aux lst label []
  
  let go_through_instruct boxes instructions =
    let rec aux boxes instructions =
      match instructions with
      | [] -> boxes
      | h :: t -> if String.contains h '='
                  then let label_value = String.split_on_char '=' h in
                        let box = hash_algo (stringToList (List.hd label_value)) 0 in 
                        let boxes = update_array boxes box (add_lens boxes.(box) label_value) in
                        aux boxes t
                  else let label = List.hd (String.split_on_char '-' h) in
                        let box = hash_algo (stringToList label) 0 in 
                        let boxes = update_array boxes box (remove_label boxes.(box) label) in 
                        aux boxes t in 
    aux boxes instructions
  
  let calculate_focal boxes =
    let acc = ref 0 in
    let rec through_lenses lenses i box_n acc =
      match lenses with
      | [] -> acc
      | h :: t -> through_lenses t (i+1) box_n (acc + box_n * i * h.focal) in
    for i = 0 to Array.length boxes - 1 do
      acc := !acc + through_lenses boxes.(i) 1 (i+1) 0
    done;
    !acc
  
  let task2 data _part1 =
    let inp = (String.split_on_char ',' data) in
    let boxes = Array.make 256 [] in 
    let new_boxes = go_through_instruct boxes inp in 

    string_of_int (calculate_focal new_boxes)
end;;