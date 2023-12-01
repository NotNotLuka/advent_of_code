open Solvers.Signature

let parse_data data = String.split_on_char '\n' data;;
let splitStringToList str = str |> String.to_seq |> List.of_seq;;
let combineCharacters a b = String.make 1 a ^ String.make 1 b;;
let appendChar str char = str ^ (String.make 1 char);;
let digits = "0123456789";;

type fake_digits = {
  name: string;
  value: char;
}
let fake_digits = [
  {name = "one"; value = '1'};
  {name = "two"; value = '2'};
  {name = "three"; value = '3'};
  {name = "four"; value = '4'};
  {name = "five"; value = '5'};
  {name = "six"; value = '6'};
  {name = "seven"; value = '7'};
  {name = "eight"; value = '8'};
  {name = "nine"; value = '9'};
];;

module Solver : Solver = struct
  let naloga1 data =
    let rec aux2 line acc2 =
      match line with
      | [] -> 
        (match List.length acc2 with
        | 0 -> 0
        | 1 -> int_of_string (combineCharacters (List.hd acc2) (List.hd acc2))
        | _ -> int_of_string (combineCharacters (List.hd(List.rev acc2)) (List.hd acc2)))
      | h :: t -> if String.contains digits h then aux2 t (h :: acc2) else aux2 t acc2 in 
      
    let rec aux parsed acc =
      match parsed with
      | [] -> List.fold_left (+) 0 acc
      | h :: t -> aux t ((aux2 (splitStringToList h) []) :: acc) in

    string_of_int (aux (parse_data data) []);;

  let naloga2 data _part1 =
    let substring_in_string s1 s2 =
      (* source: https://stackoverflow.com/questions/8373460/substring-check-in-ocaml *)
      let re = Str.regexp_string s2
      in
          try ignore (Str.search_forward re s1 0); true
          with Not_found -> false in
    let rec which_digit str fake_digits =
      match fake_digits with
      | [] -> ' '
      | h :: t -> if substring_in_string str h.name then h.value else which_digit str t in
    let rec aux2 line acc2 acc3 =
      match line with
      | [] -> 
        (match List.length acc2 with
        | 0 -> 0
        | 1 -> int_of_string (combineCharacters (List.hd acc2) (List.hd acc2))
        | _ -> int_of_string (combineCharacters (List.hd(List.rev acc2)) (List.hd acc2)))
      | h :: t -> if String.contains digits h then aux2 t (h :: acc2) ""
                  else if (which_digit (appendChar acc3 h) fake_digits) <> ' ' 
                    then aux2 t ((which_digit (appendChar acc3 h) fake_digits) ::acc2) (String.make 1 h)
                  else aux2 t acc2 (appendChar acc3 h) in      
    let rec aux parsed acc =
      match parsed with
      | [] -> List.fold_left (+) 0 acc
      | h :: t -> aux t ((aux2 (splitStringToList h) [] "") :: acc) in
    string_of_int (aux (parse_data data) []);;
end;;