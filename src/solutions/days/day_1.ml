open Solvers.Signature
open Utils.String_module

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
  let task1 data =
    let rec check_characters line numbers =
      match line with
      | [] -> 
        (match List.length numbers with
        | 0 -> 0
        | 1 -> int_of_string ((List.hd numbers) $^$ (List.hd numbers))
        | _ -> int_of_string ((List.hd(List.rev numbers)) $^$ (List.hd numbers)))
      | h :: t -> if String.contains digits h then check_characters t (h :: numbers) 
                  else check_characters t numbers in 
      
    let rec check_lines parsed numbers =
      match parsed with
      | [] -> List.fold_left (+) 0 numbers
      | h :: t -> check_lines t ((check_characters (stringToList h) []) :: numbers) in

    string_of_int (check_lines (parseData data) []);;

  let task2 data _part1 =
    let rec which_digit str fake_digits =
      match fake_digits with
      | [] -> ' '
      | h :: t -> if substringInString str h.name then h.value 
                  else which_digit str t in

    let rec check_characters line numbers words =
      match line with
      | [] -> 
        (match List.length numbers with
        | 0 -> 0
        | 1 -> int_of_string ((List.hd numbers) $^$ (List.hd numbers))
        | _ -> int_of_string ((List.hd(List.rev numbers)) $^$ (List.hd numbers)))
      | h :: t -> if String.contains digits h 
                  then check_characters t (h :: numbers) ""
                  else 
                    let digit = which_digit (words ^$ h) fake_digits in
                    if digit <> ' ' 
                    then check_characters t (digit :: numbers) (String.make 1 h)
                  else check_characters t numbers (words ^$ h) in      

    let rec check_lines parsed numbers =
      match parsed with
      | [] -> List.fold_left (+) 0 numbers
      | h :: t -> check_lines t ((check_characters (stringToList h) [] "") :: numbers) in

    string_of_int (check_lines (parseData data) []);;
end;;