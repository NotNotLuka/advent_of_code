let rec print_char_list lst =
  match lst with
  | [] -> print_newline ()
  | head :: tail ->
      Printf.printf "%c " head;
      print_char_list tail;;

let print_int_list lst =
  List.iter (fun x -> print_int x; print_string " ") lst;
  print_newline ()  ;;

let print_char_list_list lst =
  List.iter (fun inner_list ->
    List.iter (fun c -> print_char c; print_string ", ") inner_list;
    print_string "; "
  ) lst;
  print_newline () ;;

let print_string_list lst =
  List.iter (fun s-> print_string s; print_string "; ") lst;
  print_newline () ;;
  
let rec print_char_int_pairs pairs =
  match pairs with
  | [] -> ()
  | (chars, ints) :: rest ->
    begin
      print_string "[";
      List.iter (fun c -> print_char c; print_string " ") chars;
      print_string "] [";
      List.iter (fun i -> print_int i; print_string " ") ints;
      print_string "]\n";
      print_char_int_pairs rest
    end
;;

let print_bool_list bool_list =
  List.iter (fun b -> print_string (if b then "true " else "false ")) bool_list;
  print_newline();;

let print_tuple (x, y) =
  (* Function to print a single tuple (char list list, int list) *)
  print_string "First element (char list list): ";
  print_endline (List.fold_left (fun acc lst -> acc ^ " " ^ (String.concat "" (List.map (String.make 1) lst))) "" x);
  print_string "Second element (int list): ";
  print_endline (String.concat " " (List.map string_of_int y))

let rec print_list_of_tuples lst =
  (* Function to print a list of tuples *)
  match lst with
  | [] -> ()
  | hd :: tl ->
      print_tuple hd;
      print_list_of_tuples tl

let print_int_tuple_list lst =
  List.iter (fun (x, y) ->
    Printf.printf "(%d, %d), " x y
  ) lst;
  print_newline ()

let print_nested_lists (lists : ((int * int) list * (int * int) list) list) =
  List.iter (fun (list1, list2) ->
    print_string "[";
    List.iter (fun (a, b) -> Printf.printf "(%d, %d) " a b) list1;
    print_string "] [";
    List.iter (fun (a, b) -> Printf.printf "(%d, %d) " a b) list2;
    print_endline "]"
  ) lists;;

let print_int_array arr =
  Array.iter (fun elem -> Printf.printf "%d " elem) arr;
  print_newline ()