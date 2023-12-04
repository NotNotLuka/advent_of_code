open Solvers.Signature
open Utils.String_module

type card = {
  id : int;
  winning : int list;
  numbers : int list;
}

module Solver : Solver = struct
  let parse input =
    let lines = parseData input in
    let process_line line =
      let numbers_and_id = String.split_on_char ':' line in
      let id = int_of_string (List.nth (Str.split (Str.regexp " +") (List.hd numbers_and_id)) 1) in
      let string_to_numbers_list numbers = List.map int_of_string (Str.split (Str.regexp " +") numbers) in
      let numbers = split '|' (String.trim (List.nth numbers_and_id 1)) in
      {
        id = id;
        winning = string_to_numbers_list (String.trim (List.nth numbers 0));
        numbers = string_to_numbers_list (String.trim (List.nth numbers 1));
      }
    in
    List.map process_line lines
  
  let get_overlap_size a b =
    let rec aux a b acc =
      match a with
      | [] -> acc
      | h :: t ->
        if List.mem h b then
          aux t b (1 + acc)
        else
          aux t b acc
    in
    (aux a b 0)
  
  let int_power a b =
    let a' = float_of_int a in
    let b' = float_of_int b in
    int_of_float (a' ** b')

  let task1 data =
    let parsed = parse data in
    let rec get_sum_of_scores cards acc =
      match cards with
      | [] -> acc
      | card :: t -> get_sum_of_scores t ((int_power 2 ((get_overlap_size card.winning card.numbers) - 1)) + acc) in
    get_sum_of_scores parsed 0 |> string_of_int

  
  let update_n_cards won x_times current n_cards =
    let rec aux won x_times current n_cards =
      if 0 < won 
      then 
        begin
          n_cards.(current+1) <- n_cards.(current+1) + x_times;
          aux (won - 1) x_times (current + 1) n_cards
        end
      else n_cards
    in
    aux won x_times current n_cards
  
  let task2 data _part1 =
    let parsed = parse data in
    let n = (List.hd (List.rev parsed)).id in
    let number_of_cards = Array.make n 1 in
    let rec loop current n n_cards cards =
      if current < n
      then let won = get_overlap_size (List.nth cards current).winning (List.nth cards current).numbers in 
           loop (current + 1) n (update_n_cards won (n_cards.(current)) current n_cards) cards
      else n_cards in
    string_of_int (Array.fold_left (+) 0 (loop 0 n number_of_cards parsed))
end;;