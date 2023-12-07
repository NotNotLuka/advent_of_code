open Solvers.Signature
open Stdlib
open Utils.String_module

module CharMap = Map.Make(Char)

type hand = {
  cards: string;
  bet: int;
  score: int;
}

module Solver : Solver = struct
  let increment_value key array priority =
    if String.contains "23456789" key 
    then (array.((int_of_char key) - 49) <- array.((int_of_char key) - 49) + 1)
    else if String.contains priority key then (
      let ind = 9 + (String.index priority key) in
      array.(ind) <- array.(ind) + 1);
    array
  let compare_cards a b order =
    let rec aux a b  x =
      let a_value = String.index order (List.nth a x) in
      let b_value = String.index order (List.nth b x) in
      if a_value = b_value then aux a b (x + 1)
      else if b_value < a_value then true
      else false in
    aux (stringToList a) (stringToList b) 0
  let find_max_index array =
    let max_index = ref 0 in
    let max_value = ref array.(0) in
    for i = 0 to Array.length array - 2 do
      if array.(i) > !max_value then (
        max_index := i;
        max_value := array.(i))
    done;
    !max_index
  let calculate_score cards joker priority =
    let evalute_hand status card_count joker =
      if joker then card_count.(find_max_index card_count) <- card_count.(find_max_index card_count) + card_count.(13);
      let x = if joker then 2 else 1 in
      for i = 0 to Array.length card_count - x do status.(card_count.(i)) <- status.(card_count.(i)) + 1;
      done;
      if status.(5) = 1 then 6
      else if status.(4) = 1 then 5
      else if status.(3) = 1 && status.(2) = 1 then 4
      else if status.(3) = 1 then 3
      else if status.(2) = 2 then 2
      else if status.(2) = 1 then 1
      else 0 in
    let rec count_cards cards card_count priority =
      match cards with
      | [] -> card_count
      | h :: t -> count_cards t (increment_value h card_count priority) priority in
    evalute_hand (Array.make 6 0) (count_cards cards (Array.make 14 0) priority) joker
  let parse data =
    let to_hand line = 
      let splitted = (Str.split (Str.regexp " ") line) in
      {cards = (List.hd splitted); 
      bet = int_of_string (List.nth splitted 1); 
      score = (calculate_score (stringToList (List.hd splitted)) false "TJQKA")} in
    List.map to_hand (String.split_on_char '\n' data) 
  let compare_hands order a b =
    if a.score < b.score then -1
    else if b.score < a.score  then 1
    else if compare_cards a.cards b.cards order then 1
    else -1
  let calculate_final hands =
    let rec aux hands rank acc =
      match hands with
      | [] -> acc
      | h :: t -> aux t (rank+1) (acc + rank * h.bet) in
    aux hands 1 0
  let task1 data =
    let all_hands = parse data in
    let sorted_hands = List.sort (compare_hands "23456789TJQKA") all_hands in
    string_of_int (calculate_final sorted_hands)
  let parse data =
    let to_hand line = 
      let splitted = (Str.split (Str.regexp " ") line) in
      {cards = (List.hd splitted); 
      bet = int_of_string (List.nth splitted 1); 
      score = (calculate_score (stringToList (List.hd splitted)) true "TQKAJ")} in
    List.map to_hand (String.split_on_char '\n' data) 
  let task2 data _part1 =
    let all_hands = parse data in
    let sorted_hands = List.sort (compare_hands "J23456789TQKA") all_hands in
    string_of_int (calculate_final sorted_hands)
end;;