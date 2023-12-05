open Solvers.Signature

let remove_first = function
  | _ :: t -> t
  | [] -> []

type rule = {
  from: string;
  to': string;
  rules: int list list;
}
type seed_interval = {
  start: int;
  end': int;
}

type intervals = 
{
  inside: seed_interval;
  outside: seed_interval list;
}
let rec flatten lst = 
  match lst with
  | [] -> []
  | h :: t -> h @ (flatten t)

module Solver : Solver = struct
  let parse input =
    let lines = Str.split (Str.regexp "\n\n") input in
    let seeds  = List.map int_of_string (Str.split (Str.regexp_string " ") (List.nth (Str.split (Str.regexp_string ": ")  (List.hd lines)) 1)) in
    let parse_rules to_data =
      let lines = Str.split (Str.regexp "\n") to_data in
      let from_to = Str.split (Str.regexp " \\|-to-") (List.hd lines) in
      let rules_string = remove_first lines in
      let rules_list = List.map (fun x -> List.map int_of_string (Str.split (Str.regexp_string " ") x)) rules_string in
      
      {from = List.hd from_to; to' = List.nth from_to 1; rules = rules_list} in
    seeds, List.map parse_rules (remove_first lines)
  
  let find_seed_location rules seed  =
    let rec aux rules seed =
      match rules with
      | [] -> seed
      | rule :: t -> 
                let seed_start = List.nth rule 1 in
                if seed_start <= seed && seed < seed_start + (List.nth rule 2)
                then 
                  ((List.nth rule 0) + (seed - seed_start))
                else aux t seed
    in
    aux rules.rules seed 
  
  let go_through_fun functions seed =
    let rec aux functions seed =
      match functions with
      | [] -> seed
      | f :: t -> aux t (f seed)
    in
    aux functions seed
  
  let task1 data =
    let x = parse data in 
    let seeds = fst x in
    let rules = snd x in
    let rule_functions = List.map find_seed_location rules  in
    let seed_locations = List.map (go_through_fun rule_functions) seeds in
    string_of_int (List.fold_left (fun acc el -> if el < acc then el else acc) (List.hd seed_locations) seed_locations)

    let split_intervals a b =
      if a.start < b.start && b.end' <= a.end' (* first interval inside the second one*)
      then let below = {start=a.start;end'=b.start} in 
          let above = {start=b.end';end'=a.end'} in 
          let inside = {start=b.start; end'=a.end'} in
         {inside=inside; outside=[below;above]}
      else 
        if a.start < b.start && a.end' <= b.end' (*below first interval*)
        then let below = {start=a.start;end'=b.start} in 
            let inside = {start=b.start;end'=a.end'} in 
            {inside=inside; outside=[below;]}
        else
          if b.start <= a.start && b.end' < a.end' (*above first interval*)
          then let inside = {start=a.start;end'=b.end'} in 
            let above = {start=b.end';end'=a.end'} in 
            {inside=inside; outside=[above;]}
          else {inside=a; outside=[]} (*inside*)

    
  let find_range_seed_location rules range_seed =
    let rec aux rules acc intact_rules seed =
      match rules with
      | [] -> seed :: acc
      | rule :: t -> 
                let interval = {start=(List.nth rule 1);end'=((List.nth rule 1) + (List.nth rule 2))} in
                if seed.end' <= interval.start || interval.end' <= seed.start (*outside the range*)
                then 
                  aux t acc intact_rules seed
                else
                  let status = (split_intervals seed interval) in 
                  [{start=((List.nth rule 0) + (status.inside.start - interval.start)); 
                    end'=((List.nth rule 0) + (status.inside.end' - interval.start))}] @
                  (flatten (List.map (aux intact_rules [] intact_rules) status.outside))
    in
    aux rules.rules [] rules.rules range_seed
  let go_through_fun_and_seeds functions seeds =
    let rec aux functions seeds =
      match functions with
      | [] -> seeds
      | f :: t -> let new_seeds = (flatten (List.map f seeds)) in
                  aux t new_seeds
    in
    aux functions seeds
  
  let generate_seeds seeds = 
    let rec aux seeds acc =
      match seeds with
      | [] -> acc
      | h :: t -> aux (remove_first t) ({start=h; end'=(List.hd t) + h} ::  acc)
    in
    aux seeds []
  let task2 data _part1 =
    let x = parse data in 
    let seeds = generate_seeds (fst x) in
    let rules = snd x in
    let rule_functions = List.map find_range_seed_location rules  in
    let seed_locations = go_through_fun_and_seeds rule_functions seeds in
    string_of_int (List.fold_left (fun acc el -> if el.start < acc then el.start else acc) (List.hd seed_locations).start seed_locations)
end;;