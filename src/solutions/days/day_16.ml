open Solvers.Signature
open Utils.String_module

type vector = {x: int; y: int}
let (<+>) a b = {x=a.x + b.x;y=a.y+b.y}
type beam = {pos: vector; v: vector}


module Solver : Solver = struct
  let parse input = Array.of_list (List.map (fun x -> Array.of_list (stringToList x)) (String.split_on_char '\n' input))

  let simulate_all_beams beams been map =
    let inside vec =
      if vec.x < 0 || vec.y < 0 then false
      else if Array.length map <= vec.y || Array.length map.(0) <= vec.x then false
      else true in
    let validate been b =
      if inside b.pos 
        then 
          match b.v.x, b.v.y with
          | 1, 0 -> not (been.(b.pos.y).(b.pos.x).(0))
          | 0, -1 -> not been.(b.pos.y).(b.pos.x).(1)
          | -1, 0 -> not been.(b.pos.y).(b.pos.x).(2)
          | 0, 1 -> not been.(b.pos.y).(b.pos.x).(3)
          | _ -> failwith "Unknown vector"
        else false in 
    
    let update_array been h =
      match h.v.x, h.v.y with
      | 1, 0 -> been.(h.pos.y).(h.pos.x).(0) <- true
      | 0, -1 -> been.(h.pos.y).(h.pos.x).(1) <- true
      | -1, 0 -> been.(h.pos.y).(h.pos.x).(2) <- true
      | 0, 1 -> been.(h.pos.y).(h.pos.x).(3) <- true
      | _ -> failwith "Unknown vector" in

    let rec aux beams been =
      match beams with
      | [] -> ()
      | h :: t -> 
        let new_beams = 
            match map.(h.pos.y).(h.pos.x) with
            | '.' -> [{h with pos=h.pos <+> h.v}] 
            | '/' -> let new_v = 
                    if h.v.y = 0 
                    then {x=0;y=(-h.v.x)} else {x=(-h.v.y);y=0} in 
                    [{pos=h.pos <+> new_v; v=new_v}] 
            | '\\' -> let new_v = 
                    if h.v.y = 0 
                    then {x=0;y=h.v.x} else {x=h.v.y;y=0} in 
                    [{pos=h.pos <+> new_v; v=new_v}]
            | '-' -> if h.v.y <> 0 then 
                    let v1 = {x=(-1);y=0} in let v2 = {x=1;y=0} in 
                    [{pos=h.pos <+> v1; v=v1};{pos=h.pos <+> v2; v=v2}]
                    else  [{h with pos=h.pos <+> h.v}] 
            | '|' -> if h.v.x <> 0 
              then  let v1 = {x=0;y=1} in let v2 = {x=0;y=(-1)} in 
              [{pos=h.pos <+> v1; v=v1};{pos=h.pos <+> v2; v=v2}]
            else  [{h with pos=h.pos <+> h.v}] 
            | _ -> failwith "Unknown sign"
        in 
        let new_beams = List.filter (validate been) new_beams in
        update_array been h;
        aux (new_beams @ t) been in 
    aux beams been
  
    let count_trues arr =
      let count = ref 0 in
      let depth = Array.length arr in
      for i = 0 to depth - 1 do
        let rows = Array.length arr.(i) in
        for j = 0 to rows - 1 do
          let cols = Array.length arr.(i).(j) in
          let already = ref false in
          for k = 0 to cols - 1 do
            if arr.(i).(j).(k) && (not !already) then
              begin
              count := !count + 1;
              already := true;
              end
      done; done; done;
      !count
  
  let create_3d_boolean_array map =
    let arr = Array.make_matrix (Array.length map) (Array.length map.(0)) [||] in
    for z = 0 to (Array.length map) - 1 do
      arr.(z) <- Array.make_matrix (Array.length map.(0)) 4 false
    done;
    arr
  
  let run_sim map beam =
    let been = create_3d_boolean_array map in 
    simulate_all_beams [beam] been map;
    count_trues been

  let task1 data =
    let map = parse data in 
    let active = run_sim map {pos={x=0;y=0};v={x=1;y=0}} in
    string_of_int active

  let run_through_all map =
    let rec aux (x, y) (x_max, y_max) vs acc =
      if y < y_max && x < x_max then
        if x = 0 || y = 0 || x = x_max - 1 || y = y_max - 1 then
          let ans = List.fold_left (fun acc v -> let n = run_sim map {pos={x=x;y=y};v=v} in if acc < n then n else acc) acc vs in 
          aux (x, y+1) (x_max, y_max) vs (ans)
        else aux (x, y+1) (x_max, y_max) vs acc
      else if x < x_max then
            aux (x+1, 0) (x_max, y_max) vs acc
      else acc in
    aux (0, 0) (Array.length map.(0), Array.length map) [{x=1;y=0};{x=(-1);y=0};{x=0;y=1};{x=0;y=(-1)}] 0

  let task2 data _part1 =
    let map = parse data in 
    let ans = run_through_all map in
    string_of_int (ans)
end;;