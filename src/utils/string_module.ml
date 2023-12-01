let parseData data = String.split_on_char '\n' data;;
let stringToList str = str |> String.to_seq |> List.of_seq;;
let substringInString s1 s2 =
  (* source: https://stackoverflow.com/questions/8373460/substring-check-in-ocaml *)
  let re = Str.regexp_string s2
  in
      try ignore (Str.search_forward re s1 0); true
      with Not_found -> false;;
let ($^$) a b = String.make 1 a ^ String.make 1 b;;
let (^$) a b = a ^ (String.make 1 b);;
let ($^) a b = (String.make 1 a) ^ b;;