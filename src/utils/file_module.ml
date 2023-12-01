let read_file filename =
  let chan = open_in filename in
  let content = really_input_string chan (in_channel_length chan) in
  close_in chan;
  let l = String.length content in
  if content.[l - 1] = '\n' then String.sub content 0 (l - 1) else content

let write_to_file filename content =
  let chan = open_out filename in
  output_string chan (content ^ "\n");
  close_out chan



