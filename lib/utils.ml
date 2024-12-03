let read_file file_name =
  let ic = open_in file_name in
  try
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    s
  with e ->
    close_in_noerr ic;
    raise e

(** Get a list of lines of a string *)
let lines input = String.split_on_char '\n' (String.trim input)
