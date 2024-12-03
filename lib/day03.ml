let rec all_matches regexp text pos acc =
  try
    let _ = Str.search_forward regexp text pos in
    all_matches regexp text (Str.match_end ()) (Str.matched_string text :: acc)
  with Not_found -> acc

let parse_mul m =
  let nums = all_matches (Str.regexp {|[0-9]+|}) m 0 [] in
  nums |> List.map int_of_string |> List.fold_left ( * ) 1

let mul_regex = Str.regexp {|mul([0-9]+,[0-9]+)|}

let part_one input =
  let matches = all_matches mul_regex input 0 [] in
  List.map parse_mul matches |> List.fold_left ( + ) 0

let part_two input =
  let dos = Str.(split (regexp "don't()") input) in
  let tl_dos =
    List.tl dos |> List.map Str.(split (regexp "do()")) |> List.(map tl)
  in
  let all_dos = List.hd dos :: List.flatten tl_dos |> String.concat "" in
  part_one all_dos

let test_input =
  {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|}

let%test "Part 1: " = part_one test_input = 161

let test_input =
  {|xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|}

let%test "Part 2: " = part_two test_input = 48
