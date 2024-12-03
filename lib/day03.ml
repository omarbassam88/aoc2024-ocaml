let parse_mul m =
  let open List in
  Re.(matches (Pcre.regexp {|\d+|})) m |> map int_of_string |> fold_left ( * ) 1

let mul_regex = Re.Pcre.regexp {|mul\(\d+,\d+\)|}

let part_one input =
  Re.matches mul_regex input |> List.map parse_mul |> List.fold_left ( + ) 0

let part_two input =
  let open Re.Pcre in
  let dont_rgx = regexp {|don't\(\)|} and do_rgx = regexp {|do()|} in
  let dos = Re.split dont_rgx input in
  let open List in
  let all_dos =
    hd dos :: (tl dos |> map (Re.split do_rgx) |> map tl |> flatten)
  in
  map part_one all_dos |> fold_left ( + ) 0

let test_input =
  {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|}

let%test "Part 1: " = part_one test_input = 161

let test_input =
  {|xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|}

let%test "Part 2: " = part_two test_input = 48
