let parse_entry line =
  let line_split = String.split_on_char ':' line in
  let target = List.hd line_split |> int_of_string
  and nums = List.nth line_split 1 |> String.trim |> String.split_on_char ' ' in
  (target, nums)

let parse_input input = Utils.lines input |> List.map parse_entry

let rec permutations values n =
  if n = 0 then [ [] ]
  else
    let open List in
    permutations values (n - 1)
    |> map (fun comb -> map (fun value -> value :: comb) values)
    |> concat

let rec eval expr =
  match expr with
  | [ x ] -> int_of_string x
  | x :: "||" :: y :: rest -> eval ((x ^ y) :: rest)
  | x :: "+" :: y :: rest ->
      eval (string_of_int (int_of_string x + int_of_string y) :: rest)
  | x :: "*" :: y :: rest ->
      eval (string_of_int (int_of_string x * int_of_string y) :: rest)
  | _ -> failwith "Invalid combination"

let create_combination nums perm =
  let open List in
  fold_left2 (fun acc a b -> append acc [ a; b ]) [ hd nums ] perm (tl nums)

let count_valid (target, nums) operators =
  let open List in
  permutations operators (length nums - 1)
  |> fold_left (fun acc perm -> create_combination nums perm :: acc) []
  |> map eval
  |> filter (( = ) target)
  |> length

let valid_entry operators entry = count_valid entry operators > 0

let part_one input =
  let open List in
  parse_input input
  |> filter (valid_entry [ "+"; "*" ])
  |> List.map fst |> List.fold_left ( + ) 0

let part_two input =
  let open List in
  parse_input input
  |> filter (valid_entry [ "+"; "*"; "||" ])
  |> List.map fst |> List.fold_left ( + ) 0

let test_input =
  {|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20|}

let%test "Part 1: " = part_one test_input = 3749
let%test "Part 2: " = part_two test_input = 11387
