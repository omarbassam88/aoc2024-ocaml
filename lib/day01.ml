let parse_input input =
  let split_line = Re.(split (Pcre.regexp {|\s+|})) in
  let left = ref [] and right = ref [] in
  let open List in
  let parse_line line =
    (* Split each line by one or more space *)
    let lr = split_line line |> map int_of_string in
    left := nth lr 0 :: !left;
    right := nth lr 1 :: !right
  in
  Utils.lines input |> iter parse_line;
  (!left, !right)

let part_one input =
  let left, right = parse_input input in
  let open List in
  let left = sort Int.compare left and right = sort Int.compare right in
  let rec calculate_distances left right distances =
    match (left, right) with
    | l :: ll, r :: rr -> calculate_distances ll rr (abs (l - r) :: distances)
    | _, _ -> distances
  in
  calculate_distances left right [] |> fold_left ( + ) 0

let part_two input =
  let left, right = parse_input input in
  let open List in
  let similarity_score num =
    num * fold_left (fun acc n -> if num = n then acc + 1 else acc) 0 right
  in
  map similarity_score left |> fold_left ( + ) 0

let test_input = {|3   4
4   3
2   5
1   3
3   9
3   3|}

let%test "Part 1: " = part_one test_input = 11
let%test "Part 2: " = part_two test_input = 31
