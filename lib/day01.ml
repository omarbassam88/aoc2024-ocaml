let parse_input input =
  let left = ref [] and right = ref [] in
  let parse_line line =
    (* Split each line by one or more space *)
    match Str.(split (regexp " +")) line with
    | [] | [ _ ] -> print_endline line
    | l :: r :: _ ->
        left := int_of_string l :: !left;
        right := int_of_string r :: !right
  in
  Utils.lines input |> List.iter parse_line;
  (!left, !right)

let part_one input =
  let left, right = parse_input input in
  let left = List.sort compare left and right = List.sort compare right in
  let rec calculate_distances left right distances =
    match (left, right) with
    | l :: ll, r :: rr -> calculate_distances ll rr (abs (l - r) :: distances)
    | _, _ -> distances
  in
  calculate_distances left right [] |> List.fold_left ( + ) 0

let part_two input =
  let left, right = parse_input input in
  let similarity_score num =
    num * List.fold_left (fun acc n -> if num = n then acc + 1 else acc) 0 right
  in
  List.map similarity_score left |> List.fold_left ( + ) 0

let test_input = {|3   4
4   3
2   5
1   3
3   9
3   3|}

let%test "Part 1: " = part_one test_input = 11
let%test "Part 2: " = part_two test_input = 31
