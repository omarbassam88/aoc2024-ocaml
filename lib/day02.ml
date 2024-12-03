let parse_line line =
  Re.(matches (Pcre.regexp {|\d+|})) line |> List.map int_of_string

let parse_input input = Utils.lines input |> List.map parse_line

let rec calculate_distances = function
  | [ _ ] | [] -> []
  | x :: y :: xs -> (y - x) :: calculate_distances (y :: xs)

let within_range num = abs num >= 1 && abs num <= 3

let all_increasing report =
  report |> calculate_distances
  |> List.for_all (fun d -> d > 0 && within_range d)

let all_decreasing report =
  report |> calculate_distances
  |> List.for_all (fun d -> d < 0 && within_range d)

let remove_from_list lst index = List.filteri (fun i _ -> i <> index) lst
let is_safe report = all_increasing report || all_decreasing report
let part_one input = parse_input input |> List.filter is_safe |> List.length

let part_two input =
  let is_safe_dampened report =
    is_safe report
    || List.filteri
         (fun index _ -> is_safe (remove_from_list report index))
         report
       |> List.length |> ( <= ) 1
  in
  parse_input input |> List.filter is_safe_dampened |> List.length

let test_input = {|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|}

let%test "Part 1: " = part_one test_input = 2
let%test "Part 2: " = part_two test_input = 4
