let regexp = Re.Pcre.regexp

let parse_input input =
  let open List in
  let input_split = Re.split (regexp {|\n\n|}) input
  and parse_rule rule_str =
    Re.split (regexp {|\||}) rule_str |> map int_of_string |> fun rule ->
    (hd rule, nth rule 1)
  and parse_update update_str =
    Re.split (regexp ",") update_str |> map int_of_string
  in
  let rules = hd input_split |> Utils.lines |> map parse_rule
  and updates = nth input_split 1 |> Utils.lines |> map parse_update in
  (rules, updates)

let rule_exists rule = List.exists (( = ) rule)

let rec correct_update rules =
  let satisfy_rules num =
    List.for_all (fun item -> rule_exists (num, item) rules)
  in
  function
  | [ _ ] | [] -> true
  | x :: xs -> satisfy_rules x xs && correct_update rules xs

let middle_page update = List.(nth update (length update / 2))

let part_one input =
  let rules, updates = parse_input input in
  let open List in
  filter (correct_update rules) updates |> map middle_page |> fold_left ( + ) 0

let part_two input =
  let rules, updates = parse_input input in
  let open List in
  let sorted_update =
    sort (fun x y ->
        if rule_exists (x, y) rules then 0
        else if rule_exists (y, x) rules then 1
        else -1)
  in
  partition (correct_update rules) updates
  |> snd |> map sorted_update |> map middle_page |> fold_left ( + ) 0

let test_input =
  {xxx|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47|xxx}

let%test "Part 1: " = part_one test_input = 143
let%test "Part 2: " = part_two test_input = 123
