module type Day = sig
  val part_one : string -> int
  val part_two : string -> int
end

let modules =
  let open Aoc in
  [
    ("01", (module Day01 : Day));
    ("02", (module Day02 : Day));
    ("03", (module Day03 : Day));
    ("04", (module Day04 : Day));
    ("05", (module Day05 : Day));
    ("06", (module Day06 : Day));
  ]

let () =
  let d = Array.get Sys.argv 1 in
  let input = Aoc.Utils.read_file (Fmt.str "input/day%s.txt" d)
  and (module M) = List.assoc d modules in
  Fmt.pr "Day %s\n" d;
  Fmt.pr "Part 1: %d\n" (M.part_one input);
  Fmt.pr "Part 2: %d\n" (M.part_two input)
