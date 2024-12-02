module type Day = sig
  val part_one : string -> int
  val part_two : string -> int
end

let modules =
  [ ("01", (module Aoc.Day01 : Day)); ("02", (module Aoc.Day02 : Day)) ]

let () =
  let d = Array.get Sys.argv 1 in
  let input = Aoc.Utils.read_file (Fmt.str "input/day%s.txt" d)
  and (module M) = List.assoc d modules in
  Fmt.pr "Day %s\n" d;
  Fmt.pr "Part 1: %d\n" (M.part_one input);
  Fmt.pr "Part 2: %d\n" (M.part_two input)
