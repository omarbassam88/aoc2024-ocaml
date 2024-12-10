module Position = struct
  type t = int * int

  let compare = compare
end

module PositionSet = Set.Make (Position)

module Antenna = struct
  type t = int * int * char

  let compare = compare
end

module AntennaSet = Set.Make (Antenna)

let is_alpha_numeric = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
  | _ -> false

let find_antennas grid =
  grid
  |> List.mapi (fun x r ->
         List.of_seq (String.to_seq r)
         |> List.mapi (fun y c -> (x, y, c))
         |> List.filter (fun (_, _, c) -> is_alpha_numeric c))
  |> List.flatten |> AntennaSet.of_list

let within_bounds grid (r, c) =
  r >= 0 && c >= 0 && r < List.length grid && c < String.length (List.hd grid)

let deltas (r1, c1) (r2, c2) = (r2 - r1, c2 - c1)

let get_antinodes grid (r1, c1, a1) (r2, c2, a2) =
  if a1 <> a2 then PositionSet.empty
  else
    let dx, dy = deltas (r1, c1) (r2, c2) in
    PositionSet.of_list [ (r1 - dx, c1 - dy); (r2 + dx, c2 + dy) ]
    |> PositionSet.filter (within_bounds grid)

let get_antinodes_ext grid (r1, c1, a1) (r2, c2, a2) =
  if a1 <> a2 then PositionSet.empty
  else
    let dx, dy = deltas (r1, c1) (r2, c2) in
    let left = ref (r1 - dx, c1 - dy) and right = ref (r2 + dx, c2 + dy) in
    let nodes =
      ref (PositionSet.of_list [ (r1, c1); (r2, c2); !left; !right ])
    in
    while within_bounds grid !left || within_bounds grid !right do
      let rl, cl = !left and rr, cr = !right in
      left := (rl - dx, cl - dy);
      right := (rr + dx, cr + dy);
      nodes := PositionSet.of_list [ !left; !right ] |> PositionSet.union !nodes
    done;
    !nodes |> PositionSet.filter (within_bounds grid)

let find_antinodes grid get_antinodes =
  let antennas = find_antennas grid in
  AntennaSet.to_list antennas
  |> List.map (fun ant1 ->
         AntennaSet.fold
           (fun ant2 antinodes ->
             PositionSet.union antinodes (get_antinodes grid ant1 ant2))
           (AntennaSet.remove ant1 antennas)
           PositionSet.empty)
  |> List.fold_left PositionSet.union PositionSet.empty

let part_one input =
  let grid = Utils.lines input in
  find_antinodes grid get_antinodes |> PositionSet.cardinal

let part_two input =
  let grid = Utils.lines input in
  find_antinodes grid get_antinodes_ext |> PositionSet.cardinal

let test_input =
  {|............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............|}

let%test "Part 1: " = part_one test_input = 14
let%test "Part 2: " = part_two test_input = 34
