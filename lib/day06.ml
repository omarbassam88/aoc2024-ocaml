type direction = Up | Down | Left | Right

module Position = struct
  type t = int * int

  let compare p1 p2 = compare p1 p2
end

module PositionSet = Set.Make (Position)

module PositionDirection = struct
  type t = Position.t * direction

  let compare (pos1, dir1) (pos2, dir2) = compare (pos1, dir1) (pos2, dir2)
end

module PositionDirectionSet = Set.Make (PositionDirection)

let turn_right = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let move_direction (r, c) = function
  | Up -> (r - 1, c)
  | Down -> (r + 1, c)
  | Left -> (r, c - 1)
  | Right -> (r, c + 1)

let char_at_pos grid (r, c) = grid.(r).[c]

let hit_bounds (r, c) grid =
  r = 0 || c = 0
  || r = Array.(length grid) - 1
  || c = String.length grid.(0) - 1

let start_pos grid =
  let pos = ref (0, 0) in
  Array.iteri
    (fun x r -> String.index_opt r '^' |> Option.iter (fun y -> pos := (x, y)))
    grid;
  !pos

let rec start_walking grid start dir visited =
  let should_turn pos dir = char_at_pos grid (move_direction pos dir) = '#' in
  if hit_bounds start grid then PositionSet.add start visited
  else
    let next_dir = if should_turn start dir then turn_right dir else dir
    and visited = PositionSet.add start visited in
    start_walking grid (move_direction start next_dir) next_dir visited

let rec will_loop grid start dir visited obstacle =
  let should_turn pos dir =
    char_at_pos grid (move_direction pos dir) = '#'
    || obstacle = move_direction pos dir
  in
  if hit_bounds start grid || start = obstacle then false
  else if PositionDirectionSet.mem (start, dir) visited then true
  else
    let next_dir = ref dir in
    while should_turn start !next_dir do
      next_dir := turn_right !next_dir
    done;
    will_loop grid
      (move_direction start !next_dir)
      !next_dir
      (PositionDirectionSet.add (start, dir) visited)
      obstacle

let part_one input =
  let grid = Utils.lines input |> Array.of_list in
  start_walking grid (start_pos grid) Up PositionSet.empty
  |> PositionSet.cardinal

let part_two input =
  let grid = Utils.lines input |> Array.of_list in
  start_walking grid (start_pos grid) Up PositionSet.empty
  |> PositionSet.filter
       (will_loop grid (start_pos grid) Up PositionDirectionSet.empty)
  |> PositionSet.cardinal

let test_input =
  {|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|}

let%test "Part 1: " = part_one test_input = 41
let%test "Part 2: " = part_two test_input = 6
