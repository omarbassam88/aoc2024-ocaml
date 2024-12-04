type direction =
  | Up
  | Down
  | Left
  | Right
  | UpLeft
  | UpRight
  | DownLeft
  | DownRight

let next_pos (r, c) dir =
  match dir with
  | Up -> (r - 1, c)
  | Down -> (r + 1, c)
  | Left -> (r, c - 1)
  | Right -> (r, c + 1)
  | DownLeft -> (r + 1, c - 1)
  | DownRight -> (r + 1, c + 1)
  | UpLeft -> (r - 1, c - 1)
  | UpRight -> (r - 1, c + 1)

let char_at_pos grid (r, c) = (List.nth grid r).[c]

let corners grid center =
  List.fold_right
    (fun dir acc -> char_at_pos grid (next_pos center dir) :: acc)
    [ UpLeft; UpRight; DownRight; DownLeft ]
    []

let part_one input =
  let grid = Utils.lines input
  and next_char ch =
    let i = String.index "XMA" ch in
    "XMAS".[i + 1]
  in
  let height = List.length grid and width = String.length (List.hd grid) in
  let rec look_dir ch start dir =
    let r, c = next_pos start dir in
    if r >= 0 && c >= 0 && r < height && c < width then
      match char_at_pos grid (r, c) with
      | 'S' when ch = 'S' -> 1
      | m when m = ch -> look_dir (next_char ch) (r, c) dir
      | _ -> 0
    else 0
  and look_all_directions start =
    [ Up; Down; Left; Right; UpLeft; UpRight; DownLeft; DownRight ]
    |> List.fold_left (fun acc dir -> acc + look_dir 'M' start dir) 0
  and count = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      if char_at_pos grid (i, j) = 'X' then
        count := !count + look_all_directions (i, j)
    done
  done;
  !count

let part_two input =
  let grid = Utils.lines input and count = ref 0 in
  let height = List.length grid and width = String.length (List.hd grid) in
  for i = 1 to height - 2 do
    for j = 1 to width - 2 do
      if char_at_pos grid (i, j) = 'A' then
        match corners grid (i, j) with
        | [ 'M'; 'M'; 'S'; 'S' ]
        | [ 'S'; 'M'; 'M'; 'S' ]
        | [ 'S'; 'S'; 'M'; 'M' ]
        | [ 'M'; 'S'; 'S'; 'M' ] ->
            count := !count + 1
        | _ -> ()
    done
  done;
  !count

let test_input =
  {|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|}

let%test "Part 1: " = part_one test_input = 18
let%test "Part 2: " = part_two test_input = 9
