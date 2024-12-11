let create_disk disk_map =
  let disk = ref [||] and file_id = ref 0 in
  disk_map |> String.trim
  |> String.iteri (fun index size_ch ->
         let block = if index mod 2 = 0 then Some !file_id else None in
         let size = Char.(code size_ch - code '0') in
         disk := Array.make size block |> Array.append !disk;
         if index mod 2 = 0 then Core.incr file_id);

  !disk

let checksum disk =
  disk
  |> Array.mapi (fun index block ->
         match block with Some n -> n * index | None -> 0)
  |> Array.fold_left ( + ) 0

let swap_blocks disk left right =
  disk.(left) <- disk.(right);
  disk.(right) <- None

let compress_disk disk =
  let left = ref 0 and right = ref (Array.length disk - 1) in
  while !left < !right do
    match (disk.(!left), disk.(!right)) with
    | Some _, Some _ -> Core.incr left
    | _, None -> Core.decr right
    | None, Some _ ->
        swap_blocks disk !left !right;
        Core.incr left;
        Core.decr right
  done;
  disk

let get_files disk =
  disk
  |> Array.fold_left
       (fun files file_opt ->
         match file_opt with
         | Some n when List.mem n files -> files
         | Some n -> n :: files
         | None -> files)
       []

let rec move_files files disk =
  match files with
  | [] -> disk
  | file_id :: rest ->
      let last_file = ref (Array.length disk - 1) and file_size = ref 0 in
      let first_space = ref 0 and free_space = ref 0 in
      (* find the last index of the file *)
      while disk.(!last_file) <> Some file_id do
        Core.decr last_file
      done;
      (* find the file size *)
      while
        !last_file - !file_size > 0
        && disk.(!last_file - !file_size) = Some file_id
      do
        Core.incr file_size
      done;
      (* Find the frist space that fits the file  *)
      while disk.(!first_space) <> None do
        Core.incr first_space
      done;
      while
        !free_space < !file_size
        && !first_space + !free_space < Array.length disk
      do
        first_space := !first_space + !free_space;
        free_space := 0;
        while !first_space < Array.length disk && disk.(!first_space) <> None do
          Core.incr first_space
        done;
        while
          !first_space + !free_space < Array.length disk
          && disk.(!first_space + !free_space) = None
        do
          Core.incr free_space
        done
      done;
      if !last_file > !first_space && !file_size <= !free_space then
        while !file_size > 0 do
          swap_blocks disk !first_space !last_file;
          Core.decr file_size;
          Core.incr first_space;
          Core.decr last_file
        done;
      move_files rest disk

let part_one input = create_disk input |> compress_disk |> checksum

let part_two input =
  let disk = create_disk input in
  let files = get_files disk in
  move_files files disk |> checksum

(* Tests *)
let test_input = {|2333133121414131402|}
let%test "Part 1: " = part_one test_input = 1928
let%test "Part 2: " = part_two test_input = 2858
