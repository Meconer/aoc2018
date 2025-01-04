open Core

let isExample = false

let filename =
  if isExample then "lib/day20/example.txt" else "lib/day20/input.txt"

let grid_max_width = 1000000
let aoc_input = In_channel.read_all filename
let aoc_input = String.drop_suffix (String.drop_prefix aoc_input 1) 1

type posT = { r : int; c : int }

let idx_of_pos pos = (pos.r * grid_max_width) + pos.c
let pos_of_idx idx = { r = idx / grid_max_width; c = idx mod grid_max_width }
let maze = ref (Map.empty (module Int))

let move pos dir =
  match dir with
  | 'N' -> { r = pos.r - 1; c = pos.c }
  | 'S' -> { r = pos.r + 1; c = pos.c }
  | 'E' -> { r = pos.r; c = pos.c + 1 }
  | 'W' -> { r = pos.r; c = pos.c - 1 }
  | _ -> failwith "Invalid direction"

let door_type_of dir =
  match dir with
  | 'N' | 'S' -> '-'
  | 'E' | 'W' -> '|'
  | _ -> failwith "Invalid direction"

let print_maze maze start_pos =
  let min_r = ref grid_max_width in
  let max_r = ref 0 in
  let min_c = ref grid_max_width in
  let max_c = ref 0 in
  Map.iteri !maze ~f:(fun ~key ~data:_ ->
      let pos = pos_of_idx key in
      min_r := Int.min !min_r pos.r;
      max_r := Int.max !max_r pos.r;
      min_c := Int.min !min_c pos.c;
      max_c := Int.max !max_c pos.c);
  Printf.printf "min_r=%d max_r=%d min_c=%d max_c=%d\n" !min_r !max_r !min_c
    !max_c;
  for r = !min_r - 1 to !max_r + 1 do
    for c = !min_c - 1 to !max_c + 1 do
      let idx = idx_of_pos { r; c } in
      let ch = match Map.find !maze idx with Some ch -> ch | None -> '#' in
      if start_pos.r = r && start_pos.c = c then Printf.printf "X"
      else Printf.printf "%c" ch
    done;
    Printf.printf "\n"
  done

let mark_maze maze pos dir =
  let door_pos = move pos dir in
  maze := Map.set !maze ~key:(idx_of_pos door_pos) ~data:(door_type_of dir);
  let room_pos = move door_pos dir in
  maze := Map.set !maze ~key:(idx_of_pos room_pos) ~data:'.';
  room_pos

let is_dir c = match c with 'N' | 'S' | 'E' | 'W' -> true | _ -> false

let parse_input aoc_input start_pos =
  let chars = String.to_list aoc_input in
  let pos = start_pos in
  maze := Map.set !maze ~key:(idx_of_pos pos) ~data:'X';

  let rec parse_input' pos chars stack =
    match chars with
    | [] -> ()
    | dir :: rest when is_dir dir ->
        (* N,S,E or W. Move and mark in the maze *)
        let new_pos = mark_maze maze pos dir in
        parse_input' new_pos rest stack
    | '(' :: rest ->
        (* Start of sub group. Push the current pos on stack *)
        parse_input' pos rest (pos :: stack)
    | ')' :: rest ->
        (* End of sub group. Pop the previous position from the stack *)
        let pos_from_stack =
          match stack with
          | [] -> failwith "Unmatched closing parenthesis"
          | hd :: rest_of_stack -> (hd, rest_of_stack)
        in
        parse_input' (fst pos_from_stack) rest (snd pos_from_stack)
    | '|' :: rest ->
        let new_pos =
          match stack with
          | [] -> failwith "Unmatched pipe character"
          | hd :: _ -> hd
        in
        parse_input' new_pos rest stack
    | _ -> failwith "Invalid input"
  in

  parse_input' pos chars []

let start_point = { r = 1000; c = 1000 }
(* Just a start point a bit inside the maze so we dont get negative coordinates *)

let solve_part1 =
  parse_input aoc_input start_point;
  print_maze maze start_point;

  let rec loop pos visited dir =
    let neighbours =
      [ 'N'; 'S'; 'E'; 'W' ] |> List.map ~f:(fun dir -> (move pos dir, dir))
    in
    let max_doors =
      List.fold neighbours ~init:0 ~f:(fun acc door_pos_and_dir ->
          let idx = idx_of_pos (fst door_pos_and_dir) in
          if Map.mem !maze idx && not (Set.mem visited idx) then
            (* If this position is in the map, it must be a door so we walk through it *)
            let new_visited = Set.add visited idx in
            (* The door is at one pos and we move to the other side of the door, so one more step in same direction *)
            let final_pos =
              move (fst door_pos_and_dir) (snd door_pos_and_dir)
            in
            let new_visited = Set.add new_visited (idx_of_pos final_pos) in
            let new_doors = 1 + loop final_pos new_visited (dir + 1) in
            Int.max acc new_doors
          else acc)
    in
    max_doors
  in

  let res =
    loop start_point (Set.singleton (module Int) (idx_of_pos start_point)) 0
  in
  res

let solve_part2 =
  let dist_map = ref (Map.empty (module Int)) in
  dist_map := Map.set !dist_map ~key:(idx_of_pos start_point) ~data:0;
  let rec loop pos visited dist =
    let neighbours =
      [ 'N'; 'S'; 'E'; 'W' ] |> List.map ~f:(fun dir -> (move pos dir, dir))
    in
    List.iter neighbours ~f:(fun door_pos_and_dir ->
        let idx = idx_of_pos (fst door_pos_and_dir) in
        if Map.mem !maze idx && not (Set.mem visited idx) then (
          (* If this position is in the map, it must be a door so we walk through it *)
          let new_visited = Set.add visited idx in
          (* The door is at one pos and we move to the other side of the door, so one more step in same direction *)
          let final_pos = move (fst door_pos_and_dir) (snd door_pos_and_dir) in
          let new_visited = Set.add new_visited (idx_of_pos final_pos) in
          (* Update the dist *)
          dist_map :=
            Map.set !dist_map ~key:(idx_of_pos final_pos) ~data:(dist + 1);
          loop final_pos new_visited (dist + 1))
        else ())
  in

  loop start_point (Set.singleton (module Int) (idx_of_pos start_point)) 0;
  Map.count !dist_map ~f:(fun dist -> dist >= 1000)
