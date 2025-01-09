open Core

let isExample = false
let filename = "lib/day22/input.txt"

type regionT = { x : int; y : int }

let hash_of_region region = (region.x * 1000) + region.y
let aoc_input = In_channel.read_lines filename

let depth =
  if isExample then 510
  else Scanf.sscanf (List.hd_exn aoc_input) "depth: %d" (fun x -> x)

let target =
  if isExample then { x = 10; y = 10 }
  else
    Scanf.sscanf (List.nth_exn aoc_input 1) "target: %d,%d" (fun x y ->
        { x; y })

let region_eq a b = a.x = b.x && a.y = b.y
let erosion_levels = ref (Map.empty (module Int))

let rec geologic_index region =
  if region_eq region { x = 0; y = 0 } || region_eq region target then 0
  else if region.y = 0 then region.x * 16807
  else if region.x = 0 then region.y * 48271
  else
    erosion_level { x = region.x - 1; y = region.y }
    * erosion_level { x = region.x; y = region.y - 1 }

and erosion_level region =
  if Map.mem !erosion_levels (hash_of_region region) then
    Map.find_exn !erosion_levels (hash_of_region region)
  else
    let res = (geologic_index region + depth) mod 20183 in
    erosion_levels :=
      Map.set !erosion_levels ~key:(hash_of_region region) ~data:res;
    res

type region_type_T = Rocky | Wet | Narrow

let region_type region =
  match erosion_level region mod 3 with
  | 0 -> Rocky
  | 1 -> Wet
  | 2 -> Narrow
  | _ -> failwith "Invalid region type"

let calc_region_types () =
  let region_types =
    Array.make_matrix ~dimx:(target.x + 4) ~dimy:(target.y + 4) Rocky
  in
  for y = 0 to target.y + 3 do
    for x = 0 to target.x + 3 do
      region_types.(x).(y) <- region_type { x; y }
    done
  done;
  region_types

let print_region_types region_types =
  for y = 0 to target.y + 3 do
    for x = 0 to target.x + 3 do
      let c =
        match region_types.(x).(y) with
        | Rocky -> '.'
        | Wet -> '='
        | Narrow -> '|'
      in
      Printf.printf "%c" c
    done;
    Printf.printf "\n"
  done

let region_types = calc_region_types ()

let risk_level region_types =
  let risk = ref 0 in
  for y = 0 to target.y do
    for x = 0 to target.x do
      risk :=
        !risk
        + match region_types.(x).(y) with Rocky -> 0 | Wet -> 1 | Narrow -> 2
    done
  done;
  !risk

let resultP1 = risk_level region_types

type toolT = Neither | Torch | Climbing
type stateT = { x : int; y : int; tool : toolT }

let hash_of_state state =
  (state.x * 1000) + state.y
  + (1000000 * match state.tool with Neither -> 0 | Torch -> 1 | Climbing -> 2)

let state_of_hash hash =
  let tool =
    match hash / 1000000 mod 1000000 with
    | 0 -> Neither
    | 1 -> Torch
    | 2 -> Climbing
    | _ -> failwith "Invalid tool"
  in
  let x = hash / 1000 mod 1000 in
  let y = hash mod 1000 in
  { x; y; tool }

let valid_tools region_type =
  match region_type with
  | Rocky -> [ Torch; Climbing ]
  | Wet -> [ Neither; Climbing ]
  | Narrow -> [ Neither; Torch ]

let is_equal_tool a b =
  match (a, b) with
  | Neither, Neither -> true
  | Torch, Torch -> true
  | Climbing, Climbing -> true
  | _ -> false

let is_equal_state a b = a.x = b.x && a.y = b.y && is_equal_tool a.tool b.tool

let is_valid_tool state =
  let reg_type = region_type { x = state.x; y = state.y } in
  List.mem (valid_tools reg_type) state.tool ~equal:is_equal_tool

let start_state = { x = 0; y = 0; tool = Torch }

let waitForEnter s =
  Printf.printf "%s" s;
  Out_channel.flush stdout;
  let _ = In_channel.(input_line stdin) in
  ()

let string_of_tool tool =
  match tool with
  | Neither -> "Neither"
  | Torch -> "Torch"
  | Climbing -> "Climbing"

let string_of_state state =
  Printf.sprintf "(%d, %d, %s)" state.x state.y (string_of_tool state.tool)

let print_map cost_map =
  Map.iteri cost_map ~f:(fun ~key ~data ->
      let state = state_of_hash key in
      Printf.printf "(%d, %d, %s) -> %d\n" state.x state.y
        (string_of_tool state.tool)
        data)

let print_queue queue =
  Printf.printf "Queue: ";
  List.iter queue ~f:(fun state ->
      Printf.printf "(%d, %d, %s)\n" state.x state.y (string_of_tool state.tool))

let find_target start_state =
  let queue = ref [ start_state ] in
  let cost_map = ref (Map.empty (module Int)) in
  cost_map := Map.set !cost_map ~key:(hash_of_state start_state) ~data:0;

  let state_with_lowest_cost () =
    let lc_state =
      List.fold !queue ~init:(List.hd_exn !queue) ~f:(fun acc state ->
          let cost = Map.find_exn !cost_map (hash_of_state state) in
          if cost < Map.find_exn !cost_map (hash_of_state acc) then state
          else acc)
    in
    queue := List.filter !queue ~f:(fun x -> not (is_equal_state x lc_state));
    lc_state
  in

  let rec find_target' () =
    if List.is_empty !queue then failwith "No path found"
    else
      let state = state_with_lowest_cost () in
      if
        state.x = target.x && state.y = target.y
        && is_equal_tool state.tool Torch
      then (* We found the target *)
        Map.find_exn !cost_map (hash_of_state state)
      else
        let curr_cost = Map.find_exn !cost_map (hash_of_state state) in
        let next_states_with_tool_change =
          List.map
            (valid_tools (region_type { x = state.x; y = state.y }))
            ~f:(fun tool ->
              if is_equal_tool tool state.tool then None
              else
                let next_state = { state with tool } in
                let prev_visited_cost =
                  match Map.find !cost_map (hash_of_state next_state) with
                  | None -> Int.max_value
                  | Some x -> x
                in
                if curr_cost + 7 > prev_visited_cost then None
                else Some next_state)
          |> List.filter_opt
        in
        List.iter next_states_with_tool_change ~f:(fun next_state ->
            cost_map :=
              Map.set !cost_map ~key:(hash_of_state next_state)
                ~data:(curr_cost + 7));
        queue := !queue @ next_states_with_tool_change;
        let next_states_with_move =
          List.map
            [
              { x = state.x + 1; y = state.y; tool = state.tool };
              { x = state.x - 1; y = state.y; tool = state.tool };
              { x = state.x; y = state.y + 1; tool = state.tool };
              { x = state.x; y = state.y - 1; tool = state.tool };
            ]
            ~f:(fun next_state ->
              if next_state.x < 0 || next_state.y < 0 then None
              else
                let prev_visited_cost =
                  match Map.find !cost_map (hash_of_state next_state) with
                  | None -> Int.max_value
                  | Some x -> x
                in
                if curr_cost + 1 > prev_visited_cost then None
                else if not (is_valid_tool next_state) then None
                else if next_state.x > target.x * 10 then None
                  (* Limit the area that is searched to save some time *)
                else Some next_state)
          |> List.filter_opt
        in
        List.iter next_states_with_move ~f:(fun next_state ->
            cost_map :=
              Map.set !cost_map ~key:(hash_of_state next_state)
                ~data:(Map.find_exn !cost_map (hash_of_state state) + 1));
        queue := !queue @ next_states_with_move;
        find_target' ()
  in

  find_target' ()

let resultP2 = find_target start_state
