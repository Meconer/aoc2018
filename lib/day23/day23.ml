open Core

let isExample = false

let filename =
  if isExample then "lib/day23/example.txt" else "lib/day23/input.txt"

let aoc_input = In_channel.read_lines filename

type coord_T = { x : int; y : int; z : int }
type nanobot_T = { coord : coord_T; r : int }
type lineT = { a : coord_T; b : coord_T }
type box_T = { min_corner : coord_T; max_corner : coord_T }

let nanobot_of_string line =
  let parts = String.split ~on:' ' line in
  let pos =
    String.chop_prefix_exn (List.hd_exn parts) ~prefix:"pos=<"
    |> String.chop_suffix_exn ~suffix:">,"
    |> String.split ~on:',' |> List.map ~f:Int.of_string
  in
  let x = List.nth_exn pos 0 in
  let y = List.nth_exn pos 1 in
  let z = List.nth_exn pos 2 in
  let r =
    List.nth_exn parts 1 |> String.chop_prefix_exn ~prefix:"r=" |> Int.of_string
  in
  { coord = { x; y; z }; r }

let nanobots = List.map ~f:nanobot_of_string aoc_input

let manhattan_distance a b =
  let x = abs (a.x - b.x) in
  let y = abs (a.y - b.y) in
  let z = abs (a.z - b.z) in
  x + y + z

(* Get the nanobot that has the largest range *)
let strongest_nanobot =
  List.max_elt ~compare:(fun a b -> a.r - b.r) nanobots |> Option.value_exn

let in_range_of_strongest_nanobot nanobot =
  manhattan_distance nanobot.coord strongest_nanobot.coord
  <= strongest_nanobot.r

let corners_of_octahedron nanobot =
  let x = nanobot.coord.x in
  let y = nanobot.coord.y in
  let z = nanobot.coord.z in
  let r = nanobot.r in
  [
    { x = x + r; y; z };
    { x = x - r; y; z };
    { x; y = y + r; z };
    { x; y = y - r; z };
    { x; y; z = z + r };
    { x; y; z = z - r };
  ]

let resultP1 = List.count ~f:in_range_of_strongest_nanobot nanobots

(* Get a box containing the reach of all of the nanobots *)
let enclosing_box nanobots =
  let rec loop min_corner max_corner nanobots =
    match nanobots with
    | [] -> { min_corner; max_corner }
    | nanobot :: tail ->
        let corners = corners_of_octahedron nanobot in
        let min_corner' =
          List.fold corners ~init:min_corner ~f:(fun min_corner corner ->
              {
                x = min min_corner.x corner.x;
                y = min min_corner.y corner.y;
                z = min min_corner.z corner.z;
              })
        in
        let max_corner' =
          List.fold corners ~init:max_corner ~f:(fun max_corner corner ->
              {
                x = max max_corner.x corner.x;
                y = max max_corner.y corner.y;
                z = max max_corner.z corner.z;
              })
        in
        loop min_corner' max_corner' tail
  in
  loop { x = 0; y = 0; z = 0 } { x = 0; y = 0; z = 0 } nanobots

(* Take a list of boxes and split them in half in x, y and z,
   resulting in 8 new boxes for each box in the list.
   Only split if the side is larger then 1 *)
let split_boxes boxes =
  let split_in_x =
    List.concat_map boxes ~f:(fun box ->
        let x_range = box.max_corner.x - box.min_corner.x in
        if x_range = 0 then [ box ]
        else
          let x_mid = box.min_corner.x + (x_range / 2) in
          [
            {
              min_corner = box.min_corner;
              max_corner =
                { x = x_mid; y = box.max_corner.y; z = box.max_corner.z };
            };
            {
              min_corner =
                { x = x_mid + 1; y = box.min_corner.y; z = box.min_corner.z };
              max_corner = box.max_corner;
            };
          ])
  in

  let split_in_y =
    List.concat_map split_in_x ~f:(fun box ->
        let y_range = box.max_corner.y - box.min_corner.y in
        if y_range = 0 then [ box ]
        else
          let y_mid = box.min_corner.y + (y_range / 2) in
          [
            {
              min_corner = box.min_corner;
              max_corner = { box.max_corner with y = y_mid };
            };
            {
              min_corner = { box.min_corner with y = y_mid + 1 };
              max_corner = { box.max_corner with y = box.max_corner.y };
            };
          ])
  in
  List.concat_map split_in_y ~f:(fun box ->
      let z_range = box.max_corner.z - box.min_corner.z in
      if z_range = 0 then [ box ]
      else
        let z_mid = box.min_corner.z + (z_range / 2) in
        [
          {
            min_corner = box.min_corner;
            max_corner = { box.max_corner with z = z_mid };
          };
          {
            min_corner = { box.min_corner with z = z_mid + 1 };
            max_corner = { box.max_corner with z = box.max_corner.z };
          };
        ])

let corners_of_box box =
  [
    box.min_corner;
    { x = box.min_corner.x; y = box.min_corner.y; z = box.max_corner.z };
    { x = box.min_corner.x; y = box.max_corner.y; z = box.min_corner.z };
    { x = box.min_corner.x; y = box.max_corner.y; z = box.max_corner.z };
    { x = box.max_corner.x; y = box.min_corner.y; z = box.min_corner.z };
    { x = box.max_corner.x; y = box.min_corner.y; z = box.max_corner.z };
    { x = box.max_corner.x; y = box.max_corner.y; z = box.min_corner.z };
    box.max_corner;
  ]

let edges_of_box box =
  [
    { a = box.min_corner; b = { box.min_corner with x = box.max_corner.x } };
    { a = box.min_corner; b = { box.min_corner with y = box.max_corner.y } };
    { a = box.min_corner; b = { box.min_corner with z = box.max_corner.z } };
    { a = { box.max_corner with x = box.min_corner.x }; b = box.max_corner };
    { a = { box.max_corner with y = box.min_corner.y }; b = box.max_corner };
    { a = { box.max_corner with z = box.min_corner.z }; b = box.max_corner };
  ]

(* Check to see if an edge of a box is reachable by a nanobot.
   The box edges is parallell to the x, y and z axes *)
let is_edge_in_reach_of_nanobot edge nanobot =
  if edge.a.x > nanobot.coord.x + nanobot.r then false
  else if edge.b.x < nanobot.coord.x - nanobot.r then false
  else if edge.a.y > nanobot.coord.y + nanobot.r then false
  else if edge.b.y < nanobot.coord.y - nanobot.r then false
  else if edge.a.z > nanobot.coord.z + nanobot.r then false
  else if edge.b.z < nanobot.coord.z - nanobot.r then false
  else if edge.a.x = edge.b.x && edge.a.y = edge.b.y then
    (* Edge is vertical, parallell to z axis *)
    let x = edge.a.x in
    let y = edge.a.y in
    let z_min = min edge.a.z edge.b.z in
    let z_max = max edge.a.z edge.b.z in
    let z = nanobot.coord.z in
    if z < z_min then
      manhattan_distance { x; y; z = z_min } nanobot.coord <= nanobot.r
    else if z > z_max then
      manhattan_distance { x; y; z = z_max } nanobot.coord <= nanobot.r
    else manhattan_distance { x; y; z } nanobot.coord <= nanobot.r
  else if edge.a.x = edge.b.x && edge.a.z = edge.b.z then
    (* Edge is parallell to y axis *)
    let x = edge.a.x in
    let y = nanobot.coord.y in
    let y_min = min edge.a.y edge.b.y in
    let y_max = max edge.a.y edge.b.y in
    let z = edge.a.z in
    if y < y_min then
      manhattan_distance { x; y = y_min; z } nanobot.coord <= nanobot.r
    else if y > y_max then
      manhattan_distance { x; y = y_max; z } nanobot.coord <= nanobot.r
    else manhattan_distance { x; y; z } nanobot.coord <= nanobot.r
  else
    (* Edge is parallell to x axis *)
    let x = nanobot.coord.x in
    let x_min = min edge.a.x edge.b.x in
    let x_max = max edge.a.x edge.b.x in
    let y = edge.a.y in
    let z = edge.a.z in
    if x < x_min then
      manhattan_distance { x = x_min; y; z } nanobot.coord <= nanobot.r
    else if x > x_max then
      manhattan_distance { x = x_max; y; z } nanobot.coord <= nanobot.r
    else manhattan_distance { x; y; z } nanobot.coord <= nanobot.r

(* Check if a nanobot can reach a box *)
let can_nano_bot_reach box nanobot =
  (* Check if center point is inside box *)
  if
    nanobot.coord.x >= box.min_corner.x
    && nanobot.coord.x <= box.max_corner.x
    && nanobot.coord.y >= box.min_corner.y
    && nanobot.coord.y <= box.max_corner.y
    && nanobot.coord.z >= box.min_corner.z
    && nanobot.coord.z <= box.max_corner.z
  then true
  else
    (* Check if any corner is inside the box *)
    let corners = corners_of_octahedron nanobot in
    if
      List.exists corners ~f:(fun corner ->
          corner.x >= box.min_corner.x
          && corner.x <= box.max_corner.x
          && corner.y >= box.min_corner.y
          && corner.y <= box.max_corner.y
          && corner.z >= box.min_corner.z
          && corner.z <= box.max_corner.z)
    then true
    else
      (* Check if any of the box corners is inside the nanobot range *)
      let corners = corners_of_box box in
      if
        List.exists corners ~f:(fun corner ->
            manhattan_distance corner nanobot.coord <= nanobot.r)
      then true
      else if
        (* Check if the nanobot is completely outside the box *)
        box.max_corner.x < nanobot.coord.x - nanobot.r
      then false
      else if box.min_corner.x > nanobot.coord.x + nanobot.r then false
      else if box.max_corner.y < nanobot.coord.y - nanobot.r then false
      else if box.min_corner.y > nanobot.coord.y + nanobot.r then false
      else if box.max_corner.z < nanobot.coord.z - nanobot.r then false
      else if box.min_corner.z > nanobot.coord.z + nanobot.r then false
      else
        (* Check if any of the box edges is in reach of the nanobot *)
        let box_edges = edges_of_box box in
        List.exists box_edges ~f:(fun edge ->
            is_edge_in_reach_of_nanobot edge nanobot)

(* Save the boxes that has the biggest nanobot reach and throw away the rest *)
let save_max_count boxes counts max_count =
  List.filter_mapi boxes ~f:(fun i box ->
      if List.nth_exn counts i = max_count then Some box else None)

let size_of_box box =
  box.max_corner.z - box.min_corner.z + 1
  + (box.max_corner.y - box.min_corner.y + 1)
  + (box.max_corner.x - box.min_corner.x + 1)

let print_box box =
  Printf.printf "Box %d %d %d %d %d %d\n" box.min_corner.x box.min_corner.y
    box.min_corner.z box.max_corner.x box.max_corner.y box.max_corner.z

let print_boxes boxes = List.iter boxes ~f:(fun box -> print_box box)

let find_best_point () =
  (* Get a box that encloses all off the bot ranges *)
  let box = enclosing_box nanobots in

  (* Split the boxes in the box list in 8 parts. By half in x, y and z *)
  let rec loop boxes =
    (* Calculate the sizes of the boxes *)
    let sizes = List.map boxes ~f:size_of_box in
    let max_size =
      List.max_elt sizes ~compare:Int.compare |> Option.value_exn
    in
    (* Stop if the biggest box is enclosing just one point *)
    (* Size is 3 then because we calculate the size by adding the side lengths of the box,
       due to the big numbers. If we would have multiplicated to get the volume it would overflow
       the integer *)
    if max_size <= 3 then boxes
    else
      (* Split the boxes *)
      let boxes = split_boxes boxes in

      (* Count how many nanobots are in reach of each box *)
      let counts =
        List.map boxes ~f:(fun box ->
            List.count
              ~f:(fun nanobot -> can_nano_bot_reach box nanobot)
              nanobots)
      in

      (* Find the box with the most nanobots in reach *)
      let max_count =
        List.max_elt ~compare:Int.compare counts |> Option.value_exn
      in

      (* Save the boxes with the most nanobots in reach *)
      let remaining_boxes = save_max_count boxes counts max_count in

      loop remaining_boxes
  in

  (* Start with just the box enclosing all nanobots *)
  loop [ box ]

(* Find the box that encloses the best point. It should be just one single point *)
let resulting_box = List.hd_exn (find_best_point ())

let resultP2 =
  manhattan_distance resulting_box.min_corner { x = 0; y = 0; z = 0 }
