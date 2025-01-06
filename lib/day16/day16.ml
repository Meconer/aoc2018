open Core

let isExample = false

let filename =
  if isExample then "lib/day16/example.txt" else "lib/day16/input.txt"

let aoc_input = In_channel.read_lines filename
let registers = ref (Array.create ~len:4 0)

type opT =
  | Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr

let string_of_op = function
  | Addr -> "addr"
  | Addi -> "addi"
  | Mulr -> "mulr"
  | Muli -> "muli"
  | Banr -> "banr"
  | Bani -> "bani"
  | Borr -> "borr"
  | Bori -> "bori"
  | Setr -> "setr"
  | Seti -> "seti"
  | Gtir -> "gtir"
  | Gtri -> "gtri"
  | Gtrr -> "gtrr"
  | Eqir -> "eqir"
  | Eqri -> "eqri"
  | Eqrr -> "eqrr"

let op_of_string = function
  | "addr" -> Addr
  | "addi" -> Addi
  | "mulr" -> Mulr
  | "muli" -> Muli
  | "banr" -> Banr
  | "bani" -> Bani
  | "borr" -> Borr
  | "bori" -> Bori
  | "setr" -> Setr
  | "seti" -> Seti
  | "gtir" -> Gtir
  | "gtri" -> Gtri
  | "gtrr" -> Gtrr
  | "eqir" -> Eqir
  | "eqri" -> Eqri
  | "eqrr" -> Eqrr
  | _ -> failwith "Invalid op"

let do_instr op inp_a inp_b out_c =
  let a = Array.get !registers inp_a in
  let b = Array.get !registers inp_b in
  let res =
    match op with
    | Addr -> a + b
    | Addi -> a + inp_b
    | Mulr -> a * b
    | Muli -> a * inp_b
    | Banr -> a land b
    | Bani -> a land inp_b
    | Borr -> a lor b
    | Bori -> a lor inp_b
    | Setr -> a
    | Seti -> inp_a
    | Gtir -> if inp_a > b then 1 else 0
    | Gtri -> if a > inp_b then 1 else 0
    | Gtrr -> if a > b then 1 else 0
    | Eqir -> if inp_a = b then 1 else 0
    | Eqri -> if a = inp_b then 1 else 0
    | Eqrr -> if a = b then 1 else 0
  in
  Array.set !registers out_c res

let group_input lines =
  let rec first_group_loop first_group acc lines last_line_was_empty =
    match lines with
    | [] -> failwith "Invalid input"
    | line :: tail when String.is_empty line ->
        if last_line_was_empty then (List.rev first_group, tail)
        else
          let section = List.rev acc in
          first_group_loop (section :: first_group) [] tail true
    | line :: tail -> first_group_loop first_group (line :: acc) tail false
  in

  let rec second_group_loop second_group lines =
    match lines with
    | [] -> List.rev second_group
    | line :: tail when String.is_empty line ->
        (* Skip empty line at start*)
        second_group_loop second_group tail
    | line :: tail -> second_group_loop (line :: second_group) tail
  in

  let first_group, rest_of_lines = first_group_loop [] [] lines false in
  let second_group = second_group_loop [] rest_of_lines in
  (first_group, second_group)

let test_lines, program_lines = group_input aoc_input

let all_ops =
  [
    Addr;
    Addi;
    Mulr;
    Muli;
    Banr;
    Bani;
    Borr;
    Bori;
    Setr;
    Seti;
    Gtir;
    Gtri;
    Gtrr;
    Eqir;
    Eqri;
    Eqrr;
  ]

let try_test test_lines ops_to_try =
  match test_lines with
  | [ before; instr; after ] ->
      let before =
        Scanf.sscanf before "Before: [%d, %d, %d, %d]" (fun a b c d ->
            [| a; b; c; d |])
      in
      let instr =
        Scanf.sscanf instr "%d %d %d %d" (fun a b c d -> [| a; b; c; d |])
      in
      let after =
        Scanf.sscanf after "After: [%d, %d, %d, %d]" (fun a b c d ->
            [| a; b; c; d |])
      in
      let op_code, inp_a, inp_b, out_c =
        (instr.(0), instr.(1), instr.(2), instr.(3))
      in
      let ops_with_matching_code =
        List.map ~f:(fun x -> (x, op_code)) ops_to_try
      in
      let possible_ops =
        List.filter
          ~f:(fun op ->
            registers := Array.copy before;
            do_instr (fst op) inp_a inp_b out_c;
            Array.equal Int.equal !registers after)
          ops_with_matching_code
      in
      possible_ops
  | _ -> failwith "Invalid test"

let op_equal op1 op2 = String.equal (string_of_op op1) (string_of_op op2)

let resultP1 =
  List.count
    ~f:(fun test -> List.length (try_test test all_ops) >= 3)
    test_lines

let match_ops tests ops_to_try =
  let rec loop ops_to_try acc =
    let test_run = List.map ~f:(fun test -> try_test test ops_to_try) tests in
    let first_match = List.find ~f:(fun x -> List.length x = 1) test_run in
    match first_match with
    | None -> acc (* No more matches*)
    | Some match_ ->
        let op, op_code = List.hd_exn match_ in
        let ops_to_try =
          List.filter ~f:(fun x -> not (op_equal op x)) ops_to_try
        in
        let acc = (op, op_code) :: acc in
        loop ops_to_try acc
  in
  loop ops_to_try []

let op_map =
  List.fold
    ~init:(Map.empty (module Int))
    ~f:(fun acc (op, op_code) -> Map.set acc ~key:op_code ~data:op)
    (match_ops test_lines all_ops)

let run_program program op_map =
  registers := Array.create ~len:4 0;
  List.iter
    ~f:(fun line ->
      let instr =
        Scanf.sscanf line "%d %d %d %d" (fun a b c d -> [| a; b; c; d |])
      in
      let op_code, inp_a, inp_b, out_c =
        (instr.(0), instr.(1), instr.(2), instr.(3))
      in
      let op = Map.find_exn op_map op_code in
      do_instr op inp_a inp_b out_c)
    program

let () = run_program program_lines op_map
let resultP2 = Array.get !registers 0
