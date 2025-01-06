open Core

let isExample = false

let filename =
  if isExample then "lib/day19/example.txt" else "lib/day19/input.txt"

let aoc_input = In_channel.read_lines filename
let registers = ref (Array.create ~len:6 0)

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

type program_line_T = { op : opT; a : int; b : int; c : int }

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

let do_instr program_line =
  let { op; a; b; c } = program_line in

  let reg_of_a =
    if a >= 0 && a < Array.length !registers then Array.get !registers a else -1
  in
  let reg_of_b =
    if b >= 0 && b < Array.length !registers then Array.get !registers b else -1
  in
  let res =
    match op with
    | Addr -> reg_of_a + reg_of_b
    | Addi -> reg_of_a + b
    | Mulr -> reg_of_a * reg_of_b
    | Muli -> reg_of_a * b
    | Banr -> reg_of_a land reg_of_b
    | Bani -> reg_of_a land b
    | Borr -> reg_of_a lor reg_of_b
    | Bori -> reg_of_a lor b
    | Setr -> reg_of_a
    | Seti -> a
    | Gtir -> if a > reg_of_b then 1 else 0
    | Gtri -> if reg_of_a > b then 1 else 0
    | Gtrr -> if reg_of_a > reg_of_b then 1 else 0
    | Eqir -> if a = reg_of_b then 1 else 0
    | Eqri -> if reg_of_a = b then 1 else 0
    | Eqrr -> if reg_of_a = reg_of_b then 1 else 0
  in
  Array.set !registers c res

let get_program_from_input input =
  let ip_reg =
    input |> List.hd_exn |> String.split ~on:' ' |> List.last_exn
    |> Int.of_string
  in
  let program =
    List.tl_exn input
    |> List.map ~f:(String.split ~on:' ')
    |> List.map ~f:(fun x ->
           let op = List.hd_exn x |> op_of_string in
           let a = List.nth_exn x 1 |> Int.of_string in
           let b = List.nth_exn x 2 |> Int.of_string in
           let c = List.nth_exn x 3 |> Int.of_string in
           { op; a; b; c })
    |> Array.of_list
  in
  (ip_reg, program)

let ip_reg, program = get_program_from_input aoc_input

let print_regs () =
  Printf.printf "%s\n"
    (Array.to_list !registers |> List.to_string ~f:Int.to_string)

let waitForEnter s =
  Printf.printf "%s" s;
  Out_channel.flush stdout;
  let _ = In_channel.(input_line stdin) in
  ()

let print_program_line program_line =
  let { op; a; b; c } = program_line in
  Printf.printf "%s %d %d %d\n" (string_of_op op) a b c

let run_program ip_reg program =
  let rec run_program' () =
    let ip = Array.get !registers ip_reg in
    if ip >= Array.length program then ()
    else
      let program_line = program.(ip) in
      print_program_line program_line;
      do_instr program_line;
      let ip' = Array.get !registers ip_reg in
      Array.set !registers ip_reg (ip' + 1);
      print_regs ();
      waitForEnter "\n";
      run_program' ()
  in
  run_program' ()

let () = run_program ip_reg program
let resultP1 = Array.get !registers 0
let resultP2 = 22157688
(* I have done this in dart already. Its the sum of all factors of 10551260 *)
