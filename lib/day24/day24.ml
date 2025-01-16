open Core

let isExample = false

let filename =
  if isExample then "lib/day24/example.txt" else "lib/day24/input.txt"

let aoc_input = In_channel.read_lines filename

type groupT = Immune | Infection
type attackT = Cold | Fire | Bludgeoning | Slashing | Radiation

type group_typeT = {
  id : int;
  group_type : groupT;
  units : int;
  hit_points : int;
  weaknesses : attackT list;
  immunities : attackT list;
  attack_damage : int;
  attack_type : attackT;
  initiative : int;
  selected_target : int option;
}

let is_attack_equal a b =
  match (a, b) with
  | Cold, Cold -> true
  | Fire, Fire -> true
  | Bludgeoning, Bludgeoning -> true
  | Slashing, Slashing -> true
  | Radiation, Radiation -> true
  | _ -> false

let is_group_equal a b =
  match (a, b) with
  | Immune, Immune -> true
  | Infection, Infection -> true
  | _ -> false

let find_str_in_parens line =
  let regex_str = "\\((.*)\\)" in
  let re = Re2.create_exn regex_str in
  if not (Re2.matches re line) then ""
    (* Does not match if we dont have a parenthesis*)
  else
    let matches = Re2.find_submatches_exn re line in
    Option.value_exn matches.(1)

let attack_of_string s =
  match s with
  | "cold" -> Cold
  | "fire" -> Fire
  | "bludgeoning" -> Bludgeoning
  | "slashing" -> Slashing
  | "radiation" -> Radiation
  | _ -> failwith "Invalid attack type"

let wait_for_input () = ignore (In_channel.input_line_exn In_channel.stdin)

let find_weaknesses line =
  let s = find_str_in_parens line in
  (* The string inside the parenthesis *)
  let re2 = Re2.create_exn "weak to ([^;]*)" in
  if not (Re2.matches re2 s) then []
    (* Does not match if we dont have a weakness *)
  else
    let matches = Re2.find_submatches_exn re2 s in

    let match_str = Option.value_exn matches.(1) in
    String.split match_str ~on:','
    |> List.map ~f:String.strip
    |> List.map ~f:attack_of_string

let find_immunities line =
  let s = find_str_in_parens line in
  (* The string inside the parenthesis *)
  let re2 = Re2.create_exn "immune to ([^;]*)" in
  if not (Re2.matches re2 s) then []
    (* Does not match if we dont have an immunity *)
  else
    let matches = Re2.find_submatches_exn re2 s in

    let match_str = Option.value_exn matches.(1) in
    String.split match_str ~on:','
    |> List.map ~f:String.strip
    |> List.map ~f:attack_of_string

let parse_group line id group_type =
  let re =
    Re2.create_exn
      "(\\d+) units each with (\\d+) hit points .*with an attack that does \
       (\\d+) (\\w+) damage at initiative (\\d+)"
  in
  let matches = Re2.find_submatches_exn re line in
  let units = Int.of_string (Option.value_exn matches.(1)) in
  let hit_points = Int.of_string (Option.value_exn matches.(2)) in

  let weaknesses = find_weaknesses line in

  let immunities = find_immunities line in
  let attack_damage = Int.of_string (Option.value_exn matches.(3)) in
  let attack_type = Option.value_exn matches.(4) |> attack_of_string in
  let initiative = Int.of_string (Option.value_exn matches.(5)) in
  {
    id;
    group_type;
    units;
    hit_points;
    weaknesses;
    immunities;
    attack_damage;
    attack_type;
    initiative;
    selected_target = None;
  }

let parse_input input =
  let rec loop acc_immunes acc_infections section imm_id inf_id lines =
    match lines with
    | [] -> (List.rev acc_immunes, List.rev acc_infections)
    | "Immune System:" :: tl -> loop [] acc_infections Immune imm_id inf_id tl
    | "Infection:" :: tl -> loop acc_immunes [] Infection imm_id inf_id tl
    | "" :: tl -> loop acc_immunes acc_infections section imm_id inf_id tl
    | line :: tl -> (
        match section with
        | Immune ->
            let group = parse_group line imm_id section in
            loop (group :: acc_immunes) acc_infections section (imm_id + 1)
              inf_id tl
        | Infection ->
            let group = parse_group line inf_id section in
            loop acc_immunes (group :: acc_infections) section imm_id
              (inf_id + 1) tl)
  in
  loop [] [] Immune 0 100 input

let immune_groups, infection_groups = parse_input aoc_input
let effective_power group = group.units * group.attack_damage

let damage attacker defender =
  let power = effective_power attacker in
  let damage =
    if
      List.exists defender.immunities ~f:(fun imm ->
          is_attack_equal imm attacker.attack_type)
    then 0
    else if
      List.exists defender.weaknesses ~f:(fun weakness ->
          is_attack_equal weakness attacker.attack_type)
    then 2 * power
    else power
  in
  damage

let str_of_group_t group_t =
  match group_t with Immune -> "Imm" | Infection -> "Inf"

let str_of_attack_t attack_t =
  match attack_t with
  | Cold -> "Cold"
  | Fire -> "Fire"
  | Bludgeoning -> "Bldg"
  | Slashing -> "Slsh"
  | Radiation -> "Radi"

let print_group group =
  let target =
    if Option.is_some group.selected_target then
      string_of_int (Option.value_exn group.selected_target)
    else "-"
  in
  let weaknesses =
    if List.is_empty group.weaknesses then "-"
    else List.map group.weaknesses ~f:str_of_attack_t |> String.concat ~sep:", "
  in
  let immunities =
    if List.is_empty group.immunities then "-"
    else List.map group.immunities ~f:str_of_attack_t |> String.concat ~sep:", "
  in
  Printf.printf
    "Group %d: %d un, %d hp, %d %s dmg, weak: %s, imm: %s, %d int, %s, eff pow \
     %d Targ: %s\n"
    group.id group.units group.hit_points group.attack_damage
    (str_of_attack_t group.attack_type)
    weaknesses immunities group.initiative
    (str_of_group_t group.group_type)
    (effective_power group) target

let print_all_groups groups = List.iter groups ~f:(fun g -> print_group g)

let attack attacker defender =
  let damage = damage attacker defender in
  let units_killed = damage / defender.hit_points in
  let units_left = max (defender.units - units_killed) 0 in
  { defender with units = units_left }

let select_target attacker defenders =
  if List.is_empty defenders then (None, defenders)
  else
    let target =
      List.hd_exn
        (List.sort defenders ~compare:(fun a b ->
             let damage_a = damage attacker a in
             let damage_b = damage attacker b in
             if damage_a = damage_b then
               if effective_power a = effective_power b then
                 Int.descending a.initiative b.initiative
               else Int.descending (effective_power a) (effective_power b)
             else Int.descending damage_a damage_b))
    in
    let damage = damage attacker target in
    if damage = 0 then (None, defenders)
    else
      let remaining_defenders =
        List.filter defenders ~f:(fun d -> d.id <> target.id)
      in
      (Some target, remaining_defenders)

let rec select_targets attackers defenders =
  let attackers =
    List.map attackers ~f:(fun g -> { g with selected_target = None })
  in
  let attackers =
    List.sort attackers ~compare:(fun a b ->
        let cmp = Int.descending (effective_power a) (effective_power b) in
        if cmp = 0 then Int.descending a.initiative b.initiative else cmp)
  in
  let rec select_loop attackers_sel_done attackers remaining_defenders =
    if List.is_empty remaining_defenders then attackers_sel_done @ attackers
    else
      match attackers with
      | [] -> attackers_sel_done
      | attacker :: remaining_attackers -> (
          let target, remaining_defenders =
            select_target attacker remaining_defenders
          in

          match (target, remaining_defenders) with
          | None, [] -> attackers_sel_done @ (attacker :: remaining_attackers)
          | None, _ ->
              select_loop
                (attacker :: attackers_sel_done)
                remaining_attackers remaining_defenders
          | Some target, remaining_defenders ->
              let attacker =
                { attacker with selected_target = Some target.id }
              in
              select_loop
                (attacker :: attackers_sel_done)
                remaining_attackers remaining_defenders)
  in
  select_loop [] attackers defenders

let attack_phase groups =
  let groups_in_attack_order =
    List.sort groups ~compare:(fun a b ->
        Int.descending a.initiative b.initiative)
  in
  let rec attack_loop groups_done groups_remaining =
    match groups_remaining with
    | [] -> groups_done
    | attacker :: remaining_groups -> (
        match attacker.selected_target with
        | None -> attack_loop (groups_done @ [ attacker ]) remaining_groups
        | Some target_id -> (
            (* Find the target. It can be in the groups_done or in the remaining_groups*)
            let target = List.find groups_done ~f:(fun g -> g.id = target_id) in
            if Option.is_some target then
              (* We found it in the groups_done, do the attack and save the result in target *)
              let target = attack attacker (Option.value_exn target) in
              (* Remove the target from the groups_done and add the new target *)
              let groups_done =
                target
                :: List.filter groups_done ~f:(fun g -> g.id <> target_id)
              in
              attack_loop (groups_done @ [ attacker ]) remaining_groups
            else
              (* The target was not in the done group so it must be in the
                 remaining_groups, find it, do the attack and save the result
                 in target *)
              let target =
                List.find remaining_groups ~f:(fun g -> g.id = target_id)
              in
              match target with
              | None ->
                  print_endline
                    "Target not found. Probably killed in a previous attack";
                  wait_for_input ();
                  (* The target was not found, it was killed in a previous attack *)
                  attack_loop (groups_done @ [ attacker ]) remaining_groups
              | Some target ->
                  let target = attack attacker target in
                  (* Remove the target from the remaining_groups and add the new target *)
                  (* Sort the remaining groups by initiative *)
                  let remaining_groups =
                    target
                    :: List.filter remaining_groups ~f:(fun g ->
                           g.id <> target_id)
                    |> List.sort ~compare:(fun a b ->
                           Int.descending a.initiative b.initiative)
                  in
                  attack_loop (groups_done @ [ attacker ]) remaining_groups))
  in

  attack_loop [] groups_in_attack_order

let print_groups immune_groups infection_groups =
  Printf.printf "Immune System:\n";
  List.iter immune_groups ~f:(fun g -> print_group g);
  Printf.printf "Infection:\n";
  List.iter infection_groups ~f:(fun g -> print_group g)

let has_targets groups =
  List.exists groups ~f:(fun g -> Option.is_some g.selected_target)

let last_total_units = ref (-1)

let rec fight immune_groups infection_groups =
  let immune_groups = select_targets immune_groups infection_groups in

  let infection_groups = select_targets infection_groups immune_groups in

  if not (has_targets (immune_groups @ infection_groups)) then (1, 1)
    (* We have a tie.*)
  else
    let groups = attack_phase (immune_groups @ infection_groups) in
    let infection_groups =
      List.filter groups ~f:(fun g ->
          is_group_equal g.group_type Infection && g.units > 0)
    in
    let immune_groups =
      List.filter groups ~f:(fun g ->
          is_group_equal g.group_type Immune && g.units > 0)
    in
    let immune_units =
      List.fold immune_groups ~init:0 ~f:(fun acc g -> acc + g.units)
    in
    let infection_units =
      List.fold infection_groups ~init:0 ~f:(fun acc g -> acc + g.units)
    in
    if immune_units = 0 || infection_units = 0 then
      (immune_units, infection_units)
    else
      let total_units = immune_units + infection_units in
      if total_units = !last_total_units then
        (* Units stopped decreasing. We have a tie *)
        (1, 1)
      else (
        last_total_units := total_units;
        fight immune_groups infection_groups)

let result = fight immune_groups infection_groups
let resultP1 = max (fst result) (snd result)

let try_boosts immune_groups infection_groups high_boost =
  let rec loop low_boost high_boost boost =
    let immune_groups =
      List.map immune_groups ~f:(fun g ->
          { g with attack_damage = g.attack_damage + boost })
    in
    last_total_units := -1;
    let result = fight immune_groups infection_groups in
    if fst result = 0 then
      (* Immune system lost, we need more boost *)
      let low_boost = boost in
      let new_boost = ((high_boost - low_boost) / 2) + low_boost in
      let new_boost = if new_boost = boost then boost + 1 else new_boost in
      loop low_boost high_boost new_boost
    else if snd result = 0 then
      (* Infection lost, check if we found the minimum boost *)
      if high_boost - low_boost = 1 then fst result
      else
        let high_boost = boost in
        let new_boost = ((high_boost - low_boost) / 2) + low_boost in
        loop low_boost high_boost new_boost
    else
      (* We have a tie. Inc boost by 1 *)
      loop (low_boost + 1) high_boost (boost + 1)
  in
  loop 0 high_boost (high_boost / 2)

let resultP2 = try_boosts immune_groups infection_groups 100
