open Simulator
open Utility
open Command
open Commandutil
open Functionutil

(* let min_soldier_id = 1 *)
let min_soldier_id = 8

let min_free_field_id = 8

let revive_any_dead next_routine =
  let dead_slot = find_dead_prop_slot 0 255 in
  if dead_slot != -1 then begin
    let reviver_slot = find_alive_prop_slot min_free_field_id 255 in
    if reviver_slot != -1 then begin
      set_field_to_value
	reviver_slot
	dead_slot
	(fun _ ->
	  lapp "revive" reviver_slot)
    end else begin
      (* Try again, using important registers are not good, but much better than do nothing *)
      let reviver_slot = find_alive_prop_slot 0 255 in
      if reviver_slot != -1 then begin
	set_field_to_value
	  reviver_slot
	  dead_slot
	  (fun _ ->
	    lapp "revive" reviver_slot)
      end else
	nop ()
    end
  end else
    next_routine ()

let zombienize next_strategy =
  let dead_slot = find_dead_opp_slot_backward 0 255 in
  if dead_slot = -1 then
    next_strategy ()
  else begin
    let first, vitality = find_opp_slot_with_biggest_vitality 0 255 in
    let second, _ = find_alive_opp_slot_forward_ex first 0 255 in
    if second = -1 || vitality < 9000 then
      next_strategy ()
    else begin
      let reg_attack_j_backup_field, _ = get_prop_slot reg_attack_j_backup in
      apply_function_to_reg3
	reg_attack_j_backup_field
	(ZombieI(Value(255 - dead_slot)))
	reg_attack_j_backup
	(fun _ ->
	  copy_field
	    reg_attack_n_backup
	    reg_attack_j_backup
	    (fun _ ->
	      build_help_bomb
		reg3
		vitality
		(fun _ ->
		  set_field_to_value
      		    reg2
		    first (* i *)
	            (fun _ ->
		      set_field_to_value
      			reg1
			second (* j *)
			(fun _ ->
			  set_field_to_value
      			    reg0
      			    vitality (* n *)
			    (fun _ -> rapp reg_attack_n_backup "zero"))))))
	(fun _ ->
	  build_zombie_i reg_attack_j_backup dead_slot)
    end
  end

let run_self_help slot vitality =
  build_help
    slot
    slot
    (lower_power (vitality - 1))
    reg_help_command
    true
    (fun _ -> rapp reg_help_command "zero")

let run_help_with_source helper_slot target helper_vitality =
  build_help
    helper_slot
    target
    (lower_power (helper_vitality - 1))
    reg_help_command
    (helper_slot = target)
    (fun _ -> rapp reg_help_command "zero")

let find_best_helper target =
  let helping_source, helping_target = find_helping_slot reg_help_command in
    if helping_source >= 0 && (helping_target < 0 || helping_target = target) then
      helping_source, (get_vitality helping_source)
    else
      find_slot_with_vitality_ge 10000 min_soldier_id 255

let run_help target target_vitality =
  let helper_slot, helper_vitality = find_best_helper target in
    if helper_slot != -1 then
      run_help_with_source
	helper_slot
	target
	helper_vitality
    else
      run_self_help target target_vitality

let estimate_damage opp_vitality =
  (opp_vitality - 1) * 9 / 10

(* Return value:  *)
(* 0: no danger, 1: danger 2: danger of death *)
let is_in_danger attack_source attack_target biggest_opp_vitality =
  if attack_target < 0 then
    0
  else if attack_source >= 0 then
    let estimated_damage = (estimate_damage (get_opp_vitality attack_source)) in
      begin
	let _, current_vitality = proponent.(attack_target) in
	Printf.eprintf "Checking if %d is in danger- est:%d cur:%d\n%!" attack_target estimated_damage (get_vitality attack_target);
	Printf.eprintf "Checking if %d is in danger- est:%d cur:%d\n%!" attack_target estimated_damage current_vitality;
	if estimated_damage > (get_vitality attack_target) then
	  2
	else
	  1
      end
  else
    let estimated_damage = (estimate_damage biggest_opp_vitality) in
    let _, current_vitality = proponent.(attack_target) in
      begin
	Printf.eprintf "Checking if %d is in danger- est:%d cur:%d\n%!" attack_target estimated_damage (get_vitality attack_target);
	Printf.eprintf "Checking if %d is in danger- est:%d cur:%d\n%!" attack_target estimated_damage current_vitality;
	if estimated_damage > (get_vitality attack_target) then
	  2
	else if estimated_damage * 2 > (get_vitality attack_target) then
	  1
	else
	  0
      end

let rec protect_against_attack_base slots biggest_opp_vitality next_routine =
  match slots with
    | [] -> next_routine ()
    | (attack_source, attack_target)::tl ->
      let danger_type = is_in_danger attack_source attack_target biggest_opp_vitality in
      if danger_type = 2 then
	run_help
	  attack_target
	  (get_vitality attack_target)
      else if danger_type = 1 then
	run_self_help
	  attack_target
	  (get_vitality attack_target)
      else
	protect_against_attack_base tl biggest_opp_vitality next_routine

let protect_against_attack next_routine =
  let opp_attacking_targets = find_opp_attacking_targets 0 255 in
  let _, biggest_opp_vitality = find_opp_slot_with_biggest_vitality 0 255 in
    protect_against_attack_base
      opp_attacking_targets
      biggest_opp_vitality
      next_routine

let heal_in_general next_routine =
  (* There's no onging help *)
  let target_slot, target_vitality = find_slot_with_smallest_vitality 0 255 in
  begin
    Printf.eprintf "1. Smallest vitality slot is %d vit:%d!\n%!" target_slot target_vitality;
    Printf.eprintf "2. Smallest vitality slot is %d vit:%d!\n%!" target_slot (get_vitality target_slot);
    (* let _, vitality = get_prop_slot 1 in *)
    if target_vitality < 1000 then
      begin
	Printf.eprintf "running normal help \n%!";
	run_help
	  target_slot
	  target_vitality
      end
    else
      if target_vitality < 59000 then
	begin
	  Printf.eprintf "running self help \n%!";
	  run_self_help
	    target_slot
	    target_vitality
	end
      else
	next_routine ()
  end

let reuse_existing_help next_routine =
  let helping_source, helping_target = find_helping_slot reg_help_command in
    if helping_source >= 0 && helping_target >= 0 then
      begin
	Printf.eprintf "Found onging help effort from %d to %d\n%!" helping_source helping_target;
      (* If there's ongoing help effort, keep it as is *)
      (* Note that it's assued that both source and target are alive *)
	if helping_source = helping_target then
	    run_self_help
	          helping_source
	          (get_vitality helping_source)
	else if (get_vitality helping_target) < 10000 then
	  run_help_with_source
	        helping_source
	        helping_target
	        (get_vitality helping_source)
	else
	    (* Maybe the help entry was created by false alerm of attack *)
	    (* If the control reaches here, there's no threat of attack anymore *)
	    next_routine ()
      end
    else if helping_source >= 0 && helping_target < 0 then
      (* find one with the smallest vitality *)
      let target_slot, target_vitality = find_slot_with_smallest_vitality 0 255 in
      begin
	Printf.eprintf "Found onging help effort from %d to ANYONE\n%!" helping_source;
	if target_vitality > 1000 || (get_vitality helping_source) < 59000 || target_slot = helping_source then
	    (* If the vitality of the smallest vitality slot is more than 1000 *)
	    (* then we should ignore it and try self_help to get the maximum *)
	    (* benefit *)
	  begin
	    Printf.eprintf "Ignoring %d running self help for %d\n%!" target_vitality helping_source;
	    run_self_help
	      helping_source
	      (get_vitality helping_source)
	  end
	else
	  begin
	    Printf.eprintf "Running normal help by %d\n%!" helping_source;
	    run_help_with_source
	      helping_source
	      target_slot
	      (get_vitality helping_source)
	  end
      end
    else
      next_routine ()

let heal_damaged2 next_routine =
  begin
    Printf.eprintf "heal_damaged2\n%!";
    reuse_existing_help
      (fun _ -> heal_in_general next_routine)
  end

let heal_damaged next_routine =
  let helping_source, helping_target = find_helping_slot reg_help_command in
    if helping_source >= 0 && helping_target >= 0 then
      begin
	Printf.eprintf "Found onging help effort from %d to %d\n%!" helping_source helping_target;
      (* If there's ongoing help effort, keep it as is *)
      (* Note that it's assued that both source and target are alive *)
	if helping_source = helping_target then
	  run_self_help
	    helping_source
	    (get_vitality helping_source)
	else
	  run_help_with_source
	    helping_source
	    helping_target
	    (get_vitality helping_source)
      end
    else if helping_source >= 0 && helping_target < 0 then
      (* find one with the smallest vitality *)
      let target_slot, target_vitality = find_slot_with_smallest_vitality 0 255 in
      begin
	Printf.eprintf "Found onging help effort from %d to %d\n%!" helping_source helping_target;
	if target_vitality > 1000 then
	  run_self_help
	    helping_source
	    (get_vitality helping_source)
	else
	  run_help_with_source
	    helping_source
	    target_slot
	    (get_vitality helping_source)
      end
    else
      let _, vitality = get_prop_slot 1 in
      if vitality < 1000 then begin
	run_help
	  1
	  vitality
      end else
	if vitality < 10000 then
	  run_self_help
	    1
	    vitality
	else
	  next_routine ()

let stupid_dec next_routine =
  let target_slot, _ = find_alive_opp_slot_backward 0 255 in
  let command_slot = find_alive_prop_slot 0 255 in
  let temporary_slot = find_alive_prop_slot (command_slot + 1) 255 in
  if temporary_slot = -1 then
    nop ()
  else
    set_field_to_value
      temporary_slot
      (255 - target_slot)
      (fun _ ->
	copy_field
	  command_slot
	  temporary_slot
	  (fun _ ->
	    lapp "dec" command_slot))

let build_dec_base field target_slot reg_command next_routine =
  apply_function_to_reg1
    field
    (Dec)
    reg_command
    (fun _ ->
      set_field_to_value
	reg1
	(255 - target_slot)
	(fun _ ->
	  next_routine ()))
    (fun _ ->
      set_field_to_card
	reg_command
	"dec"
	(fun _ -> ()))

let smart_dec target_slot next_routine =
  let field, _ = get_prop_slot reg0 in
  wrap_infinite_loop_in_reg0
    field
    (Sfg(KX(Sfg(KX(Dec), Get)), Succ))
    reg0
    next_routine
    (fun _ ->
      build_dec_base
	field
	target_slot
	reg0
	(fun _ ->
	  lapp "S" reg0))

let find_best_opp_target reg_attack_command =
  let attacking_slot = find_attacking_slot reg_attack_command in
  let attacking_vitality = get_opp_vitality(attacking_slot) in
  let weak_slot, weak_vitality = find_alive_opp_slot_with_field_value_lt 50 in
  let weak_slot2, weak_vitality2 = find_alive_opp_slot_with_field_value_lt 9000 in
  let busy_slot, busy_vitality = find_opp_busy_alive in
  let back_slot, back_vitality = find_alive_opp_slot_backward 200 255 in
  if attacking_slot >= 0 && attacking_vitality > 0 then
    attacking_slot, attacking_vitality
  else if weak_slot >= 0 && weak_vitality > 0 then
    weak_slot, weak_vitality
  else if weak_slot2 >= 0 && weak_vitality2 > 0 then
    weak_slot2, weak_vitality2
  else if back_slot >= 0 && back_vitality > 0 then
    back_slot, back_vitality
  else if busy_slot >= 0 && busy_vitality > 0 then
    busy_slot, busy_vitality
  else
    find_alive_opp_slot_backward 0 255

let normal_attack next_routine =
  let target_slot, target_vitality =
    find_best_opp_target reg_attack_command in
  let vitality_needed =
    min (power_ceil(target_vitality * 10 / 9 + 9)) 65535 in
  if vitality_needed < 50 then
    smart_dec
      target_slot
      (fun _ -> rapp reg0 "zero")
  else begin
    let slash_attacker_slot, _ =
      find_slot_with_vitality_ge (vitality_needed + 1) min_soldier_id 255 in
    if slash_attacker_slot = -1 then begin
      if find_dead_opp_slot_forward 0 255 = -1 then begin
	let coop_attacker_slot, coop_attacker_vitality =
	  find_slot_with_biggest_vitality min_soldier_id 255 in
	check_attack_command
	  coop_attacker_slot
	  target_slot
	  reg_attack_command
	  (fun _ ->
	    build_attack
	      coop_attacker_slot
	      target_slot
	      (max 0 (min vitality_needed (min 8192 (coop_attacker_vitality - 100))))
	      reg_attack_command
	      reg_tmp
	      reg_attack_j_backup
	      reg_attack_n_backup)
      end else begin
	let helper_slot, vitality = find_slot_with_biggest_vitality min_soldier_id 255 in
	if helper_slot = -1 then
	  next_routine ()
	else
	  run_self_help
	    helper_slot
	    vitality
      end
    end else begin
      check_attack_command
	slash_attacker_slot
	target_slot
	reg_attack_command
	(fun _ ->
	  build_attack
	    slash_attacker_slot
	    target_slot
	    vitality_needed
	    reg_attack_command
	    reg_tmp
	    reg_attack_j_backup
	    reg_attack_n_backup)
    end
  end
