open Simulator
open Utility
open Command
open Commandutil
open Functionutil

let revive_any_dead next_routine =
  let dead_slot = find_dead_prop_slot 0 255 in
  if dead_slot != -1 then begin
    let reviver_slot = find_alive_prop_slot 8 255 in
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


let zombienize next_routine =
  (* let dead_slot = find_dead_non_identity_opp_slot_forward 0 255 in *)
  let dead_slot = find_dead_opp_slot_forward 0 255 in
  if dead_slot = -1 then
    next_routine ()
  else begin
    let first, vitality = find_opp_slot_with_biggest_vitality 0 255 in
    let second, _ = find_alive_opp_slot_forward_ex first 0 255 in
    if second = -1 || vitality < 9000 then
      next_routine ()
    else begin
      let field, _ = get_prop_slot reg_attack_j_backup in
      apply_function_to_reg3
	field
	(ZombieI(Value(255 - dead_slot)))
	reg_attack_j_backup
	(fun _ ->
	  copy_field
	    reg_attack_n_backup
	    reg_attack_j_backup
	    (fun _ ->
	      build_help_bomb
		reg_attack_command
		vitality
		(fun _ ->
	      (* Prepare i *)
		  set_field_to_value
      		    reg2
		    first
	            (fun _ ->
	          (* Prepare j *)
		      set_field_to_value
      			reg1
			second
			(fun _ ->
                      (* Prepare n *)
			  set_field_to_value
      			    reg0
      			    vitality
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
    (fun _ ->	rapp reg_help_command "zero")

let run_help target target_vitality =
  let helper_slot, helper_vitality =
    find_slot_with_vitality_ge 10000 6 255 in
    if helper_slot != -1 then
      build_help
	helper_slot
	target
	(lower_power (helper_vitality - 1))
	reg_help_command
	(helper_slot = target)
	(fun _ ->	rapp reg_help_command "zero")
    else
      run_self_help target target_vitality

let estimate_damage opp_vitality =
  (opp_vitality - 1) * 9 / 10

let is_in_danger attack_source attack_target biggest_opp_vitality =
  if attack_target < 0 then
    false
  else if attack_source >= 0 then
    let estimated_damage = (estimate_damage (get_opp_vitality attack_source)) in
      begin
	let _, current_vitality = proponent.(attack_target) in
	Printf.eprintf "Checking if %d is in danger- est:%d cur:%d\n%!" attack_target estimated_damage (get_vitality attack_target);
	Printf.eprintf "Checking if %d is in danger- est:%d cur:%d\n%!" attack_target estimated_damage current_vitality;
	estimated_damage > (get_vitality attack_target)
      end
  else
    let estimated_damage = (estimate_damage biggest_opp_vitality) in
    let _, current_vitality = proponent.(attack_target) in
      begin
	Printf.eprintf "Checking if %d is in danger- est:%d cur:%d\n%!" attack_target estimated_damage (get_vitality attack_target);
	Printf.eprintf "Checking if %d is in danger- est:%d cur:%d\n%!" attack_target estimated_damage current_vitality;
	estimated_damage > (get_vitality attack_target)
      end

let rec protect_against_attack_base slots biggest_opp_vitality next_routine =
  match slots with
    | [] -> next_routine ()
    | (attack_source, attack_target)::tl ->
	if is_in_danger attack_source attack_target biggest_opp_vitality then
	  run_help
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
  let busy_slot, busy_vitality = find_opp_busy_alive in
  if attacking_slot >= 0 && attacking_vitality > 0 then
    attacking_slot, attacking_vitality
  else if weak_slot >= 0 then
    weak_slot, weak_vitality
  else if busy_slot >= 0 then
    busy_slot, busy_vitality
  else
    find_alive_opp_slot_forward 0 255

let normal_attack next_routine =
  let target_slot, target_vitality =
    find_best_opp_target reg_attack_command in
  let damage_needed =
    min (power_ceil(target_vitality * 10 / 9 + 9)) 65535 in
  if damage_needed < 50 then
    smart_dec
      target_slot
      (fun _ -> rapp reg0 "zero")
  else begin
    let attacker_slot, _ =
      find_slot_with_vitality_ge (damage_needed + 1) 8 255 in
    if attacker_slot = -1 then begin
      let helper_slot, vitality = find_slot_with_biggest_vitality 8 255 in
      if helper_slot = -1 then
	next_routine ()
      else
	run_self_help
	  helper_slot
	  vitality
    end else begin
      check_attack_command
	attacker_slot
	target_slot
	reg_attack_command
	(fun _ ->
	  build_attack
	    attacker_slot
	    target_slot
	    damage_needed
	    reg_attack_command
	    reg_tmp
	    reg_attack_j_backup
	    reg_attack_n_backup);
  (* let attacker, _ = find_slot_with_vitality_ge 8200 20 255 in *)
  (* build_attack *)
  (*   attacker_slot *)
  (*   (find_alive_opp_slot_forward 0) *)
  (*   8192 *)
  (*   reg_command *)
  (*   reg_tmp; *)
    end
  end
