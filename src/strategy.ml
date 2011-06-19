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
    (* match field with *)
    (*   | ZombieI(Value(value)) -> *)
    (* 	if value = 255 - dead_slot then *)
    (* 	  rapp reg_attack_j_backup "zero" *)
    (* 	else *)
    (* 	  build_zombie_i dead_slot *)
    (*   | _ -> *)
    (* 	build_zombie_i dead_slot *)
      apply_function_to_reg3
	field
	(ZombieI(Value(255 - dead_slot)))
	reg_attack_j_backup
	(fun _ ->
	  build_help_bomb
	    reg_attack_command
	    vitality
	    (fun _ ->
	      (* Prepare i and j *)
	      set_field_to_value
      		reg1
		first
	        (fun _ ->
		  (* Prepare n *)
		  set_field_to_value
      		    reg0
      		    vitality
		    (fun _ -> rapp reg_attack_j_backup "zero"))))
	(fun _ ->
	  build_zombie_i reg_attack_j_backup dead_slot)
    end
  end

let heal_damaged next_routine =
  let _, vitality = get_prop_slot 1 in
  if vitality < 1000 then begin
    let helper_slot, helper_vitality =
      find_slot_with_vitality_ge 10000 6 255 in
    let helper_slot, helper_vitality =
      if helper_slot != -1 then helper_slot, helper_vitality
      else 1, vitality in
    build_help
      helper_slot
      1
      (helper_vitality - 1)
      reg_help_command
      (helper_slot = 1)
      (fun _ ->	rapp reg_help_command "zero")
  end else
    if vitality < 10000 then
      build_help
	1
	1
	(vitality - 1)
	reg_help_command
	true
	(fun _ -> rapp reg_help_command "zero")
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
	build_help
	  helper_slot
	  helper_slot
	  (vitality - 1)
	  reg_help_command
	  true
	  (fun _ -> rapp reg_help_command "zero")
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
