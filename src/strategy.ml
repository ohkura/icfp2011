open Simulator
open Utility

let reg_help_command = 2
let reg_attack_command = 3
let reg_tmp = 1
let reg_attack_j_backup = 4
let reg_attack_n_backup = 5
let reg_dec_i_backup = 6

let nop () =
  let i = find_alive_prop_slot 0 255 in
  if i = -1 then
    lapp "I" 0
  else
    lapp "I" i

let set_field_to_zero slot next_routine =
  let field, vitality = get_prop_slot slot in
  match field with
    | Value(value) ->
      if value = 0 then
	next_routine slot
      else
	lapp "put" slot
    | Identity ->
      rapp slot "zero"
    | _ ->
      lapp "put" slot

let rec set_field_to_value slot target next_routine =
  let rep slot =
    set_field_to_value slot target next_routine in
  let field, vitality = get_prop_slot slot in
  match field with
    | Value(value) ->
      if value = target then
	next_routine slot
      else
	if value < target then
	  lapp (choose_succ_dbl value target) slot
	else
	  set_field_to_zero slot rep
    | _ ->
      set_field_to_zero slot rep
	
let rec copy_value dst src next_routine =
  let rep _ =
    copy_value dst src next_routine in
  let dst_field, _ =
    get_prop_slot dst in
  let src_field, _ =
    get_prop_slot src in
  if dst_field = src_field then (* revisit == or = *)
    next_routine dst
  else
    match dst_field with
      | Value(value) ->
	if value = src then
	  lapp "get" dst
	else
	  set_field_to_value dst src rep
      | _ ->
	set_field_to_value dst src rep

let revive_dead next_routine =
  let dead_slot = find_dead_prop_slot 0 255 in
  if dead_slot != -1 then begin
    let reviver = find_alive_prop_slot 8 255 in
    if reviver != -1 then begin
      set_field_to_value
	reviver
	dead_slot
	(fun r -> lapp "revive" r)
    end else begin
      nop ()
    end
  end else
    next_routine ()

let zombienize next_routine =
  let field, _ = get_prop_slot reg_attack_j_backup in
  match field with
    | Value(value) ->
      let target = validate_slot_number (Value(value)) in
      if target != -1 then
	let field, vitality = get_opp_slot (255 - target) in
	if vitality <= 0 && field != Identity then
	  lapp "zombie" reg_attack_j_backup
	else
	  next_routine ()
      else
	next_routine ()
    | ZombieI(i) ->
      let target = validate_slot_number i in
      if target != -1 then
	let _, vitality = get_opp_slot (255 - target) in
	if vitality <= 0 then
	  rapp reg_attack_j_backup "put"
	else
	  lapp "put" reg_attack_j_backup
      else
	lapp "put" reg_attack_j_backup
    | _ ->
      next_routine ()

let check_value field expected reg_command next_routine =
  match field with
    | Value(value) ->
      begin
	if value = expected then
	  next_routine reg_command
	else
	  set_field_to_zero reg_command next_routine
      end
    | _ ->
      set_field_to_zero reg_command next_routine

let rec check_attack_command_field field attacker_slot target_slot reg_command next_routine =
  match field with
    | KX(y) ->
      check_attack_command_field
    	y attacker_slot target_slot reg_command next_routine
    | Sf(g) ->
      check_attack_command_field
    	g attacker_slot target_slot reg_command next_routine
    | Sfg(f, g) ->
      check_attack_command_field
    	f attacker_slot target_slot reg_command next_routine
    | AttackI(i) ->
      check_value
	i attacker_slot reg_command next_routine
    | AttackIJ(i, j) ->
      check_value
	i
	attacker_slot
	reg_command
	(fun _ ->
	  check_value
	    j (255 - target_slot) reg_command next_routine)
    | _ ->
      next_routine reg_command

let check_attack_command attacker_slot target_slot reg_command next_routine =
  let field, _ = get_prop_slot reg_command in
  check_attack_command_field
    field attacker_slot target_slot reg_command next_routine

let build_attack attacker_slot target_slot amount reg_command reg_tmp reg_j_backup reg_n_backup =
  let field, _ = get_prop_slot reg_command in
  match field with
    | KX(_) ->
      lapp "S" reg_command
    | Sf(KX(Sfg(_, _))) ->
      rapp reg_command "succ"
    | AttackI(_) ->
      lapp "K" reg_command
    | Sf(KX(AttackI(_))) ->
      if 255 - target_slot = 1 then
	rapp reg_command "succ"
      else
	rapp reg_command "get"
    | Sfg(KX(AttackI(_)), Succ) ->
      rapp reg_command "zero"
    | Sfg(KX(AttackI(_)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(AttackI(_)), Get)), Succ) ->
      (* Prepare j *)
      set_field_to_value
	reg_j_backup
	(255 - target_slot)
	(fun _ ->
	  copy_value
	    reg_tmp
	    reg_j_backup
            (* Apply the value in slot 1 as argument j *)
	    (fun _ -> rapp reg_command "zero"))
    | AttackIJ(_, _) ->
      lapp "K" reg_command
    | Sf(KX(AttackIJ(_, _))) ->
      rapp reg_command "get"
    | Sfg(KX(AttackIJ(_, _)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(AttackIJ(_, _)), Get)), Succ) ->
      (* Prepare n *)
      set_field_to_value
      	reg_n_backup
      	amount
      	(fun _ ->
      	  copy_value
      	    reg_tmp
      	    reg_n_backup
      	    (* Apply the value in slot 1 as argument n *)
      	    (fun _ -> rapp reg_command "zero"))
    | _ ->
      (* Prepare i *)
      set_field_to_value
	reg_command
	attacker_slot
	(fun _ -> lapp "attack" reg_command)

let rec check_help_command_field field helper_slot target_slot reg_command next_routine =
  match field with
    | KX(y) ->
      check_help_command_field
	y helper_slot target_slot reg_command next_routine
    | Sf(g) ->
      check_help_command_field
	g helper_slot target_slot reg_command next_routine
    | Sfg(f, g) ->
      check_help_command_field
	f helper_slot target_slot reg_command next_routine
    | HelpI(i) ->
      check_value
	i helper_slot reg_command next_routine
    | HelpIJ(i, j) ->
      check_value
	i
	helper_slot
	reg_command
	(fun _ ->
	    check_value
	          j target_slot reg_command next_routine)
    | _ ->
      next_routine reg_command

let check_help_command helper_slot target_slot reg_command next_routine =
  let field, vitality = get_prop_slot reg_command in
  check_help_command_field
    field helper_slot target_slot reg_command next_routine

let build_help helper_slot target_slot amount reg_command reg_tmp =
  let field, vitality = get_prop_slot reg_command in
  match field with
    | KX(_) ->
      lapp "S" reg_command
    | Sf(KX(Sfg(_, _))) ->
      rapp reg_command "succ"
    | HelpI(_) ->
      lapp "K" reg_command
    | Sf(KX(HelpI(_))) ->
      if target_slot = 1 then
      	rapp reg_command "succ"
      else
	rapp reg_command "get"
    | Sfg(KX(HelpI(_)), Succ) ->
      rapp reg_command "zero"
    | Sfg(KX(HelpI(_)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(HelpI(_)), Get)), Succ) ->
      (* Prepare j *)
      set_field_to_value
	reg_tmp
	target_slot
        (* Apply the value in slot 1 as argument j *)
	(fun _ -> rapp reg_command "zero")
    | HelpIJ(_, _) ->
      lapp "K" reg_command
    | Sf(KX(HelpIJ(_, _))) ->
      rapp reg_command "get"
    | Sfg(KX(HelpIJ(_, _)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(HelpIJ(_, _)), Get)), Succ) ->
      (* Prepare n *)
      set_field_to_value
      	reg_tmp
      	amount
        (* Apply the value in slot 1 as argument n *)
	(fun _ -> rapp reg_command "zero")
    | _ ->
      (* Prepare i *)
      set_field_to_value
	reg_command
	helper_slot
	(fun _ -> lapp "help" reg_command)

let heal_damaged next_routine =
  let _, vitality = get_prop_slot 1 in
  if vitality < 10000 then begin
    let helper_slot, helper_vitality = find_slot_with_vitality_ge 10000 6 255 in
    let helper_slot, helper_vitality =
      if helper_slot != -1 then helper_slot, helper_vitality
      else 1, vitality in
    check_help_command
      helper_slot
      1
      reg_help_command
      (fun _ -> 
	build_help
	  helper_slot
	  1
	  (helper_vitality - 1)
	  reg_help_command
	  reg_tmp)
  end else
    next_routine ()

let stupid_dec next_routine =
  let target_slot, _ = find_alive_opp_slot_backward 0 255 in
  let arg0 = 255 - target_slot in
  set_field_to_value
    reg_dec_i_backup
    arg0
    (fun _ ->
      copy_value
	reg_attack_command
	reg_dec_i_backup
	(fun r -> lapp "dec" r))

let normal_attack next_routine =
  let target_slot, target_vitality =
    find_best_opp_target reg_attack_command in
  let damage_needed =
    target_vitality * 10 / 9 + 9 in
  let attacker_slot, _ =
    find_slot_with_vitality_ge (damage_needed + 1) 8 255 in
  if attacker_slot = -1 then begin
    let helper_slot, vitality = find_slot_with_biggest_vitality 8 255 in
    if helper_slot = -1 then
	  (* let target_slot, _ = find_alive_opp_slot_backward 0 255 in *)
	  (* let arg0 = 255 - target_slot in *)
	  (* set_field_to_value *)
	  (*   reg_tmp *)
	  (*   arg0 *)
	  (*   (fun _ -> *)
	  (*     copy_value *)
	  (* 	reg_command *)
	  (* 	reg_tmp *)
	  (* 	(fun r -> lapp "dec" r)); *)
      next_routine ()
    else
      check_help_command
	helper_slot
	helper_slot
	reg_help_command
	(fun _ ->
	  build_help
	    helper_slot
	    helper_slot
	    (vitality - 1)
	    reg_help_command
	    reg_tmp)
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
