open Simulator
open Utility

let reg_help_command = 0
let reg0 = 0
let reg_help_bomb = 2
let reg_attack_command = 3
let reg_tmp = 1
let reg1 = 1
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
      (* Try again, using important registers are not good, but much better than do nothing *)
      let reviver = find_alive_prop_slot 0 255 in
      if reviver != -1 then begin
	set_field_to_value
	  reviver
	  dead_slot
	  (fun r -> lapp "revive" r)
      end
      else
	nop ()
    end
  end else
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

let rec check_help_command field helper_slot target_slot reg_command next_routine =
  match field with
    | KX(y) ->
      check_help_command
	y helper_slot target_slot reg_command next_routine
    | Sf(g) ->
      check_help_command
	g helper_slot target_slot reg_command next_routine
    | Sfg(f, g) ->
      check_help_command
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

let apply_function_with_opp_reg1 field contents reg_command next_routine on_mismatch =
  match field with
    | Sfg(KX(Sfg(KX(x), Copy)), Succ) ->
      if x = contents then
	next_routine ()
      else
	on_mismatch ()
    | Sf(KX(Sfg(KX(x), Copy))) ->
      if x = contents then
	rapp reg_command "succ"
      else
	on_mismatch ()
    | KX(Sfg(KX(x), Copy)) ->
      if x = contents then
	lapp "S" reg_command
      else
	on_mismatch ()
    | Sfg(KX(x), Copy) ->
      if x = contents then
	lapp "K" reg_command
      else
	on_mismatch ()
    | Sf(KX(x)) ->
      if x = contents then
	rapp reg_command "copy"
      else
	on_mismatch ()
    | KX(x) ->
      if x = contents then
	lapp "S" reg_command
      else
	on_mismatch ()
    | x ->
      if x = contents then
	lapp "K" reg_command
      else
	on_mismatch ()

let apply_function_with_reg0 field contents reg_command next_routine on_mismatch =
  match field with
    | Sfg(KX(x), Get) ->
      if x = contents then
	next_routine ()
      else
	on_mismatch ()
    | Sf(KX(x)) ->
      if x = contents then
	rapp reg_command "get"
      else
	on_mismatch ()
    | KX(x) ->
      if x = contents then
	lapp "S" reg_command
      else
	on_mismatch ()
    | x ->
      if x = contents then
	lapp "K" reg_command
      else
	on_mismatch ()

let apply_function_with_reg1 field contents reg_command next_routine on_mismatch =
  match field with
    | Sfg(KX(Sfg(KX(x), Get)), Succ) ->
      if x = contents then
	next_routine ()
      else
	on_mismatch ()
    | Sf(KX(Sfg(KX(x), Get))) ->
      if x = contents then
	rapp reg_command "succ"
      else
	on_mismatch ()
    | KX(Sfg(KX(x), Get)) ->
      if x = contents then
	lapp "S" reg_command
      else
	on_mismatch ()
    | Sfg(KX(x), Get) ->
      if x = contents then
	lapp "K" reg_command
      else
	on_mismatch ()
    | Sf(KX(x)) ->
      if x = contents then
	rapp reg_command "get"
      else
	on_mismatch ()
    | KX(x) ->
      if x = contents then
	lapp "S" reg_command
      else
	on_mismatch ()
    | x ->
      if x = contents then
	lapp "K" reg_command
      else
	on_mismatch ()

let wrap_infinite_loop field contents reg_command next_routine on_mismatch =
  (* Matching order is important *)
  match field with
    | Sfg(Sfg(f, Get), Identity) ->
      if f = contents then
	next_routine ()
      else
	on_mismatch ()
    | Sf(Sfg(f, Get)) ->
      if f = contents then
	rapp reg_command "I"
      else
	on_mismatch ()
    | Sfg(f, Get) ->
      if f = contents then
	lapp "S" reg_command
      else
	on_mismatch ()
    | Sf(f) ->
      if f = contents then
	rapp reg_command "get"
      else
	on_mismatch ()
    | _ ->
      on_mismatch ()

let build_help_i helper_slot reg_command =
  (* Prepare i *)
  set_field_to_value
    reg_command
    helper_slot
    (fun _ ->
      lapp "help" reg_command)

let build_help_ij field helper_slot target_slot reg_command =
  apply_function_with_reg1
    field
    (HelpI(Value(helper_slot)))
    reg_command
    (fun _ ->
      (* Prepare j *)
      set_field_to_value
	reg1
	target_slot
        (* Apply the value in slot 1 as argument *)
	(fun _ -> rapp reg_command "zero"))
    (fun _ ->
      build_help_i helper_slot reg_command)

let build_help_base field helper_slot target_slot amount reg_command next_routine =
  apply_function_with_reg1
    field
    (HelpIJ(Value(helper_slot), Value(target_slot)))
    reg_command
    (fun _ ->
      (* Prepare n *)
      set_field_to_value
      	reg1
      	amount
        (fun _ ->
	  next_routine ()))
    (fun _ ->
      build_help_ij field helper_slot target_slot reg_command)

let build_help helper_slot target_slot amount reg_command infinite next_routine =
  let field, vitality = get_prop_slot reg_command in
  check_help_command
    field
    helper_slot
    target_slot
    reg_command
    (fun _ ->
      if infinite then
	wrap_infinite_loop
	  field
	  (Sfg(KX(Sfg(KX(HelpIJ(Value(helper_slot), Value(target_slot))), Get)), Succ))
	  reg_command
	  next_routine
	  (fun _ ->
	    build_help_base
	      field
	      helper_slot
	      target_slot
	      amount
	      reg_command
	      (fun _ -> lapp "S" reg_command))
      else
	build_help_base
	  field
	  helper_slot
	  target_slot
	  amount
	  reg_command
	  next_routine)
	
let build_help_bomb_base field helper_slot target_slot amount reg_command next_routine =
  apply_function_with_opp_reg1
    field
    (HelpIJ(Value(helper_slot), Value(target_slot)))
    reg_command
    (fun _ ->
      (* Prepare n *)
      set_field_to_value
      	reg1
      	amount
        (fun _ ->
	  next_routine ()))
    (fun _ ->
      build_help_ij field helper_slot target_slot reg_command)

let build_help_bomb field slot next_routine =
  let first, vitality = find_alive_opp_slot_forward 0 255 in
  let second, _ = find_alive_opp_slot_forward (first + 1) 255 in
  if second = -1 then
    next_routine ()
  else
    build_help_bomb_base
      field
      first
      second
      vitality
      slot
      next_routine

let zombienize next_routine =
  let dead_slot = find_dead_non_identity_opp_slot_forward 0 255 in
  if dead_slot = -1 then
    next_routine ()
  else
    let field, _ = get_prop_slot reg_attack_j_backup in
    apply_function_with_reg0
      field
      (ZombieI(Value(dead_slot)))
      reg_attack_j_backup
      (fun _ ->
	build_help_bomb
	  field
	  reg0
	  (fun _ ->
	    rapp reg_attack_j_backup "zero"))
      (fun _ ->
	set_field_to_value
	  reg_attack_j_backup
	  dead_slot
	  (fun _ ->
	    lapp "zombie" reg_attack_j_backup))

let heal_damaged next_routine =
  let _, vitality = get_prop_slot 1 in
  if vitality < 10000 then begin
    let helper_slot, helper_vitality = find_slot_with_vitality_ge 10000 6 255 in
    let helper_slot, helper_vitality =
      if helper_slot != -1 then helper_slot, helper_vitality
      else 1, vitality in
    build_help
      helper_slot
      1
      (helper_vitality - 1)
      reg_help_command
      true
      (fun _ -> rapp reg_help_command "zero")
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

let power_ceil x =
  let rec f x current =
    if x = 0 then current
    else f (x / 2) (current * 2) in
  f x 1

let normal_attack next_routine =
  let target_slot, target_vitality =
    find_best_opp_target reg_attack_command in
  let damage_needed =
    min (power_ceil(target_vitality * 10 / 9 + 9)) 65535 in
  let attacker_slot, _ =
    find_slot_with_vitality_ge (damage_needed + 1) 8 255 in
    (*find_slot_with_vitality_ge 60000 8 255 in*)
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
