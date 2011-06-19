open Simulator
open Utility

let reg_help_command = 0
let reg0 = 0
let reg_tmp = 1
let reg1 = 1
let reg2 = 2
let reg_attack_command = 3
let reg_attack_j_backup = 4
let reg_attack_n_backup = 5
let reg6 = 6

let nop () =
  let i = find_alive_prop_slot 0 255 in
  if i = -1 then
    lapp "I" 0
  else
    lapp "I" i

let set_field_to_card slot card next_routine =
  let x = parse_card card in
  let field, _ = get_prop_slot slot in
  if field = x then
    next_routine ()
  else if field = Identity then
    rapp slot card
  else
    lapp "put" slot

let set_field_to_value slot x next_routine =
  let field, _ = get_prop_slot slot in
  match field with
    | Value(value) ->
      if value = x then
	next_routine ()
      else
	if value < x then
	  lapp (choose_succ_dbl value x) slot
	else
	  lapp "put" slot
    | Identity ->
      rapp slot "zero"
    | _ ->
      lapp "put" slot
	
let copy_field dst src next_routine =
  let dst_field, _ = get_prop_slot dst in
  let src_field, _ = get_prop_slot src in
  if dst_field = src_field then (* revisit == or = *)
    next_routine ()
  else
    set_field_to_value
      dst
      src
      (fun _ ->
	lapp "get" dst)

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

let check_field_value field expected_value reg_command next_routine =
  match field with
    | Value(value) ->
      begin
	if value = expected_value then
	  next_routine ()
	else
	  set_field_to_value reg_command 0 next_routine
      end
    | _ ->
      set_field_to_value reg_command 0 next_routine

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
      check_field_value
	i attacker_slot reg_command next_routine
    | AttackIJ(i, j) ->
      check_field_value
	i
	attacker_slot
	reg_command
	(fun _ ->
	  check_field_value
	    j (255 - target_slot) reg_command next_routine)
    | _ ->
      next_routine ()

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
      if 255 - target_slot = 0 then
	rapp reg_command "I"
      else if 255 - target_slot = 1 then
	rapp reg_command "succ"
      else
	rapp reg_command "get"
    | Sfg(KX(AttackI(_)), Identity) ->
      rapp reg_command "zero"
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
	  copy_field
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
      	  copy_field
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
      check_field_value
	i helper_slot reg_command next_routine
    | HelpIJ(i, j) ->
      check_field_value
	i
	helper_slot
	reg_command
	(fun _ ->
	  check_field_value
	    j target_slot reg_command next_routine)
    | _ ->
      next_routine ()

let apply_function_to_opp_reg0 field x reg_command next_routine on_mismatch =
  if field = Sf(KX(Sfg(KX(Sfg(KX(x), Copy)), Identity))) then
    next_routine ()
  else if field = KX(Sfg(KX(Sfg(KX(x), Copy)), Identity)) then
    lapp "S" reg_command
  else if field = Sfg(KX(Sfg(KX(x), Copy)), Identity) then
    lapp "K" reg_command
  else if field = Sf(KX(Sfg(KX(x), Copy))) then
    rapp reg_command "I"
  else if field = KX(Sfg(KX(x), Copy)) then
    lapp "S" reg_command
  else if field = Sfg(KX(x), Copy) then
    lapp "K" reg_command
  else if field = Sf(KX(x)) then
    rapp reg_command "copy"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_opp_reg1 field x reg_command next_routine on_mismatch =
  if field = Sf(KX(Sfg(KX(Sfg(KX(x), Copy)), Succ))) then
    next_routine ()
  else if field = KX(Sfg(KX(Sfg(KX(x), Copy)), Succ)) then
    lapp "S" reg_command
  else if field = Sfg(KX(Sfg(KX(x), Copy)), Succ) then
    lapp "K" reg_command
  else if field = Sf(KX(Sfg(KX(x), Copy))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(x), Copy)) then
    lapp "S" reg_command
  else if field = Sfg(KX(x), Copy) then
    lapp "K" reg_command
  else if field = Sf(KX(x)) then
    rapp reg_command "copy"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_reg0 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(x), Get) then
    next_routine ()
  else if field = Sf(KX(x)) then
    rapp reg_command "get"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_reg1 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(Sfg(KX(x), Get)), Succ) then
    next_routine ()
  else if field = Sf(KX(Sfg(KX(x), Get))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(x), Get)) then
    lapp "S" reg_command
  else if field = Sfg(KX(x), Get) then
    lapp "K" reg_command
  else if field = Sf(KX(x)) then
    rapp reg_command "get"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_reg2 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ) then
    next_routine ()
  else if field = Sf(KX(Sfg(KX(Sfg(KX(x), Get)), Succ))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(Sfg(KX(x), Get)), Succ)) then
    lapp "S" reg_command
  else if field = Sfg(KX(Sfg(KX(x), Get)), Succ) then
    lapp "K" reg_command
  else if field = Sf(KX(Sfg(KX(x), Get))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(x), Get)) then
    lapp "S" reg_command
  else if field = Sfg(KX(x), Get) then
    lapp "K" reg_command
  else if field = Sf(KX(x)) then
    rapp reg_command "get"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_reg3 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ)), Succ) then
    next_routine ()
  else if field = Sf(KX(Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ)) then
    lapp "S" reg_command
  else if field = Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ) then
    lapp "K" reg_command
  else if field = Sf(KX(Sfg(KX(Sfg(KX(x), Get)), Succ))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(Sfg(KX(x), Get)), Succ)) then
    lapp "S" reg_command
  else if field = Sfg(KX(Sfg(KX(x), Get)), Succ) then
    lapp "K" reg_command
  else if field = Sf(KX(Sfg(KX(x), Get))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(x), Get)) then
    lapp "S" reg_command
  else if field = Sfg(KX(x), Get) then
    lapp "K" reg_command
  else if field = Sf(KX(x)) then
    rapp reg_command "get"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_0 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(x), Identity) then
    next_routine ()
  else if field = Sf(KX(x)) then
    rapp reg_command "I"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_1 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(x), Succ) then
    next_routine ()
  else if field = Sf(KX(x)) then
    rapp reg_command "succ"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let wrap_infinite_loop_in_reg0 field x reg_command next_routine on_mismatch =
  (* Matching order is important *)
  if field = Sfg(Sfg(x, Get), Identity) then
    next_routine ()
  else if field = Sf(Sfg(x, Get)) then
    rapp reg_command "I"
  else if field = Sfg(x, Get) then
    lapp "S" reg_command
  else if field = Sf(x) then
    rapp reg_command "get"
  else
    on_mismatch ()

let build_help_i helper_slot reg_command =
  (* Prepare i *)
  set_field_to_value
    reg_command
    helper_slot
    (fun _ -> lapp "help" reg_command)

let build_help_ij field helper_slot target_slot reg_command =
  if target_slot = 0 then
    apply_function_to_0
      field
      (HelpI(Value(helper_slot)))
      reg_command
      (fun _ ->	rapp reg_command "zero")
      (fun _ ->	build_help_i helper_slot reg_command)
  else if target_slot = 1 then
    apply_function_to_1
      field
      (HelpI(Value(helper_slot)))
      reg_command
      (fun _ -> rapp reg_command "zero")
      (fun _ ->	build_help_i helper_slot reg_command)
  else
    apply_function_to_reg1
      field
      (HelpI(Value(helper_slot)))
      reg_command
      (fun _ ->
        (* Prepare j *)
	set_field_to_value
	  reg1
	  target_slot
	  (fun _ -> rapp reg_command "zero"))
      (fun _ ->	build_help_i helper_slot reg_command)

let build_help_base field helper_slot target_slot amount reg_command next_routine =
  apply_function_to_reg1
    field
    (HelpIJ(Value(helper_slot), Value(target_slot)))
    reg_command
    (fun _ ->
      (* Prepare n *)
      set_field_to_value
      	reg1
      	amount
        (fun _ -> next_routine ()))
    (fun _ ->
      build_help_ij field helper_slot target_slot reg_command)

let build_help helper_slot target_slot amount reg_command infinite next_routine =
  let field, _ = get_prop_slot reg_command in
  check_help_command
    field
    helper_slot
    target_slot
    reg_command
    (fun _ ->
      if infinite then
	wrap_infinite_loop_in_reg0
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

(* Command to be injected into an opponent slot *)
(* Takes anything. Forces the opponent slot suicide *)
let rec build_help_bomb slot vitality next_routine =
  let field, _ = get_prop_slot slot in
  if field =
    Sfg(KX(Sfg(KX(Sfg(KX(
      Sfg(KX(Sfg(KX(Sfg(KX(
	Sfg(KX(Sfg(KX(Sfg(KX(
	  Help
	), Copy)), Identity)), KX(Value(0)))
      ), Copy)), Identity)), KX(Value(0)))
    ), Copy)), Succ)), KX(Value(0))) then
    next_routine ()
  else begin
    let field2, _ = get_prop_slot reg2 in
    if field2 = KX(Value(0)) then
      apply_function_to_reg2
	field
	(Sf(KX(Sfg(KX(Sfg(KX(
	  Sfg(KX(Sfg(KX(Sfg(KX(
	    Sfg(KX(Sfg(KX(Sfg(KX(
	      Help
	    ), Copy)), Identity)), KX(Value(0)))
	  ), Copy)), Identity)), KX(Value(0)))
	 ), Copy)), Succ))))
	slot
	(fun _ -> rapp slot "zero")
	(fun _ ->
	  apply_function_to_opp_reg1
	    field
	    (Sfg(KX(Sfg(KX(Sfg(KX(
	      Sfg(KX(Sfg(KX(Sfg(KX(
		Help
	      ), Copy)), Identity)), KX(Value(0)))
	     ), Copy)), Identity)), KX(Value(0))))
	    slot
	    (fun _ -> rapp slot "zero")
	    (fun _ ->
	      apply_function_to_reg2
		field
		(Sf(KX(Sfg(KX(Sfg(KX(
		  Sfg(KX(Sfg(KX(Sfg(KX(
		    Help
		  ), Copy)), Identity)), KX(Value(0)))
		 ), Copy)), Identity))))
		slot
		(fun _ -> rapp slot "zero")
		(fun _ ->
		  apply_function_to_opp_reg0
		    field
		    (Sfg(KX(Sfg(KX(Sfg(KX(
		      Help
		     ), Copy)), Identity)), KX(Value(0))))
		    slot
		    (fun _ -> rapp slot "zero")
		    (fun _ ->
		      apply_function_to_reg2
			field
			(Sf(KX(Sfg(KX(Sfg(KX(
			  Help
			 ), Copy)), Identity))))
			slot
			(fun _ -> rapp slot "zero")
			(fun _ ->
			  apply_function_to_opp_reg0
			    field
			    (Help)
			    slot
			    (fun _ -> rapp slot "zero")
			    (fun _ -> set_field_to_card slot "help" (fun _ -> ())))))))
    else
      set_field_to_value
	reg2
	0
	(fun _ -> lapp "K" reg2)
  end

let build_zombie_i slot dead_slot =
  set_field_to_value
    slot
    (255 - dead_slot)
    (fun _ -> lapp "zombie" reg_attack_j_backup)

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
      		(255 - 0)
		first
	        (fun _ ->
		  (* Prepare n *)
		  set_field_to_value
      		    (255 - 1)
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
