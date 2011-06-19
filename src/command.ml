open Simulator
open Commandutil
open Utility
open Functionutil

let nop () =
  let i = find_alive_prop_slot 0 255 in
  if i = -1 then
    lapp "I" 0
  else
    lapp "I" i

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


let build_help_i helper_slot reg_command =
  (* Prepare i *)
  set_field_to_value
    reg_command
    helper_slot
    (fun _ -> lapp "help" reg_command)

let build_help_ij field helper_slot target_slot reg_command =
  (* Comment out temporarily as it seems it's not working *)
  (* if target_slot = 0 then *)
  (*   apply_function_to_0 *)
  (*     field *)
  (*     (HelpI(Value(helper_slot))) *)
  (*     reg_command *)
  (*     (fun _ ->	rapp reg_command "zero") *)
  (*     (fun _ ->	build_help_i helper_slot reg_command) *)
  (* else if target_slot = 1 then *)
  (*   apply_function_to_1 *)
  (*     field *)
  (*     (HelpI(Value(helper_slot))) *)
  (*     reg_command *)
  (*     (fun _ -> rapp reg_command "zero") *)
  (*     (fun _ ->	build_help_i helper_slot reg_command) *)
  (* else *)
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
let build_help_bomb slot vitality next_routine =
  let field, _ = get_prop_slot slot in
  if field =
    Sfg(KX(
      Sfg(KX(Sfg(
	Sfg(KX(Sfg(
	  Sfg(KX(Sfg(
	    KX(Help)
	  , Copy)), Succ)
	, Copy)), Succ)
    , Copy)), Identity)), KX(Value(0))) then
    next_routine ()
  else begin
    let field2, _ = get_prop_slot reg2 in
    if field2 = KX(Value(0)) then
      apply_function_to_reg2
	field
	(
	  Sf(KX(
	    Sfg(KX(Sfg(
	      Sfg(KX(Sfg(
		Sfg(KX(Sfg(
		  KX(Help)
		, Copy)), Succ)
	      , Copy)), Succ)
	    , Copy)), Identity)))
	)
	slot
	(fun _ -> rapp slot "zero")
	(fun _ ->
	  if field =
	    KX(
	      Sfg(KX(Sfg(
		Sfg(KX(Sfg(
		  Sfg(KX(Sfg(
		    KX(Help)
		  , Copy)), Succ)
		, Copy)), Succ)
	      , Copy)), Identity)
	    )
	  then
	    lapp "S" slot
	  else begin
	    if field =
	      Sfg(KX(Sfg(
		Sfg(KX(Sfg(
		  Sfg(KX(Sfg(
		    KX(Help)
		  , Copy)), Succ)
		, Copy)), Succ)
	      , Copy)), Identity)
	    then
	      lapp "K" slot
	    else
	      apply_function_to_opp_reg_same
		field
		(
		  Sfg(KX(Sfg(
		    Sfg(KX(Sfg(
		      KX(Help)
		    , Copy)), Succ)
		  , Copy)), Succ)
		)
		slot
		(fun _ -> ())
		(fun _ ->
		  apply_function_to_opp_reg_next
		    field
		    (
		      Sfg(KX(Sfg(
			KX(Help)
		      , Copy)), Succ)
		    )
		    slot
		    (fun _ -> ())
		    (fun _ ->
		      apply_function_to_opp_reg_next
			field
			(KX(Help))
			slot
			(fun _ -> ())
			(fun _ ->
			  if field = Help then
			    lapp "K" slot
			  else
			    set_field_to_card slot "help" (fun _ -> ()))))
	  end
	)
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
