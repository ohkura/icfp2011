open Simulator

let check_value field expect clear_slot next_routine =
  match field with
    | Value(value) ->
      begin
	if value = expect then
	  next_routine clear_slot
	else
	  set_field_to_zero clear_slot next_routine
      end
    | _ ->
      set_field_to_zero clear_slot next_routine

let rec check_attacker_field field attacker_slot target_slot reg_command next_routine =
  match field with
    | KX(y) ->
      check_attacker_field
    	y attacker_slot target_slot reg_command next_routine
    | Sf(g) ->
      check_attacker_field
    	g attacker_slot target_slot reg_command next_routine
    | Sfg(f, g) ->
      check_attacker_field
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

let check_attacker attacker_slot target_slot reg_command next_routine =
  let field, vitality = get_prop_slot reg_command in
  check_attacker_field
    field
    attacker_slot
    target_slot
    reg_command
    next_routine

let build_attack attacker_slot target_slot amount reg_command reg_tmp reg_n_backup =
  let field, vitality = get_prop_slot reg_command in
  match field with
    | KX(_) ->
      lapp "S" reg_command
    | Sf(KX(Sfg(_, _))) ->
      rapp reg_command "succ"
    | AttackI(_) ->
      (* Prepare j *)
      set_field_to_value
	reg_tmp
	(255 - target_slot)
	(fun _ -> lapp "K" reg_command)
    | Sf(KX(AttackI(_))) ->
      rapp reg_command "get"
    | Sfg(KX(AttackI(_)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(AttackI(_)), Get)), Succ) ->
      (* Apply the value in slot 1 as argument j *)
      rapp reg_command "zero"
    | AttackIJ(_, _) ->
      (* Prepare n *)
      set_field_to_value
      	reg_n_backup
      	amount
      	(fun _ ->
      	  copy_value
      	    reg_tmp
      	    reg_n_backup
      	    (fun _ -> lapp "K" reg_command))
    | Sf(KX(AttackIJ(_, _))) ->
      rapp reg_command "get"
    | Sfg(KX(AttackIJ(_, _)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(AttackIJ(_, _)), Get)), Succ) ->
      (* Make sure that the value is still valid *)
      (* set_field_to_value *)
      (* 	reg_tmp *)
      (* 	amount *)
      (* 	(\* Apply the value in slot 1 as argument n *\) *)
      (* 	(fun _ -> rapp reg_command "zero") *)
	rapp reg_command "zero"
    | _ ->
      (* Prepare i *)
      set_field_to_value
	reg_command
	attacker_slot
	(fun _ -> lapp "attack" reg_command)

let build_help helper_slot proponent_slot amount reg_command reg_tmp =
  let field, vitality = get_prop_slot reg_command in
  match field with
    | KX(_) ->
      lapp "S" reg_command
    | Sf(KX(Sfg(_, _))) ->
      rapp reg_command "succ"
    | HelpI(_) ->
      (* Prepare j *)
      set_field_to_value
	reg_tmp
	proponent_slot
	(fun _ -> lapp "K" reg_command)
    | Sf(KX(HelpI(_))) ->
      rapp reg_command "get"
    | Sfg(KX(HelpI(_)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(HelpI(_)), Get)), Succ) ->
      (* Apply the value in slot 1 as argument j *)
      rapp reg_command "zero"
    | HelpIJ(_, _) ->
      (* Prepare n *)
      set_field_to_value
      	reg_tmp
      	amount
      	(fun _ -> lapp "K" reg_command)
    | Sf(KX(HelpIJ(_, _))) ->
      rapp reg_command "get"
    | Sfg(KX(HelpIJ(_, _)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(HelpIJ(_, _)), Get)), Succ) ->
      (* Apply the value in slot 1 as argument n *)
      rapp reg_command "zero"
    | _ ->
      (* Prepare i *)
      set_field_to_value
	reg_command
	helper_slot
	(fun _ -> lapp "help" reg_command)

let find_best_opp_target () =
  let _, vitality = get_opp_slot 1 in
  if vitality > 0 then
    1, vitality
  else begin
    let slot, vitality = find_alive_non_identity_opp_slot_forward 0 255 in
    if slot != -1 then
      (slot, vitality)
    else
      find_alive_opp_slot_forward 0 255
  end

let turn =
  if (Array.length Sys.argv) != 2 then
    exit 1
  else
    Array.get Sys.argv 1

;;

if turn = "1" then begin
  opp_check_zombie ();
  read_action ()
end;
while true do
  prop_check_zombie ();
  let reg_command = 2 and
      reg_tmp = 1 and
      reg_attack_n_backup = 5 in
  let dead_slot = find_dead_prop_slot 0 255 in
  if dead_slot != -1 then begin
    let reviver = find_alive_prop_slot 10 255 in
    if reviver != -1 then begin
      set_field_to_value
	reviver
	dead_slot
	(fun r -> lapp "revive" r)
    end else begin
      nop ()
    end
  end else begin
    let _, vitality = get_prop_slot 1 in
    if vitality < 10000 then begin
      let helper, helper_vitality = find_slot_with_vitality_ge 10000 10 255 in
      let helper, helper_vitality =
	if helper != -1 then helper, helper_vitality
	else 1, vitality in
      build_help
	helper
	1
	(helper_vitality - 1)
	reg_command
	reg_tmp
    end else begin
      let target_slot, target_vitality =
	find_best_opp_target () in
      let damage_needed =
	target_vitality * 10 / 9 + 10 in
      let attacker_slot, _ =
	find_slot_with_vitality_ge (damage_needed + 1) 10 255 in
      if attacker_slot = -1 then begin
	let helper_slot, vitality = find_slot_with_biggest_vitality 10 255 in
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
	  nop ()
	else
	  build_help
	    helper_slot
	    helper_slot
	    (vitality - 1)
	    reg_command
	    reg_tmp
      end else begin
	check_attacker
	  attacker_slot
	  target_slot
	  reg_command
	  (fun _ ->
	    build_attack
	      attacker_slot
	      target_slot
	      damage_needed
	      reg_command
	      reg_tmp
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
  end;
  opp_check_zombie ();
  read_action ()
done
