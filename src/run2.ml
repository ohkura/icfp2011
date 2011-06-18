open Simulator

let check_value field expect clear_slot next_routine =
  match field with
    | Value(v) ->
      begin
	  if v = expect then
	        (next_routine clear_slot)
	  else
	    set_field_to_zero clear_slot next_routine
      end
    | _ ->
      set_field_to_zero clear_slot next_routine


let rec check_attacker_field field attacker_slot opponent_slot reg_command next_routine =
  match field with
    | KX(f1) -> (check_attacker_field
		      f1 attacker_slot opponent_slot reg_command
		         next_routine)
    | Sf(f1) -> (check_attacker_field
		      f1 attacker_slot opponent_slot reg_command
		         next_routine)
    | Sfg(f1, g2) -> (check_attacker_field
			f1 attacker_slot opponent_slot reg_command
			next_routine)
    | AttackI(f1) -> (check_value
			f1 attacker_slot reg_command next_routine)
    | AttackIJ(f1,f2) -> (check_value
			        f1 attacker_slot reg_command
				    (fun _ -> check_value
				             f2 opponent_slot reg_command next_routine
				    )
    )
    | _ -> next_routine reg_command

let check_attacker attacker_slot opponent_slot reg_command next_routine =
  let field, vitality = get_prop_slot reg_command in
    check_attacker_field field attacker_slot opponent_slot reg_command next_routine

let build_attack attacker_slot opponent_slot amount reg_command reg_tmp reg_i_backup reg_j_backup reg_n_backup =
  let field, vitality = get_prop_slot reg_command in
  match field with
    | KX(_) ->
      lapp "S" reg_command
    | Sf(KX(Sfg(_, _))) ->
      rapp reg_command "succ"
    | AttackI(_) ->
      (* Prepare j *)
      (* set_field_to_value *)
      (* 	reg_j_backup *)
      (* 	(255 - opponent_slot) *)
      (* 	(fun _ -> *)
      (* 	  copy_value *)
      (* 	    reg_tmp *)
      (* 	    reg_j_backup *)
      (* 	    (fun _ -> lapp "K" reg_command)) *)
      set_field_to_value
	reg_tmp
	(255 - opponent_slot)
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
      (* set_field_to_value *)
      (* 	reg_n_backup *)
      (* 	amount *)
      (* 	(fun _ -> *)
      (* 	  copy_value *)
      (* 	    reg_tmp *)
      (* 	    reg_n_backup *)
      (* 	    (fun _ -> lapp "K" reg_command)) *)
      set_field_to_value
      	reg_tmp
      	amount
      	(fun _ -> lapp "K" reg_command)
    | Sf(KX(AttackIJ(_, _))) ->
      rapp reg_command "get"
    | Sfg(KX(AttackIJ(_, _)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(AttackIJ(_, _)), Get)), Succ) ->
      (* Make sure that the value is still valid *)
      set_field_to_value
	  reg_tmp
	  amount
	  (* Apply the value in slot 1 as argument n *)
	  (fun _ -> rapp reg_command "zero")
    | _ ->
      (* set_field_to_value *)
      (* 	reg_i_backup *)
      (* 	attacker_slot *)
      (* 	(fun _ -> *)
      (* 	  copy_value *)
      (* 	    reg_command *)
      (* 	    reg_i_backup *)
      (* 	    (fun _ -> lapp "attack" reg_command)) *)
      set_field_to_value
	reg_command
	attacker_slot
	(fun _ -> lapp "attack" reg_command)

let build_help helper_slot proponent_slot amount reg_command reg_tmp reg_i_backup reg_j_backup reg_n_backup =
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
      (* set_field_to_value *)
      (* 	reg_n_backup *)
      (* 	amount *)
      (* 	(fun _ -> *)
      (* 	  copy_value *)
      (* 	    reg_tmp *)
      (* 	    reg_n_backup *)
      (* 	    (fun _ -> lapp "K" reg_command)) *)
    | Sf(KX(HelpIJ(_, _))) ->
      rapp reg_command "get"
    | Sfg(KX(HelpIJ(_, _)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(HelpIJ(_, _)), Get)), Succ) ->
      (* Apply the value in slot 1 as argument n *)
      rapp reg_command "zero"
    | _ ->
      (* set_field_to_value *)
      (* 	reg_i_backup *)
      (* 	helper_slot *)
      (* 	(fun _ -> *)
      (* 	  copy_value *)
      (* 	    reg_command *)
      (* 	    reg_i_backup *)
      (* 	    (fun _ -> lapp "help" reg_command)) *)
      set_field_to_value
	reg_command
	helper_slot
	(fun _ -> lapp "help" reg_command)

let find_best_opp_target () =
  let _, vitality = get_opp_slot 1 in
  if vitality > 0 then
    1
  else
    find_alive_opp_slot_forward 0 255

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
      reg_i_backup = 3 and
      reg_j_backup = 4 and
      reg_n_backup = 5 in
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
	reg_i_backup
	reg_j_backup
	reg_n_backup
    end else begin
    (* let attacker, _ = find_slot_with_vitality_ge 11114 10 255 in *)
      let attacker, _ = find_slot_with_vitality_ge 65535 10 255 in
      if attacker = -1 then begin
	let helper, vitality = find_slot_with_vitality_ge 10000 10 255 in
	if helper = -1 then
	  let alive = find_alive_opp_slot_backward 0 255 in
	  let arg0 = 255 - alive in
	  set_field_to_value
	    reg_tmp
	    arg0
	    (fun _ ->
	      copy_value
		reg_command
		reg_tmp
		(fun r -> lapp "dec" r));
	else
	  build_help
	    helper
	    helper
	    (vitality - 1)
	    reg_command
	    reg_tmp
	    reg_i_backup
	    reg_j_backup
	    reg_n_backup
      end else begin
	let opp_target = find_best_opp_target () in
	check_attacker
	    attacker
	    opp_target
	    reg_command
	    (fun _ -> 
	           build_attack
		            attacker
		            opp_target
		            11113
		            reg_command
		            reg_tmp
		            reg_i_backup
		            reg_j_backup
		            reg_n_backup);
  (* let attacker, _ = find_slot_with_vitality_ge 8200 20 255 in *)
  (* build_attack *)
  (*   attacker *)
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
