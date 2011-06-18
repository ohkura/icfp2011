open Simulator

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
      (* Apply the value in slot 1 as argument n *)
      rapp reg_command "zero"
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
  let field1, vitality1 = get_opp_slot 1 in
  if vitality1 > 0 then
    1
  else
    (find_alive_opp_slot_forward 0)

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
  let reviver, _ = find_slot_with_vitality_ge 1 50 255 in
  let _, vitality1 = proponent.(1) in
  if vitality1 < 1 then begin
    set_field_to_value
      reviver
      1
      (fun r -> lapp "revive" r)
  end else if vitality1 < 10000 then begin
    let helper, vitality = find_slot_with_vitality_ge 10000 10 255 in
    if helper = -1 then
      let alive = find_alive_opp_slot_backward 255 in
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
    (* let attacker, _ = find_slot_with_vitality_ge 11114 10 255 in *)
    let attacker, _ = find_slot_with_vitality_ge 65535 10 255 in
    if attacker = -1 then begin
      let helper, vitality = find_slot_with_vitality_ge 10000 10 255 in
      if helper = -1 then
	let alive = find_alive_opp_slot_backward 255 in
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
      build_attack
	attacker
	(find_best_opp_target ())
	11113
	reg_command
	reg_tmp
	reg_i_backup
	reg_j_backup
	reg_n_backup;
  (* let attacker, _ = find_slot_with_vitality_ge 8200 20 255 in *)
  (* build_attack *)
  (*   attacker *)
  (*   (find_alive_opp_slot_forward 0) *)
  (*   8192 *)
  (*   reg_command *)
  (*   reg_tmp; *)
    end
  end;
  opp_check_zombie ();
  read_action ()
done
