open Simulator

let turn =
  if (Array.length Sys.argv) != 2 then
    exit 1
  else
    Array.get Sys.argv 1

let create_number slot last_alive next_action =
  let field, vitality = get_prop_slot slot in
  match field with
    (* quick hack to avoid number overwrite *)
    | AttackI(_)
    | AttackIJ(_)
    | KX(_)
    | Sf(_)
    | Sfg(_, _) ->
      next_action ()
    (* quick hack to avoid number overwrite - end *)

    | Identity ->
      begin
	Printf.eprintf "IDENTITY";
	prerr_newline ();
	rapp slot "zero"
      end
    | Value(current) ->
      if current == last_alive then
	next_action ()
      else
	begin
	  Printf.eprintf "Putting %d to %d" last_alive slot;
	  prerr_newline ();
	  if current < last_alive then
	    begin
	      Printf.eprintf "CHOOSE_SUCC_DBL";
	      prerr_newline ();
	      lapp (choose_succ_dbl current last_alive) slot
	    end
	  else 
	    begin
	      Printf.eprintf "ZERO";
	      prerr_newline ();
	      lapp "put" slot
	    end
	end
    | PutX ->
      rapp slot "zero"
    | _ ->
      lapp "put" slot

let create_attack reg_attack reg_tmp =
  let field, vitality = get_prop_slot reg_attack in
  match field with
    | Sfg(KX(AttackI(_)), Get) ->
      lapp "K" reg_attack
    | Sfg(KX(AttackIJ(_,_)), Get) ->
      lapp "K" reg_attack
    | Sfg(KX(Sfg(KX(AttackI(_)), Get)), Succ) ->
      rapp reg_attack "zero"
    | Sfg(KX(Sfg(KX(AttackIJ(_, _)), Get)), Succ) ->
      rapp reg_attack "zero"
    | Sf(KX(AttackI(_))) ->
      rapp reg_attack "get"
    | Sf(KX(AttackIJ(_, _))) ->
      rapp reg_attack "get"
    | Sf(KX(Sfg(_,_))) ->
      rapp reg_attack "succ"
    | KX(_) ->
      lapp "S" reg_attack
    | AttackI(_) ->
      let arg_j =
	255 - (find_alive_opp_slot_backward 255) in
      create_number reg_tmp arg_j (fun _ -> lapp "K" reg_attack)
    | AttackIJ(_, _) ->
      create_number reg_tmp 8192 (fun _ -> lapp "K" reg_attack)
    | _ ->
      lapp "attack" reg_attack

;;

let _ =
  if turn = "1" then
    read_action ()
  else
    () in
while true do
  let reg_attack = 15 in
  let reg_tmp = 1 in
  let attack_source =
    find_slot_with_vitality_ge 8200 20 255 in
  create_number
    reg_attack
    attack_source
    (fun _ -> create_attack reg_attack reg_tmp);
  read_action ()
done
