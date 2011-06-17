open Simulator

let turn =
  if (Array.length Sys.argv) != 2 then
    exit 1
  else
    Array.get Sys.argv 1

let create_number reg_number target next_routine =
  let field, vitality = get_prop_slot reg_number in
  match field with
    (* quick hack to avoid number overwrite *)
    | AttackI(_)
    | AttackIJ(_)
    | KX(_)
    | Sf(_)
    | Sfg(_, _) ->
      next_routine reg_number
    (* quick hack to avoid number overwrite - end *)

    | Value(value) ->
      if value = target then
	next_routine reg_number
      else
	begin
	  Printf.eprintf "Putting %d to %d" target reg_number;
	  prerr_newline ();
	  if value < target then
	    lapp (choose_succ_dbl value target) reg_number
	  else
	    lapp "put" reg_number
	end
    | Identity ->
      rapp reg_number "zero"
    | _ ->
      lapp "put" reg_number

let create_attack reg_attack reg_tmp =
  let field, vitality = get_prop_slot reg_attack in
  match field with
    | Sfg(KX(AttackI(_)), Get) ->
      lapp "K" reg_attack
    | Sfg(KX(AttackIJ(_, _)), Get) ->
      lapp "K" reg_attack
    | Sfg(KX(Sfg(KX(AttackI(_)), Get)), Succ) ->
      rapp reg_attack "zero"
    | Sfg(KX(Sfg(KX(AttackIJ(_, _)), Get)), Succ) ->
      rapp reg_attack "zero"
    | Sf(KX(AttackI(_))) ->
      rapp reg_attack "get"
    | Sf(KX(AttackIJ(_, _))) ->
      rapp reg_attack "get"
    | Sf(KX(Sfg(_, _))) ->
      rapp reg_attack "succ"
    | KX(_) ->
      lapp "S" reg_attack
    | AttackI(_) ->
      create_number
	reg_tmp
	(255 - (find_alive_opp_slot_backward 255))
	(fun _ -> lapp "K" reg_attack)
    | AttackIJ(_, _) ->
      create_number
	reg_tmp
	8192
	(fun _ -> lapp "K" reg_attack)
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
  create_number
    reg_attack
    (find_slot_with_vitality_ge 8200 20 255)
    (fun reg_number -> create_attack reg_number reg_tmp);
  read_action ()
done
