open Simulator

let turn =
  if (Array.length Sys.argv) != 2 then
    exit 1
  else
    Array.get Sys.argv 1

let create_attack reg_source reg_command reg_tmp =
  let field, vitality = get_prop_slot reg_command in
  match field with
    | Sfg(KX(AttackI(_)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(AttackIJ(_, _)), Get) ->
      lapp "K" reg_command
    | Sfg(KX(Sfg(KX(AttackI(_)), Get)), Succ) ->
      rapp reg_command "zero"
    | Sfg(KX(Sfg(KX(AttackIJ(_, _)), Get)), Succ) ->
      rapp reg_command "zero"
    | Sf(KX(AttackI(_))) ->
      rapp reg_command "get"
    | Sf(KX(AttackIJ(_, _))) ->
      rapp reg_command "get"
    | Sf(KX(Sfg(_, _))) ->
      rapp reg_command "succ"
    | KX(_) ->
      lapp "S" reg_command
    | AttackI(_) ->
      set_field_to_value
	reg_tmp
	(255 - (find_alive_opp_slot_backward 255))
	(fun _ -> lapp "K" reg_command)
    | AttackIJ(_, _) ->
      set_field_to_value
	reg_tmp
	8192
	(fun _ -> lapp "K" reg_command)
    | _ ->
      set_field_to_value
	reg_command
	reg_source
	(fun _ -> lapp "attack" reg_command)

;;

let _ =
  if turn = "1" then
    read_action ()
  else
    () in
while true do
  let reg_command = 15 in
  let reg_tmp = 1 in
  create_attack
    (find_slot_with_vitality_ge 8200 20 255)
    reg_command
    reg_tmp;
  read_action ()
done
