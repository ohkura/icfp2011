open Simulator
open Utility

let reg0 = 0
let reg_tmp = 1
let reg1 = 1
let reg2 = 2
let reg3 = 3
let reg_attack_j_backup = 4
let reg_attack_n_backup = 5
let reg_help_command = 0
let reg_attack_command = 6
let min_free_reg = 7

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
