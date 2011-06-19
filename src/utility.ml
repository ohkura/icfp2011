open Simulator

let find_alive_opp_slot_backward low high =
  let rec f i =
    if i < low then
      (-1, -1)
    else begin
      let _, vitality = get_opp_slot i in
      if vitality > 0 then
	(i, vitality)
      else
	f (i - 1)
    end in
  f high

let find_alive_opp_slot_forward low high =
  let rec f i =
    if i > high then
      (-1, -1)
    else begin
      let _, vitality = get_opp_slot i in
      if vitality > 0 then
	(i, vitality)
      else
	f (i + 1)
    end in
  f low

let find_dead_opp_slot_forward low high =
  let rec f i =
    if i > high then
      -1
    else begin
      let _, vitality = get_opp_slot i in
      if vitality <= 0 then
	i
      else
	f (i + 1)
    end in
  f low

let find_alive_non_identity_opp_slot_forward low high =
  let rec f i =
    if i > high then
      (-1, -1)
    else begin
      let field, vitality = get_opp_slot i in
      if vitality > 0 && field != Identity then
	(i, vitality)
      else
	f (i + 1)
    end in
  f low

let find_alive_opp_slot_forward_ex ex low high =
  let rec f i =
    if i > high then
      (-1, -1)
    else begin
      let field, vitality = get_opp_slot i in
      if vitality > 0 && i != ex then
	(i, vitality)
      else
	f (i + 1)
    end in
  f low

let find_dead_non_identity_opp_slot_forward low high =
  let rec f i =
    if i > high then
      -1
    else begin
      let field, vitality = get_opp_slot i in
      if vitality <= 0 && field != Identity then
	i
      else
	f (i + 1)
    end in
  f low

let rec get_attacking_slot field =
  match field with
    | Sf(f) -> get_attacking_slot f
    | Sfg(f,g) -> get_attacking_slot f
    | KX(f) -> get_attacking_slot f
    | AttackI(i) -> -1
    | AttackIJ(i, j) ->
      begin
	  match j with 
	    | Value(value) -> 255 - value
	    | _ -> -1
      end
    | Value(i) -> -1
    | _ -> -1
  
let rec find_opp_busy_alive_slot pos current_max =
  if pos > 255 then
    -1
  else
    let field, vitality = get_opp_slot pos in
      if vitality <= 0 then
	find_opp_busy_alive_slot (pos + 1) current_max
      else
	let count = opp_get_count.(pos) in
	  if count > current_max then
	        find_opp_busy_alive_slot (pos + 1) count
	  else
	        find_opp_busy_alive_slot (pos + 1) current_max

let find_opp_busy_alive =
  let slot = find_opp_busy_alive_slot 0 0 in
    if slot >= 0 then
      let _, vitality = get_opp_slot slot in
      slot, vitality
    else
      -1, -1

let find_attacking_slot reg_attack_command =
  let field, _ = get_prop_slot reg_attack_command in
    get_attacking_slot field

let find_dead_prop_slot low high =
  let rec f i =
    if i > high then
      -1
    else begin
      let _, vitality = get_prop_slot i in
      if vitality <= 0 then i
      else f (i + 1)
    end in
  f low

let find_alive_prop_slot low high =
  let rec f i =
    if i > high then
      -1
    else begin
      let _, vitality = get_prop_slot i in
      if vitality > 0 then i
      else f (i + 1)
    end in
  f low

let find_slot_with_field x =
  let rec f i =
    if i > 255 then
      -1
    else begin
      let field, _ = get_prop_slot i in
      if field = x then
	i
      else
	f (i + 1)
    end in
  f 0

let find_slot_with_field_except ex x =
  let rec f i =
    if i > 255 then
      -1
    else begin
      let field, v = get_prop_slot i in
      if field = x && i != ex then
	i
      else
	f (i + 1)
    end in
  f 0

let find_slot_except ex =
  let rec f i =
    if i > 255 then
      -1
    else begin
      if i != ex then
	i
      else
	f (i + 1)
    end in
  f 0

let find_slot_with_field_value_lt x =
  let rec f i =
    if i > 255 then
      (-1, -1)
    else begin
      let field, _ = get_prop_slot i in
      match field with
	| Value(y) -> begin
	  if y < x then
	    (i, y)
	  else
	    f (i + 1)
	  end
	| _ -> f (i + 1)
    end in
  f 0

let find_alive_opp_slot_with_field_value_lt x =
  let rec f i =
    if i > 255 then
      (-1, -1)
    else begin
      let field, _ = get_opp_slot i in
      match field with
	| Value(y) -> begin
	    if y < x && y > 0 then
	          (i, y)
	    else
	          f (i + 1)
	end
	| _ -> f (i + 1)
    end in
  f 0

let find_slot_with_vitality_ge x low high =
  let rec f i =
    if i > high then
      (-1, -1)
    else begin
      let _, vitality = get_prop_slot i in
      if vitality >= x then
	(i, vitality)
      else
	f (i + 1)
    end in	  
  f low

let find_slot_with_biggest_vitality low high =
  let rec f i (current_slot, current_vitality) =
    if i > high then
      (current_slot, current_vitality)
    else begin
      let _, vitality = get_prop_slot i in
      f (i + 1) (
	if vitality > current_vitality then
	  (i, vitality)
	else
	  (current_slot, current_vitality))
    end in	  
  f low (-1, -1)

let find_opp_slot_with_biggest_vitality low high =
  let rec f i (current_slot, current_vitality) =
    if i > high then
      (current_slot, current_vitality)
    else begin
      let _, vitality = get_opp_slot i in
      f (i + 1) (
	if vitality > current_vitality then
	  (i, vitality)
	else
	  (current_slot, current_vitality))
    end in	  
  f low (-1, -1)

let choose_succ_dbl now target =
  let rec compute_shift now target =
    if now * 2 > target then
      1
    else
      2 * compute_shift (now * 2) target in
  if now = 0 then
    "succ"
  else begin
    let mul = compute_shift now target in
    if mul = 1 then
      "succ"
    else begin
      if mul > target - mul * now then
	"dbl"
      else
	"succ"
    end
  end

let power_ceil x =
  let rec f x current =
    if x = 0 then current
    else f (x / 2) (current * 2) in
  f x 1

let get_opp_vitality opp_slot =
  if opp_slot < 0 then
    0
  else let _, vitality = get_opp_slot opp_slot in
    vitality

let get_vitality slot =
  if slot < 0 then
    0
  else let _, vitality = get_prop_slot slot in
    vitality

let rec parse_target field =
  match field with
    | Value(x) -> x
    | _ -> -1

let rec find_attacking_target_from_field field =
  match field with
    | KX(f) -> find_attacking_target_from_field f
    | Sf(f) -> find_attacking_target_from_field f
    | Sfg(f,g) -> find_attacking_target_from_field f
    | AttackIJ(f,g) -> parse_target f
    | AttackI(f) -> parse_target f
    | _ -> -1

(* If opp constructs AttackI(Value(x)), find it and detects the attacking target *)
let find_opp_attacking_targets =
  let rec f i lst =
    if i > 255 then
      lst
    else begin
      let field, _ = get_opp_slot i in
      let attacking_target = (find_attacking_target_from_field field) in
      if attacking_target >= 0 then
	  f (i + 1) (attacking_target::lst)
      else
	  f (i + 1) lst
    end in
f 0 []
