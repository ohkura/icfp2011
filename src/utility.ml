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
