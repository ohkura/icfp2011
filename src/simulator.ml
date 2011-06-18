exception Error

type field_type =
  | Identity
  | Succ
  | Dbl
  | Get
  | Put
  | S | Sf of field_type | Sfg of field_type * field_type
  | K | KX of field_type
  | Inc
  | Dec
  | Attack | AttackI of field_type | AttackIJ of field_type * field_type
  | Help | HelpI of field_type | HelpIJ of field_type * field_type
  | Copy
  | Revive
  | Zombie | ZombieI of field_type
  | Value of int

let proponent =
  Array.init 256 (fun _ -> (Identity, 10000))
let opponent =
  Array.init 256 (fun _ -> (Identity, 10000))

let get_prop_slot slot =
  Array.get proponent slot
let set_prop_slot slot v =
  Array.set proponent slot v
let get_opp_slot slot =
  Array.get opponent slot
let set_opp_slot slot v =
  Array.set opponent slot v

let validate_value i =
  match i with
    | Value(x) -> x
    | _ -> raise Error

let validate_slot_number i =
  let x =
    validate_value i in
  if x >= 0 && x < 256 then
    x
  else
    raise Error

let max a b =
  if a > b then a else b
let min a b =
  if a > b then b else a

let succ n =
  let x =
    validate_value n in
  if x < 65535 then
    Value(x + 1)
  else
    Value(65535)
let dbl n =
  let x =
    validate_value n in
  if x < 32768 then
    Value(x * 2)
  else
    Value(65535)
let prop_get i =
  let x =
    validate_slot_number i in
  let field, vitality =
    get_prop_slot x in
  if vitality > 0 then
    field
  else
    raise Error
let opp_get i =
  let x =
    validate_slot_number i in
  let field, vitality =
    get_opp_slot x in
  if vitality > 0 then
    field
  else
    raise Error
let prop_inc i zombie =
  let x =
    validate_slot_number i in
  let field, vitality =
    get_prop_slot x in
  let _ =
    if zombie && vitality > 0 then
      set_prop_slot x (field, vitality - 1)
    else if not zombie && vitality > 0 && vitality < 65535 then
      set_prop_slot x (field, vitality + 1)
    else
      () in
  Identity
let opp_inc i zombie =
  let x =
    validate_slot_number i in
  let field, vitality =
    get_opp_slot x in
  let _ =
    if zombie && vitality > 0 then
      set_opp_slot x (field, vitality - 1)
    else if not zombie && vitality > 0 && vitality < 65535 then
      set_opp_slot x (field, vitality + 1)
    else
      () in
  Identity
let prop_dec i zombie =
  let x =
    validate_slot_number i in
  let field, vitality =
    get_opp_slot (255 - x) in
  let _ =
    if zombie && vitality > 0 && vitality < 65535 then
      set_opp_slot (255 - x) (field, vitality + 1)
    else if not zombie && vitality > 0 then
      set_opp_slot (255 - x) (field, vitality - 1)
    else
      () in
  Identity
let opp_dec i zombie =
  let x =
    validate_slot_number i in
  let field, vitality =
    get_prop_slot (255 - x) in
  let _ =
    if zombie && vitality > 0 && vitality < 65535 then
      set_prop_slot (255 - x) (field, vitality + 1)
    else if not zombie && vitality > 0 then
      set_prop_slot (255 - x) (field, vitality - 1)
    else
      () in
  Identity
let prop_attack i j n zombie =
  let x = validate_slot_number i in
  let amount = validate_value n in
  let prop_field, prop_vitality = get_prop_slot x in
  let _ =
    if prop_vitality < amount then
      raise Error
    else
      set_prop_slot x (prop_field, prop_vitality - amount) in
  let y = validate_slot_number j in
  let opp_field, opp_vitality = get_opp_slot (255 - y) in
  let _ =
    if zombie && opp_vitality > 0 then
      set_opp_slot (255 - y) (opp_field, min (opp_vitality + amount * 9 / 10) 65535)
    else if zombie then
      ()
    else
      set_opp_slot (255 - y) (opp_field, max (opp_vitality - amount * 9 / 10) 0) in
  Identity
let opp_attack i j n zombie =
  let x = validate_slot_number i in
  let amount = validate_value n in
  let opp_field, opp_vitality = get_opp_slot x in
  let _ =
    if opp_vitality < amount then
      raise Error
    else
      set_opp_slot x (opp_field, opp_vitality - amount) in
  let y = validate_slot_number j in
  let prop_field, prop_vitality = get_prop_slot (255 - y) in
  let _ =
    if zombie && prop_vitality > 0 then
      set_prop_slot (255 - y) (prop_field, min (prop_vitality + amount * 9 / 10) 65535)
    else if zombie then
      ()
    else
      set_prop_slot (255 - y) (prop_field, max (prop_vitality - amount * 9 / 10) 0) in
  Identity
let prop_help i j n zombie =
  let x = validate_slot_number i in
  let amount = validate_value n in
  let prop_field, prop_vitality = get_prop_slot x in
  let _ =
    if prop_vitality < amount then
      raise Error
    else
      set_prop_slot x (prop_field, prop_vitality - amount) in
  let y = validate_slot_number j in
  let prop_field, prop_vitality = get_prop_slot y in
  let _ =
    if zombie then
      set_prop_slot y (prop_field, max (prop_vitality - amount * 11 / 10) 0)
    else if prop_vitality > 0 then
      set_prop_slot y (prop_field, min (prop_vitality + amount * 11 / 10) 65535)
    else
      () in
  Identity
let opp_help i j n zombie =
  let x = validate_slot_number i in
  let amount = validate_value n in
  let opp_field, opp_vitality = get_opp_slot x in
  let _ =
    if opp_vitality < amount then
      raise Error
    else
      set_opp_slot x (opp_field, opp_vitality - amount) in
  let y = validate_slot_number j in
  let opp_field, opp_vitality = get_opp_slot y in
  let _ =
    if zombie then
      set_opp_slot y (opp_field, max (opp_vitality - amount * 11 / 10) 0)
    else if opp_vitality > 0 then
      set_opp_slot y (opp_field, min (opp_vitality + amount * 11 / 10) 65535)
    else
      () in
  Identity
let prop_copy i =
  let x = validate_slot_number i in
  let field, _ = get_opp_slot x in
  field
let opp_copy i =
  let x = validate_slot_number i in
  let field, _ = get_prop_slot x in
  field
let prop_revive i =
  let x = validate_slot_number i in
  let field, vitality = get_prop_slot x in
  let _ =
    if vitality <= 0 then
      set_prop_slot x (field, 1)
    else
      () in
  Identity
let opp_revive i =
  let x = validate_slot_number i in
  let field, vitality = get_opp_slot x in
  let _ =
    if vitality <= 0 then
      set_opp_slot x (field, 1)
    else
      () in
  Identity
let prop_zombie i x =
  let slot = validate_slot_number i in
  let field, vitality = get_opp_slot (255 - slot) in
  if vitality <= 0 then
    let _ = set_opp_slot (255 - slot) (x, -1) in
    Identity
  else
    raise Error
let opp_zombie i x =
  let slot = validate_slot_number i in
  let field, vitality = get_prop_slot (255 - slot) in
  if vitality <= 0 then
    let _ = set_prop_slot (255 - slot) (x, -1) in
    Identity
  else
    raise Error

let parse_card card =
  match card with
    | "I" -> Identity
    | "zero" -> Value(0)
    | "succ" -> Succ
    | "dbl" -> Dbl
    | "get" -> Get
    | "put" -> Put
    | "S" -> S
    | "K" -> K
    | "inc" -> Inc
    | "dec" -> Dec
    | "attack" -> Attack
    | "help" -> Help
    | "copy" -> Copy
    | "revive" -> Revive
    | "zombie" -> Zombie
    | _ -> raise Error

let read_card () =
  parse_card (read_line())

let rec process_prop_action left right zombie =
  match left with
    | Identity -> right
    | Succ -> succ right
    | Dbl -> dbl right
    | Get -> prop_get right
    | Put -> Identity
    | S -> Sf(right)
    | Sf(f) -> Sfg(f, right)
    | Sfg(f, g) ->
      process_prop_action
	(process_prop_action f right zombie)
	(process_prop_action g right zombie)
	zombie
    | K -> KX(right)
    | KX(x) -> x
    | Inc -> prop_inc right zombie
    | Dec -> prop_dec right zombie
    | Attack -> AttackI(right)
    | AttackI(i) -> AttackIJ(i, right)
    | AttackIJ(i, j) -> prop_attack i j right zombie
    | Help -> HelpI(right)
    | HelpI(i) -> HelpIJ(i, right)
    | HelpIJ(i, j) -> prop_help i j right zombie
    | Copy -> prop_copy right
    | Revive -> prop_revive right
    | Zombie -> ZombieI(right)
    | ZombieI(i) -> prop_zombie i right
    | Value(i) -> raise Error

let rec process_opp_action left right zombie =
  match left with
    | Identity -> right
    | Succ -> succ right
    | Dbl -> dbl right
    | Get -> opp_get right
    | Put -> Identity
    | S -> Sf(right)
    | Sf(f) -> Sfg(f, right)
    | Sfg(f, g) ->
      process_opp_action
	(process_opp_action f right zombie)
	(process_opp_action g right zombie)
	zombie
    | K -> KX(right)
    | KX(x) -> x
    | Inc -> opp_inc right zombie
    | Dec -> opp_dec right zombie
    | Attack -> AttackI(right)
    | AttackI(i) -> AttackIJ(i, right)
    | AttackIJ(i, j) -> opp_attack i j right zombie
    | Help -> HelpI(right)
    | HelpI(i) -> HelpIJ(i, right)
    | HelpIJ(i, j) -> opp_help i j right zombie
    | Copy -> opp_copy right
    | Revive -> opp_revive right
    | Zombie -> ZombieI(right)
    | ZombieI(i) -> opp_zombie i right
    | Value(i) -> raise Error

let read_action () =
  let t = read_int() in
  match t with
    | 1 ->
      begin
	let card = read_card () in
	let slot = read_int () in
	let field, vitality = get_opp_slot slot in
	if vitality <= 0 then
	  ()
	else begin
	  try
	    let new_field =
	      process_opp_action card field false in
	    begin
	      set_opp_slot slot (new_field, vitality)
	    end
	  with
	    | Error ->
	      ()
	end
      end
    | 2 ->
      begin
	let slot = read_int () in
	let card = read_card () in
	let field, vitality = get_opp_slot slot in
	if vitality <= 0 then
	  ()
	else begin
	  try
	    let new_field =
	      process_opp_action field card false in
	    begin
	      set_opp_slot slot (new_field, vitality)
	    end
	  with
	    | Error ->
	      ()
	end
      end
    | _ -> raise Error

let prop_check_zombie () =
  let rec f i =
    if i > 255 then
      ()
    else begin
      let field, vitality =
	get_prop_slot i in
      if vitality = -1 then begin
	begin
	  try
	    let _ = process_prop_action field Identity true in
	    ()
	  with
	    | Error ->
	      ()
	end;
	set_prop_slot i (Identity, 0);
      end;
      f (i + 1)
    end in
  f 0

let opp_check_zombie () =
  let rec f i =
    if i > 255 then
      ()
    else begin
      let field, vitality =
	get_opp_slot i in
      if vitality = -1 then begin
	begin
	  try
	    let _ = process_opp_action field Identity true in
	    ()
	  with
	    | Error ->
	      ()
	end;
	set_opp_slot i (Identity, 0)
      end;
      f (i + 1)
    end in
  f 0

let lapp card slot =
  print_endline "1";
  print_endline card;
  print_int slot; print_newline ();
  try
    let field, vitality = get_prop_slot slot in
    let new_field =
      process_prop_action (parse_card card) field false in
    begin
	(* Printf.eprintf "Prop %d (field change)" slot; *)
	(* prerr_newline (); *)
      set_prop_slot slot (new_field, vitality)
    end
  with
    | Error ->
      ()
      (* prerr_endline "Prop phase end with error" *)

let rapp slot card =
  print_endline "2";
  print_int slot; print_newline ();
  print_endline card;
  try
    let field, vitality = get_prop_slot slot in
    let new_field =
      process_prop_action field (parse_card card) false in
    begin
	(* Printf.eprintf "Prop %d (field change)" slot; *)
	(* prerr_newline (); *)
      set_prop_slot slot (new_field, vitality)
    end
  with
    | Error ->
      ()
      (* prerr_endline "Prop phase end with error" *)

let rec find_alive_opp_slot_backward i =
  if i < 0 then
    0
  else
    let field, vitality = get_opp_slot i in
    if vitality > 0 then
      i
    else
      find_alive_opp_slot_backward (i - 1)

let rec find_alive_opp_slot_forward i =
  if i > 255 then
    255
  else
    let field, vitality = get_opp_slot i in
    if vitality > 0 then
      i
    else
      find_alive_opp_slot_forward (i + 1)

let find_slot_with_field x =
  let rec f i =
    if i > 255 then
      -1
    else begin
      let field, v = get_prop_slot i in
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
      let field, v = get_prop_slot i in
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
      let field, v = get_prop_slot i in
      if v >= x then
	(i, v)
      else
	f (i + 1)
    end in	  
  f low

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

let set_field_to_zero slot next_routine =
  let field, vitality = get_prop_slot slot in
  match field with
    | Value(value) ->
      if value = 0 then
	next_routine slot
      else
	lapp "put" slot
    | Identity ->
      rapp slot "zero"
    | _ ->
      lapp "put" slot

let rec set_field_to_value slot target next_routine =
  let rep slot =
    set_field_to_value slot target next_routine in
  let field, vitality = get_prop_slot slot in
  match field with
    | Value(value) ->
      if value = target then
	next_routine slot
      else
	if value < target then
	  lapp (choose_succ_dbl value target) slot
	else
	  set_field_to_zero slot rep
    | _ ->
      set_field_to_zero slot rep
	
let rec copy_value dst src next_routine =
  let rep _ =
    copy_value dst src next_routine in
  let dst_field, _ =
    get_prop_slot dst in
  let src_field, _ =
    get_prop_slot src in
  if dst_field = src_field then (* revisit == or = *)
    next_routine dst
  else
    match dst_field with
      | Value(value) ->
	if value = src then
	  lapp "get" dst
	else
	  set_field_to_value dst src rep
      | _ ->
	set_field_to_value dst src rep
	  
