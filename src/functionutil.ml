open Simulator

let apply_function_to_opp_reg_same field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(Sfg(x, Copy)), Identity) then
    next_routine ()
  else if field = Sf(KX(Sfg(x, Copy))) then
    rapp reg_command "I"
  else if field = KX(Sfg(x, Copy)) then
    lapp "S" reg_command
  else if field = Sfg(x, Copy) then
    lapp "K" reg_command
  else if field = Sf(x) then
    rapp reg_command "copy"
  else if field = x then
    lapp "S" reg_command
  else
    on_mismatch ()

let apply_function_to_opp_reg_next field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(Sfg(x, Copy)), Succ) then
    next_routine ()
  else if field = Sf(KX(Sfg(x, Copy))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(x, Copy)) then
    lapp "S" reg_command
  else if field = Sfg(x, Copy) then
    lapp "K" reg_command
  else if field = Sf(x) then
    rapp reg_command "copy"
  else if field = x then
    lapp "S" reg_command
  else
    on_mismatch ()

(* let apply_function_to_reg0 field x reg_command next_routine on_mismatch = *)
(*   if field = Sfg(KX(x), Get) then *)
(*     next_routine () *)
(*   else if field = Sf(KX(x)) then *)
(*     rapp reg_command "get" *)
(*   else if field = KX(x) then *)
(*     lapp "S" reg_command *)
(*   else if field = x then *)
(*     lapp "K" reg_command *)
(*   else *)
(*     on_mismatch () *)

(* let apply_function_to_reg1 field x reg_command next_routine on_mismatch = *)
(*   if field = Sfg(KX(Sfg(KX(x), Get)), Succ) then *)
(*     next_routine () *)
(*   else if field = Sf(KX(Sfg(KX(x), Get))) then *)
(*     rapp reg_command "succ" *)
(*   else if field = KX(Sfg(KX(x), Get)) then *)
(*     lapp "S" reg_command *)
(*   else *)
(*     apply_function_to_reg0 field x reg_command next_routine on_mismatch *)

(* let apply_function_to_reg2 field x reg_command next_routine on_mismatch = *)
(*   if field = Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ) then *)
(*     next_routine () *)
(*   else if field = Sf(KX(Sfg(KX(Sfg(KX(x), Get)), Succ))) then *)
(*     rapp reg_command "succ" *)
(*   else if field = KX(Sfg(KX(Sfg(KX(x), Get)), Succ)) then *)
(*     lapp "S" reg_command *)
(*   else *)
(*     apply_function_to_reg1 field x reg_command next_routine on_mismatch *)

(* let apply_function_to_reg3 field x reg_command next_routine on_mismatch = *)
(*   if field = Sfg(KX(Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ)), Succ) then *)
(*     next_routine () *)
(*   else if field = Sf(KX(Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ))) then *)
(*     rapp reg_command "succ" *)
(*   else if field = KX(Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ)) then *)
(*     lapp "S" reg_command *)
(*   else *)
(*     apply_function_to_reg2 field x reg_command next_routine on_mismatch *)

(* let apply_function_to_reg n field x reg_command next_routine on_mismatch = *)
(*   if n == 0 then *)
(*     apply_function_to_reg0 field x reg_command next_routine on_mismatch *)
(*   else if n == 1 then *)
(*     apply_function_to_reg1 field x reg_command next_routine on_mismatch *)
(*   else if n == 2 then *)
(*     apply_function_to_reg2 field x reg_command next_routine on_mismatch *)
(*   else if n == 3 then *)
(*     apply_function_to_reg3 field x reg_command next_routine on_mismatch *)
(*   else *)
(*     raise Error *)

let apply_function_to_reg0 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(x), Get) then
    next_routine ()
  else if field = Sf(KX(x)) then
    rapp reg_command "get"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_reg1 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(Sfg(KX(x), Get)), Succ) then
    next_routine ()
  else if field = Sf(KX(Sfg(KX(x), Get))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(x), Get)) then
    lapp "S" reg_command
  else if field = Sfg(KX(x), Get) then
    lapp "K" reg_command
  else if field = Sf(KX(x)) then
    rapp reg_command "get"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_reg2 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ) then
    next_routine ()
  else if field = Sf(KX(Sfg(KX(Sfg(KX(x), Get)), Succ))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(Sfg(KX(x), Get)), Succ)) then
    lapp "S" reg_command
  else if field = Sfg(KX(Sfg(KX(x), Get)), Succ) then
    lapp "K" reg_command
  else if field = Sf(KX(Sfg(KX(x), Get))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(x), Get)) then
    lapp "S" reg_command
  else if field = Sfg(KX(x), Get) then
    lapp "K" reg_command
  else if field = Sf(KX(x)) then
    rapp reg_command "get"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_reg3 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ)), Succ) then
    next_routine ()
  else if field = Sf(KX(Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ)) then
    lapp "S" reg_command
  else if field = Sfg(KX(Sfg(KX(Sfg(KX(x), Get)), Succ)), Succ) then
    lapp "K" reg_command
  else if field = Sf(KX(Sfg(KX(Sfg(KX(x), Get)), Succ))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(Sfg(KX(x), Get)), Succ)) then
    lapp "S" reg_command
  else if field = Sfg(KX(Sfg(KX(x), Get)), Succ) then
    lapp "K" reg_command
  else if field = Sf(KX(Sfg(KX(x), Get))) then
    rapp reg_command "succ"
  else if field = KX(Sfg(KX(x), Get)) then
    lapp "S" reg_command
  else if field = Sfg(KX(x), Get) then
    lapp "K" reg_command
  else if field = Sf(KX(x)) then
    rapp reg_command "get"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

(* type parse_normal_state = Operator of field_type | SCombinator | KCombinator | NoCombinator of field_type *)

(* let parse_normal_operation field slot l = *)
(*   let rec p field slot l = *)
(*     match field with *)
(*       | Sfg(KX(x), op) -> *)
(* 	p x slot (Operator(op) :: l) *)
(*       | x -> *)
(* 	NoCombinator(xo) :: l in *)
(*   begin *)
(*     match field with *)
(*       | Sfg(KX(x), op) -> *)
(* 	p x slot (Operator(op) :: l) *)
(*       | Sf(KX(x)) -> *)
(* 	p x slot (SCombinator :: l) *)
(*       | KX(x) -> *)
(* 	p x slot (KCombinator :: l) *)
(*       | x -> *)
(* 	NoCombinator(x) :: l *)
(*   end *)

(* let apply_function_to_reg operand field action slot next_routine on_mismatch = *)
(*   let rec check l i = *)
(*     if i > operand then *)
(*       on_mismatch () *)
(*     else begin *)
(*       match l with *)
(* 	| [] -> *)
(* 	  next_routine () *)
(*         | x :: xs -> *)
(* 	  begin *)
(* 	    match x with *)
(* 	      | Operator(op) -> *)
(* 		if i = 0 && op != Get then *)
(* 		  on_mismatch (); *)
(* 		if i != 0 && op != Succ then *)
(* 		  on_mismatch (); *)
(* 		if i = operand && xs != [] then *)
(* 		  on_mismatch (); *)
(* 		if i != operand then *)
(* 		  lapp "K" slot *)
(* 		else *)
(* 		  next_routine () *)
(* 	      | SCombinator -> *)
(* 		if i = 0 then *)
(* 		  rapp slot "get" *)
(* 		else *)
(* 		  rapp slot "succ" *)
(* 	      | KCombinator -> *)
(* 		lapp "S" slot *)
(* 	      | NoCombinator(_) -> *)
(* 		on_mismatch () *)
(* 	  end; *)
(* 	  check xs (i + 1) *)
(*     end in *)
(*   match parse_normal_operation field slot [] with *)
(*     | [] -> *)
(*       on_mismatch () *)
(*     | x :: xs -> *)
(*       begin *)
(* 	match x with *)
(* 	  | NoCombinator(last) -> *)
(* 	    if last != action then *)
(* 	      on_mismatch () *)
(* 	    else begin *)
(* 	      match xs with *)
(* 		| [] -> *)
(* 		  rapp slot "K" *)
(* 		| x -> *)
(* 		  check xs 0 *)
(* 	    end *)
(* 	  | _ -> *)
(* 	    on_mismatch () *)
(*       end *)
      
let apply_function_to_0 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(x), Identity) then
    next_routine ()
  else if field = Sf(KX(x)) then
    rapp reg_command "I"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let apply_function_to_1 field x reg_command next_routine on_mismatch =
  if field = Sfg(KX(x), Succ) then
    next_routine ()
  else if field = Sf(KX(x)) then
    rapp reg_command "succ"
  else if field = KX(x) then
    lapp "S" reg_command
  else if field = x then
    lapp "K" reg_command
  else
    on_mismatch ()

let wrap_infinite_loop_in_reg0 field x reg_command next_routine on_mismatch =
  (* Matching order is important *)
  if field = Sfg(Sfg(x, Get), Identity) then
    next_routine ()
  else if field = Sf(Sfg(x, Get)) then
    rapp reg_command "I"
  else if field = Sfg(x, Get) then
    lapp "S" reg_command
  else if field = Sf(x) then
    rapp reg_command "get"
  else
    on_mismatch ()
