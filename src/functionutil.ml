open Simulator

let apply_function_to_opp_reg0 field x reg_command next_routine on_mismatch =
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

let apply_function_to_opp_reg1 field x reg_command next_routine on_mismatch =
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

