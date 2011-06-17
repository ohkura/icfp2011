open Simulator

let turn =
  if (Array.length Sys.argv) != 2 then
    exit 1
  else
    Array.get Sys.argv 1

let reg0 = 0
let reg1 = 1

;;

let _ =
  if turn = "1" then
    read_action ()
  else
    () in
while true do
  begin
    let alive = find_alive_opp_slot_backward 255 in
    let arg0 = 255 - alive in
    let field, vitality = get_prop_slot reg1 in
    match field with
      | Value(reg1_value) ->
	begin
	  if reg1_value = arg0 then begin
	    (* arg0 is prepared in reg1. Copy to reg0 for dec call *)
	    let field, vitality = get_prop_slot reg0 in
	    match field with
	      | Value(reg0_value) ->
		begin
		  if reg0_value = arg0 then
		    lapp "dec" reg0
		  else begin
		    if reg0_value = reg1 then begin
      		      lapp "get" reg0 (* Copy reg1 to reg0 *)
		    end else
		      set_field_to_value reg0 reg1
		  end
		end
	      | _ ->
		set_field_to_value reg0 reg1
	  end else begin
	    (* Need to initialize reg1 *)
	    set_field_to_value reg1 arg0
	  end
	end
      | _ ->
	set_field_to_value reg1 arg0
  end;
  read_action ()
done
