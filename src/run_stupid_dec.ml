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
  let alive_slot, _ = find_alive_opp_slot_backward 0 255 in
  let arg0 = 255 - alive_slot in
  set_field_to_value
    reg1
    arg0
    (fun _ ->
      copy_value
	reg0
	reg1
	(fun r -> lapp "dec" r));
  read_action ()
done
