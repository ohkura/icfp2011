open Simulator
open Strategy
open Command
open Commandutil

;;

let controller _ =
  (* stupid_dec nop in *)
  build_cast_all_into_reg0
    Dec
    (fun r c ->
      set_field_to_card r "dec" c)
    (fun _ -> rapp 0 "zero") in
run_simulator controller
