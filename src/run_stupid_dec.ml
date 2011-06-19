open Simulator
open Strategy
open Command

;;

let controller _ =
  stupid_dec nop in
run_simulator controller
