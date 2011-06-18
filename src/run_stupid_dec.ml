open Simulator
open Strategy

;;

let controller _ =
  stupid_dec nop in
run_simulator controller
