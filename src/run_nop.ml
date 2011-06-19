open Simulator
open Strategy
open Command

let reg0 = 0
let reg1 = 1

;;

let controller _ =
  nop () in
run_simulator controller;;
