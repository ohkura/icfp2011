open Simulator
open Strategy
open Command

;;

let controller _ =
  zombienize
    (fun _ -> normal_attack nop) in
run_simulator controller
