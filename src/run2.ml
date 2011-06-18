open Simulator
open Strategy

;;

let controller _ =
  revive_dead
    (fun _ ->
      heal_damaged
	(fun _ ->
	  zombienize
	    (fun _ -> normal_attack nop))) in
run_simulator controller
