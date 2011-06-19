open Simulator
open Strategy
open Command

;;

let controller _ =
  revive_any_dead 
    (fun _ ->
      protect_against_attack
	(fun _ ->
	  heal_damaged2
      	    (fun _ ->
	      zombienize
		(fun _ -> normal_attack nop)))) in
run_simulator controller
