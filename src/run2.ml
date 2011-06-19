open Simulator
open Strategy
open Command

;;

let controller _ =
  revive_any_dead 
    (fun _ ->
       heal_damaged
      	 (fun _ ->
	    protect_against_attack
      	      (fun _ ->
		 zombienize
		   (fun _ -> normal_attack nop)))) in
run_simulator controller
