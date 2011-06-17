open Simulator

type strategy_type =
  | Zombie of int
  | Kill of int
  | ApplyIntArg of int (*slot*) * int (* value *)
  | PrepareIntAndGet of int (* dst *) * int (* src *)
  | Attack of int * int * int
  | Help of int * int * int
  | PrepareInt of int (* value *)

let print_play = fun play ->
  match play with
    |(1, card, slot) -> (Printf.sprintf "%d\n%s\n%d" 1 card slot)
    |(2, card, slot) -> (Printf.sprintf "%d\n%d\n%s" 2 slot card)

let apply_card_to_slot = fun card slot ->
  (1, card, slot)

let apply_slot_to_card = fun slot card ->
  (2, card, slot)

let print_commands = fun commands ->
  print_endline (String.concat "\n" (List.rev (List.map print_play commands)))

let reset_slot = fun slot ->
  (apply_card_to_slot "put" slot)

let rec create_number slot now goal =
  if (n <= 0 || now > goal) then
    (apply_slot_to_card slot "zero")::(reset_slot slot)::[]
  else
    if (choose_succ_dbl now goal = "succ") then
      (apply_card_to_slot "succ" slot)::(create_number slot (n-1))
    else
      (apply_card_to_slot "succ" slot)::(create_number slot (n-1))

let add_arg = fun slot n ->
  (apply_slot_to_card slot "zero")::
  (apply_slot_to_card slot "succ")::
  (apply_card_to_slot "S" slot)::
  (apply_card_to_slot "K" slot)::
  (apply_slot_to_card slot "get")::
  (apply_card_to_slot "S" slot)::
  (apply_card_to_slot "K" slot)::
    (create_number 1 n)

let attack = fun i j n ->
  let slot = 10 in
  let cmd0 = (create_number slot i)@[(reset_slot slot)] in
  let cmd1 = (apply_card_to_slot "attack" slot) :: cmd0 in
  let cmd2 = (add_arg slot j) @ cmd1 in
    (add_arg slot n) @ cmd2;;

let help = fun i j n ->
  let slot = 10 in
  let cmd0 = (create_number slot i)@[(reset_slot slot)] in
  let cmd1 = (apply_card_to_slot "help" slot) :: cmd0 in
  let cmd2 = (add_arg slot j) @ cmd1 in
    (add_arg slot n) @ cmd2;;

# higher goal
let kill = fun i ->
  match oponent.(i) with
    | _, v -> 

let get_commands = fun strategy ->
  match strategy with
    | PrepareInt(i) -> (create_number 10 i @[(reset_slot 10)]
    | Attack(i,j,n) -> (attack i j n)
    | Help(i,j,n) -> (help i j n)
    | Zombie(i) -> ()
    | Kill(i) -> (kill i)
    | ApplyIntArg


