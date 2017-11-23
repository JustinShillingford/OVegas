open Table
open Player
open Command

type state = {players: player list; round: int; pot:int; table: table }
let initial_state player_list = 
  failwith "Unimplemented"  

let string_of_state st = 
  failwith "Unimplemented"

let do' st p c=
  failwith "Unimplemented"