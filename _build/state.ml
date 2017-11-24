open Card
open Table
open Player
open Command

type state = {players: player list; round: int; pot:int; table: table}
let initial_state player_list = 
  let d = shuffle (new_deck ()) in
  {players = player_list; round = 0; pot = 0; table = (d, None)}  

let string_of_state st = 
  failwith "Unimplemented"

let do' st p c=
  failwith "Unimplemented"