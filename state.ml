open Card
open Table
open Player
open Command

let initial_state player_list deck =
  let d = deck in
  {players = player_list; round = 0; pot = 0; table = (d, None)}

let string_of_state st =
  failwith "Unimplemented"

let do' st p c=
  failwith "Unimplemented"