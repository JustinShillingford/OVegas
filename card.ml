type suit = Heart | Club | Diamond | Spade

type rank = int

type card = rank * suit

let init_card (r : rank) (s : suit) =
  (r, s)

let rep_ok c =
  match c with
  | (r, s) -> begin
    if ((r >= 1 && r <= 13)
    && (s = Heart || s = Club || s = Diamond || s = Spade)) then true else false
  end

let string_of_card c =
  match c with
  | (r, s) ->