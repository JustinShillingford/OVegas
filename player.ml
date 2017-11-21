open Card

type hand = card list

type player = {id: string; two_cards: hand; money: int}

(*need to write make_hand deck*)
let init_player name deck =
  {id = name; hand = (make_hand deck) ; money=100}
