open Card

type hand = {card1: card; card2: card}

type player = {id: string; cards: hand; money: int}

(*need to write make_hand deck*)
let init_player name deck =
  {id = name; hand = (make_hand deck) ; money=100}
