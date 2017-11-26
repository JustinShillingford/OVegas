(*hand , table -> command*)
(*hand ==> card list*)
(*table ==> card list*)
(*decide with 0/3/4/5 cards*)
(*decide with 2/5/6/7 cards*)

let card_rank_compare (r1,s1) (r2,s2) =
  Pervasives.compare r1 r2

let sort_by_ranking cards =
  List.sort card_rank_compare cards

let get_ranks cards =
  let rec get_ranks_helper cards acc =
    match cards with
    | [] -> acc
    | (r1,_)::t -> get_ranks_helper t (r1::acc)
  in get_ranks_helper cards []

let get_suits cards =
  let rec get_suits_helper cards acc =
    match cards with
    | [] -> acc
    | (_,s1)::t -> get_ranks_helper t (s1::acc)
  in get_suits_helper cards []

let ai_has_pair cards =
  let sorted_cards = sort_by_ranking cards in
  let sorted_ranks = get_ranks sorted_cards in
  let rec has_pair_helper sorted_ranks =
    match sorted_ranks with
    | r1::r2::t ->
      if r1=r2 then true else has_pair_helper (r2::t)
    | _ -> false
  in has_pair_helper sorted_ranks

let ai_has_three_kind cards =
  if (List.length cards)<3 then ai_has_pair cards
  else
    let sorted_cards = sort_by_ranking cards in
    let sorted_ranks = get_ranks sorted_cards in
    let rec has_three_kind_helper sorted_ranks =
      match sorted_ranks with
      | r1::r2::r3::t ->
        if r1=r2 && r2=r3 then true else has_three_kind_helper (r2::r3::t)
      | _ -> false
    in has_three_kind_helper sorted_ranks

let ai_has_four_kind cards =
  if (List.length cards)<4 then ai_has_three_kind cards
  else
    let sorted_cards = sort_by_ranking cards in
    let sorted_ranks = get_ranks sorted_cards in
    let rec has_four_kind_helper sorted_ranks =
      match sorted_ranks with
      | r1::r2::r3::r4::t ->
        if r1=r2 && r2=r3 && r3=r4 then true else has_four_kind_helper (r2::r3::r4::t)
      | _ -> false
    in has_four_kind_helper sorted_ranks

let rec straight sorted_ranks =
    match sorted_ranks with
    | r1::r2::t ->
      if r1+1=r2 then straight (r2::t) else false
    | _ -> true

let ai_has_straight cards =
  let sorted_cards = sort_by_ranking cards in
  let sorted_ranks = get_ranks sorted_cards in
  match sorted_ranks with
  | [r1;r2;r3;r4;r5;r6] ->
    straight [r1;r2;r3;r4;r5] || straight [r2;r3;r4;r5;r6]
  | [r1;r2;r3;r4;r5;r6;r7] ->
    straight [r1;r2;r3;r4;r5] || straight [r2;r3;r4;r5;r6] || straight [r3;r4;r5;r6;r7]
  | _ -> straight sorted_ranks





