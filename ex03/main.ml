let get_1_2 (a,_) = a
let get_2_2 (_,a) = a

let () =
    let n_deck = Deck.newDeck in
    let s_deck = Deck.toStringList in
    let v_deck = Deck.toStringListVerbose in
    let rec deckreader deck_v =
        match deck_v with
        |head::tail -> print_string deck_v;print_char ' '; deckreader tail
        |[] -> print_endline "end"
    in deckreader v_deck;
    deckreader s_deck;
    print_string Deck.Card.toStringVerbose (get_1_2 (Deck.drawCard n_deck));
    print_string Deck.Card.Color (get_1_2 (Deck.drawcard Deck.newCard))


