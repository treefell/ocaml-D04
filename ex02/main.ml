let () =
    let l_card = 
(*        [Card.newCard Card.Value.T2 Card.Color.Spade;
        Card.newCard Card.Value.T7 Card.Color.Club;
        Card.newCard Card.Value.T5 Card.Color.Heart;
        Card.newCard Card.Value.T2 Card.Color.Diamond;
        Card.newCard Card.Value.As Card.Color.Spade;
        Card.newCard Card.Value.King Card.Color.Spade;
        Card.newCard Card.Value.T5 Card.Color.Spade;
        Card.newCard Card.Value.King Card.Color.Heart;
        Card.newCard Card.Value.Jack Card.Color.Club;
        Card.newCard Card.Value.T4 Card.Color.Diamond;
        Card.newCard Card.Value.T6 Card.Color.Spade;
        Card.newCard Card.Value.T9 Card.Color.Club;
        Card.newCard Card.Value.T2 Card.Color.Heart;
        Card.newCard Card.Value.T8 Card.Color.Spade;
        Card.newCard Card.Value.T6 Card.Color.Heart]*)
        Card.all in
      let rec loop l_c = 
        match l_c with 
            |[] -> print_endline "over"
            |head :: tail -> print_string (Card.Value.toString (Card.getValue head)); print_string " | ";
                             print_string (Card.Color.toString (Card.getColor head));  print_string " | ";
                             print_string (Card.toString head); print_string " | ";
                             print_string (Card.toStringVerbose head); print_string "\n";
                             loop tail
        in
        loop l_card;
        print_string (Card.toString (Card.best l_card));
        print_char '\n';
        print_endline "compare 2 of spade to 7 of spade and compare 2 of spade with 2 of heart";
        print_int (Card.compare (List.nth l_card 0) (List.nth l_card 6));
        print_string " | ";
        print_int (Card.compare (List.nth l_card 0) (List.nth l_card 13));
        print_string "\n";
        print_string (Card.toString(Card.min (List.nth l_card 0) (List.nth l_card 6)));
        print_string " | ";
        print_string (Card.toString(Card.min (List.nth l_card 0) (List.nth l_card 13)));
        print_string "\n";
        print_string (Card.toString(Card.max (List.nth l_card 0) (List.nth l_card 6)));
        print_string " | ";
        print_string (Card.toString(Card.max (List.nth l_card 0) (List.nth l_card 13)));
        print_char '\n';
        print_string (string_of_bool(Card.isOf (List.nth l_card 0) Card.Color.Spade)); print_char '\n';
        print_string (string_of_bool(Card.isSpade (List.nth l_card 0))); print_string " | ";
        print_string (string_of_bool(Card.isHeart (List.nth l_card 0))); print_string " | ";
        print_string (string_of_bool(Card.isDiamond (List.nth l_card 0))); print_string " | ";
        print_string (string_of_bool(Card.isClub (List.nth l_card 0))); print_char '\n'

