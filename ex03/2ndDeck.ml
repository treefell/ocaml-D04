let  newDeck () =
    let l_card = Deck.Card.all in
    Random.self_init ();
    let rec shuffle l_card a_card b_card =
        match l_card with
            |[] -> a_card @ b_card
            |head:: tail -> (let p = Random.int 2 in
                            match p with
                                |0 -> shuffle tail (head :: a_card) b_card
                                |1 -> shuffle tail a_card (head :: b_card)
                            )
    in
    shuffle l_card [] []

let toStringList l_card =
    let rec stringify_list l_card s_card =
        match l_card with
            |head :: tail -> stringify_list l_card (s_card @ [Deck.Card.toString head])
            |[] -> s_card
    in
    stringify_list l_card []

let toStringListVerbose l_card =
    let rec stringify_listVerb l_card s_card =
        match l_card with
            |head :: tail -> stringify_listVerb l_card (s_card @ [Deck.Card.toStringVerbose head])
            |[] -> s_card
    in 
    stringify_listVerb l_card []


let drawCard d_card =
            match d_card with
            |head::tail -> (head, tail)
            |[] -> invalid_arg "empty deck"

