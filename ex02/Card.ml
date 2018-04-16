module Color =
    struct
        type t = Spade | Heart | Diamond | Club

             let all = [Spade; Heart; Diamond; Club]

            let toString card =
                match card with
                    |Spade -> "S"
                    |Heart -> "H"
                    |Diamond ->"D"
                    |Club -> "C"

            let toStringVerbose card =
                match card with
                    |Spade -> "Spade"
                    |Heart -> "Heart"
                    |Diamond ->"Diamond"
                    |Club -> "Club"
    end

    module Value =
        struct

            type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

            let all =  [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As] 

            let toInt card =
                match card with 
        | T2    -> 1
        | T3    -> 2
        | T4    -> 3
        | T5    -> 4
        | T6    -> 5
        | T7    -> 6
        | T8    -> 7
        | T9    -> 8
        | T10   -> 9
        | Jack  -> 10
        | Queen -> 11
        | King  -> 12
        | As    -> 13

            let toStringVerbose card =
                match card with 
        | T2    -> "2"
        | T3    -> "3"
        | T4    -> "4"
        | T5    -> "5"
        | T6    -> "6"
        | T7    -> "7"
        | T8    -> "8"
        | T9    -> "9"
        | T10   -> "10"
        | Jack  -> "Jack"
        | Queen -> "Queen"
        | King  -> "King"
        | As    -> "As"

            let toString card =
                match card with 
        | T2    -> "2"
        | T3    -> "3"
        | T4    -> "4"
        | T5    -> "5"
        | T6    -> "6"
        | T7    -> "7"
        | T8    -> "8"
        | T9    -> "9"
        | T10   -> "10"
        | Jack  -> "J"
        | Queen -> "Q"
        | King  -> "K"
        | As    -> "A"
    let next card =
        match card with 
            | T2    -> T3
            | T3    -> T4
            | T4    -> T5
            | T5    -> T6
            | T6    -> T7
            | T7    -> T8
            | T8    -> T9
            | T9    -> T10
            | T10   -> Jack
            | Jack  -> Queen
            | Queen -> King
            | King  -> As
            | As    -> invalid_arg "invalid"

    let previous card =
        match card with 
            | T2    -> invalid_arg "invalid"
            | T3    -> T2
            | T4    -> T3
            | T5    -> T4
            | T6    -> T5
            | T7    -> T6
            | T8    -> T7
            | T9    -> T8
            | T10   -> T9
            | Jack  -> T10
            | Queen -> Jack
            | King  -> Queen
            | As    -> King
    end

            type t = {value:Value.t ; color:Color.t}

            let newCard c_value c_color =
                {value = c_value; color = c_color}

            let allSpades  = 
                [newCard Value.T2  Color.Spade;
                newCard Value.T3  Color.Spade;
                newCard Value.T4  Color.Spade;
                newCard Value.T5  Color.Spade;
                newCard Value.T6  Color.Spade;
                newCard Value.T7  Color.Spade;
                newCard Value.T8  Color.Spade;
                newCard Value.T9  Color.Spade;
                newCard Value.T10  Color.Spade;
                newCard Value.Jack Color.Spade;
                newCard Value.Queen  Color.Spade; 
                newCard Value.King  Color.Spade;
                newCard Value.As  Color.Spade]

            let allHearts  = 
                [newCard Value.T2  Color.Heart;
                newCard Value.T3  Color.Heart;
                newCard Value.T4  Color.Heart;
                newCard Value.T5  Color.Heart;
                newCard Value.T6  Color.Heart;
                newCard Value.T7  Color.Heart;
                newCard Value.T8  Color.Heart;
                newCard Value.T9  Color.Heart;
                newCard Value.T10  Color.Heart;
                newCard Value.Jack Color.Heart;
                newCard Value.Queen  Color.Heart; 
                newCard Value.King  Color.Heart;
                newCard Value.As  Color.Heart]

            let allDiamonds  = 
                [newCard Value.T2  Color.Diamond;
                newCard Value.T3  Color.Diamond;
                newCard Value.T4  Color.Diamond;
                newCard Value.T5  Color.Diamond;
                newCard Value.T6  Color.Diamond;
                newCard Value.T7  Color.Diamond;
                newCard Value.T8  Color.Diamond;
                newCard Value.T9  Color.Diamond;
                newCard Value.T10  Color.Diamond;
                newCard Value.Jack Color.Diamond;
                newCard Value.Queen  Color.Diamond; 
                newCard Value.King  Color.Diamond;
                newCard Value.As  Color.Diamond]

            let allClubs  = 
                [newCard Value.T2  Color.Club;
                newCard Value.T3  Color.Club;
                newCard Value.T4  Color.Club;
                newCard Value.T5  Color.Club;
                newCard Value.T6  Color.Club;
                newCard Value.T7  Color.Club;
                newCard Value.T8  Color.Club;
                newCard Value.T9  Color.Club;
                newCard Value.T10  Color.Club;
                newCard Value.Jack Color.Club;
                newCard Value.Queen  Color.Club; 
                newCard Value.King  Color.Club;
                newCard Value.As  Color.Club]

            let all = 
             allSpades @ allHearts @ allDiamonds @ allClubs

            let getValue card =
                card.value

            let getColor card =
                card.color

            let toString t =
                (Value.toString t.value) ^ (Color.toString t.color)

            let toStringVerbose t =
                "Card("^ (Value.toStringVerbose t.value) ^ "," ^ (Color.toStringVerbose t.color)^")"

            let compare a b =
                (Value.toInt a.value) - (Value.toInt b.value) 

            let max a b =
                if b.value > a.value
                    then b
                else a

            let min a b =
                if a.value > b.value
                    then b
                else a

    (* List.fold_left*)
            let best l_t =
                let rec rec_best l_t card =
                    match l_t with
            |[] -> card
            |head::tail -> rec_best tail (max card head)
                in
    match l_t with
        |[] -> invalid_arg "empty list"
        |head::tail -> rec_best l_t head

            let isOf  card c_color =
                if card.color = c_color
        then true
    else false

            let isSpade card =
                if card.color = Color.Spade
        then true
    else false

            let isHeart card =
                if card.color = Color.Heart
        then true
    else false

            let isDiamond card =
                if card.color = Color.Diamond
        then true
    else false

            let isClub card =
                if card.color = Club
        then true
    else false
