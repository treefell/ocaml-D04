
let () =
    let x = [Color.Spade; Color.Diamond; Color.Heart; Color.Club] in
    let rec loop card = 
        (match card with
            |[] -> print_endline "end"
            |head::tail -> (match head with 
                             |Color.Spade | Color.Club -> print_string (Color.toString head);print_char '\n'; loop tail
                             |Color.Diamond | Color.Heart -> print_string(Color.toStringVerbose head);print_char '\n'; loop tail
            )
        )
    in 
    print_string (Color.toString Color.Spade);
    print_char '\n';
    print_string (Color.toStringVerbose Color.Spade);
    print_char '\n';
    print_string (Color.toString Color.Heart);
    print_char '\n';
    print_string (Color.toStringVerbose Color.Heart);
    print_char '\n';
    loop x

