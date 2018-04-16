let print_value card =
    print_string (Value.toString card)


let () =
    let cards = [Value.T2 ; Value.T3 ; Value.T4 ; Value.T5 ; Value.T6 ;
                 Value.T7 ; Value.T8 ; Value.T9 ; Value.T10 ; Value.Jack ;
                 Value.Queen ; Value.King ; Value.As] in
(*    let rec loop funk ?(funk2=print_string) l_card =
        match l_card with
            |[] -> print_endline "over"
            |head::tail ->  funk2 (funk head);print_char ' '; loop funk funk2 tail
    in
    loop Value.toString cards;
    loop Value.toStringVerbose cards;
    loop Value.toInt ~funk2:print_int cards;
    loop Value.next ~funk2:print_value cards;
    loop Value.previous ~funk2:print_value cards*)
 let rec loop funk_str l_card =
        match l_card with
            |[] -> print_endline "over"
            |head::tail ->  print_string (funk_str head);
            print_char ' '; loop funk_str tail
    in
    loop Value.toString cards;
    loop Value.toStringVerbose cards;

let rec loop_int funk_int l_card =
        match l_card with
            |[] -> print_endline "over"
            |head::tail -> print_int (funk_int head);print_char ' ';
            loop_int funk_int tail
    in
    loop_int Value.toInt cards;

let rec loop_value funk_val l_card =
        match l_card with
            |[] -> print_endline "over"
            |head::tail -> print_value (funk_val head); 
            print_char ' ';
            loop_value funk_val tail
    in
    (*loop_value Value.next cards;*)
    loop_value Value.previous cards

