open Genlex;;
open Printf;;
let lexer = make_lexer ["+";"-";"*";"/";"(";")"];;

let replace input output =
    Str.global_replace (Str.regexp_string input) output;;

let rec calc = parser
    [< 'Int a >] -> a
  | [< 'Kwd "("; e = calc; 'Kwd ")" >] -> e
  | [< 'Kwd "+"; e1 = calc; e2 = calc >] -> e1 + e2
  | [< 'Kwd "-"; e1 = calc; e2 = calc >] -> e1 - e2
  | [< 'Kwd "*"; e1 = calc; e2 = calc >] -> e1 * e2
  | [< 'Kwd "/"; e1 = calc; e2 = calc >] -> e1 / e2;;

(*let () =
  let expr = Sys.argv.(1) in
  printf "%d\n" (calc (lexer (Stream.of_string expr)));
;;*)