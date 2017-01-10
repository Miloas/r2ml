open Genlex
open Printf

let lexer = make_lexer ["+";"-";"*";"/";"(";")";"[";"]";"let"]

let replace input output =
    Str.global_replace (Str.regexp_string input) output

let rec interp = parser 
    [< 'Int a >] -> a
  | [< 'Kwd "("; e = interp; 'Kwd ")" >] -> e
  | [< 'Kwd "+"; e1 = interp; e2 = interp >] -> e1 + e2
  | [< 'Kwd "-"; e1 = interp; e2 = interp >] -> e1 - e2
  | [< 'Kwd "*"; e1 = interp; e2 = interp >] -> e1 * e2
  | [< 'Kwd "/"; e1 = interp; e2 = interp >] -> e1 / e2;;

(*let () =
  let expr = Sys.argv.(1) in
  printf "%d\n" (interp (lexer (Stream.of_string expr)));
;;*)
