open Genlex
open Printf

let lexer = make_lexer ["+";"-";"*";"/";"(";")";"[";"]";"let"]

let replace input output =
    Str.global_replace (Str.regexp_string input) output

let env0 = []

let lookup k env =
  if List.mem_assoc k env
  then Some (List.assoc k env)
  else None

let extEnv k v env = (k, v)::env 

let tryGet v = 
  match v with 
  | Some x -> x
  | None -> 42 

let rec interp env = parser 
    [< 'Int a >] -> Some a
  | [< 'Ident a >] -> lookup a env 
  | [< 'Kwd "("; e = interp env; 'Kwd ")" >] -> e
  | [< 'Kwd "+"; e1 = interp env; e2 = interp env >] -> Some ((tryGet e1) + (tryGet e2))
  | [< 'Kwd "-"; e1 = interp env; e2 = interp env >] -> Some ((tryGet e1) - (tryGet e2))
  | [< 'Kwd "*"; e1 = interp env; e2 = interp env >] -> Some ((tryGet e1) * (tryGet e2))
  | [< 'Kwd "/"; e1 = interp env; e2 = interp env >] -> Some ((tryGet e1) / (tryGet e2))
  | [< 'Kwd "let"; 'Kwd "["; 'Ident a; e1 = interp env; 'Kwd "]"; e2 = interp (extEnv a (tryGet e1) env) >]
             -> e2;;

(*let () =
  let expr = Sys.argv.(1) in
  printf "%d\n" (interp env (lexer (Stream.of_string expr)));
;;*)
