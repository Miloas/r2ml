open OUnit2

let f s = s |> (R2.replace "(*" "( *") |> Stream.of_string |> R2.lexer |> (R2.interp R2.env0) |> R2.tryGet

let addTest test_ctxt = assert_equal 3 (f "(+ 1 2)")
let minusTest test_ctxt = assert_equal 0 (f "(- 2 2)")
let multiTest test_ctxt = assert_equal 6 (f "(* 2 3)")
let diviTest test_ctxt = assert_equal 1 (f "(/ 2 2)")
let mixTest test_ctxt = assert_equal 1 (f "(/ (+ (* (- 2 3) (* 2 1)) 4) (+ 1 1))")

let letbindTest test_ctxt = assert_equal 6 (f "(let [x (+ 1 2)] (+ x 3))")

let suite =
"suite">:::
 ["add test">:: addTest;
  "minus test">:: minusTest;
  "multiply test">:: multiTest;
  "divide test">:: diviTest;
  "mix calculate test">:: mixTest;
  "let bind Test">:: letbindTest]
;;
