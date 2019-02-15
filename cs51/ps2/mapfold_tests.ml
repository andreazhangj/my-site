(* 
			 CS 51 Problem Set 2
	    Higher Order Functional Programming -- Testing
 *)

open Mapfold ;;

open Test_simple ;;      (* a really simple unit testing framework *)
  
let test () =
  unit_test ((negate_all []) = []) "negate_all empty";
  unit_test ((negate_all [1; -2; 0]) = [-1; 2; 0]) "negate_all mixed";
  unit_test ((negate_all [1; 2; 3]) = [1; 2; 3]) "negate_all positive";

  unit_test ((sum []) = 0) "sum empty";
  unit_test ((sum [4]) = 4) "sum single";
  unit_test ((sum [1, 3, 5]) = 9) "sum positive";
  unit_test ((sum [-1, 3, 5]) = 7) "sum mixed";

  unit_test ((sum_rows []) = 0) "sum_rows empty";
  unit_test ((sum_rows [[]]) = 0) "sum_rows more empty";
  unit_test ((sum_rows [[1], [3; 7]]) = [1; 10]) "sum_rows single/multiple";

  unit_test ((filter_odd []) = []) "filter_odd empty";
  unit_test ((filter_odd [1; 4; 5; -3]) = [1; 5; -3]) "filter_odd pos/neg";
  
  unit_test ((num_occurs 3 []) = 0) "num_occurs empty";
  unit_test ((num_occurs 3 [3]) = 1) "num_occurs one";
  unit_test ((num_occurs 3 [1; 3; 5; 4; 3; 3]) = 3) "num_occurs multiple";
  
  unit_test ((super_sum []) = 0) "super_sum empty";
  unit_test ((super_sum [[1; 2; 3]; []; [-5]]) = 1) "super_sum mixed";

  unit_test ((filter_range [] (1, 3)) = []) "filter_range empty";
  unit_test ((filter_range [1; 3; 4; 5; 2; -2] (1, 3)) = [1; 3; 2]) "filter_range normal";
  unit_test ((filter_range [1; 3; 4; 5; 2; -2] (1, 0)) = []) "filter_range flipped";

  unit_test ((floats_of_ints []) = []) "floats_of_ints empty";
  unit_test ((floats_of_ints [1; 0; -4; 5]) = [1.; 0.; -4.; 5.]) "floats_of_ints mixed";

  unit_test ((log10s []) = []) "logs10s empty";
  unit_test ((log10s [1.0; 10.0; -10.0]) = [Some 0.; Some 1.; None]) "logs10s mixed";

  unit_test ((deoptionalize [])) = []) "deoptionalize empty";
  unit_test ((deoptionalize [Some 3; None; Some 5; Some -10])) = [3; 5; -10]) "deoptionalize mixed";
	
  unit_test ((some_sum [])) = 0) "some_sum empty";
  unit_test ((some_sum [Some 3; None; Some 5; Some -10])) = -2) "some_sum mixed";

  unit_test ((mult_odds [])) = 1) "mult_odds empty";
  unit_test ((mult_odds [1; 3; 0; 2; -5])) = -15) "mult_odds mixed";

  unit_test ((concat [])) = []) "concat empty";
  unit_test ((concat [[1; 2]; []; [3; 4; 5]; [6]])) = [1; 2; 3; 4; 5; 6]) "concat normal";

  unit_test ((filter_by_year [] 2000 = []) "filter_by_year empty";
  unit_test ((filter_by_year [("Joe", 2010); ("Bob", 2010); ("Tom", 2013)] 2010
  	= ["Joe"; "Bob"]) "filter_by_year normal";

  () ;;

test ();;
