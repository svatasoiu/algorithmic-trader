open Core.Std;;
open Async.Std;;

exception EmptyRef;;

let start_time = ref 0.
let first = ref false
let numPassed = ref 0
let numFailed = ref 0
let numExcept = ref 0
let test_suite = ref []

let get_option = function None -> raise EmptyRef 
												| Some v -> v

let empty_ref = fun _ -> ref None

let add_test_case_to_suite tc = test_suite := tc::!test_suite

let print_results = 
	fun _ -> Printf.printf "\027[32m%d tests passed/%d tests failed/%d exceptions in %fs\n" 
												 !numPassed !numFailed !numExcept (Time.to_float (Time.now ()) -. !start_time)

let handle_test_result test_id printer act exp t = 
	try print_string ((if t 
										 then (incr numPassed; "\027[32m[SUCC]: (") 
										 else (incr numFailed; "\027[31m[FAIL]: " ^ 
														match printer with
																None -> ""
															| Some printer -> ("Actual: {" ^ printer (get_option !act)) ^ "}, Expected: {" 
																							^ (printer exp) ^ "} (")) 
														^ test_id ^ ")\n")
	with _ -> (incr numExcept; print_string ("\027[31m[EXCP]: " ^ test_id ^ "\n"))

(* appends an async test case to test_suite
	 that checks the result of (comp res (f arg)) 
	 with the proper test_id *)
let assert_test comp act exp ?printer test_id = 
	let p = empty_ref () in
	if not !first then (first := true; start_time := Time.to_float (Time.now ()));
	try add_test_case_to_suite (act 
														>>| (fun t -> p := Some t; comp t exp)
														>>| handle_test_result test_id printer p exp)
	with _ -> (incr numExcept; print_string ("\027[31m[EXCP]: " ^ test_id ^ "\n"))
(* need to fix error handling (think try_with) *)


(* appends an async test case to test_suite
	 that checks the equality of res and (f arg) 
	 with the proper test_id *)
let assert_equal act exp ?printer test_id = assert_test (=) act exp ?printer:printer test_id

(* runs all tests in test_suite and then shuts down the scheduler *)
let run_suite = 
	fun _ -> 
		(match !test_suite with
				f::xs -> (List.fold_left 
										xs 
										~init:f	
										~f:(fun a x -> a >>= fun _ -> x)) 
			| [] -> return ()) 
		>>> fun _ -> (print_results (); shutdown (if (!numFailed = 0 && !numExcept = 0) then 0 else 1))
