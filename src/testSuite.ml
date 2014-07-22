open Core.Std;;
open Async.Std;;

exception EmptyRef;;

let numPassed = ref 0
let numFailed = ref 0
let numExcept = ref 0
let test_suite = ref []

let get_option = function None -> raise EmptyRef 
												| Some v -> v

let empty_ref = fun _ -> ref None

let add_test_case_to_suite tc = test_suite := tc::!test_suite

let print_results = 
	fun _ -> Printf.printf "%d tests passed/%d tests failed/%d exceptions\n" !numPassed !numFailed !numExcept

let handle_test_result test_id printer act exp t = 
	try print_string ((if t 
										 then (incr numPassed; "[SUCC]: (") 
										 else (incr numFailed; "[FAIL]: " ^ (printer (get_option !act)) ^ " not comp to " ^ (printer exp) ^ " (")) ^ test_id ^ ")\n")
	with _ -> incr numExcept

(* appends an async test case to test_suite
	 that checks the result of (comp res (f arg)) 
	 with the proper test_id *)
let assert_test comp act exp test_id printer = 
	let p = empty_ref () in
	try add_test_case_to_suite (act 
														>>| (fun t -> p := Some t; comp t exp)
														>>| handle_test_result test_id printer p exp)
	with EmptyRef -> (incr numExcept; print_string ("[EXCP]: " ^ test_id ^ "\n"))

(* appends an async test case to test_suite
	 that checks the equality of res and (f arg) 
	 with the proper test_id *)
let assert_eq act exp test_id printer = assert_test (=) act exp test_id printer

(* runs all tests in test_suite and then shutsdown the scheduler *)
let run_suite = 
	fun _ -> 
		(match !test_suite with
				f::xs -> (List.fold_left 
										xs 
										~init:f	
										~f:(fun a x -> a >>= fun _ -> x)) 
			| [] -> return ()) 
		>>> fun _ -> print_results (); shutdown 0
