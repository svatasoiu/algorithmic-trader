open Core.Std;;
open Async.Std;;

let numPassed = ref 0
let numFailed = ref 0
let numExcept = ref 0
let test_suite = ref []

let addTestCaseToSuite tc = test_suite := tc::!test_suite

let print_results = 
	fun _ -> Printf.printf "%d tests passed/%d tests failed/%d exceptions\n" !numPassed !numFailed !numExcept

let handleTestResult status t = 
	return (try print_string ((if t 
														 then (incr numPassed; "[SUCC]: ") 
														 else (incr numFailed; "[FAIL]: ")) ^ status ^ "\n")
					with _ -> incr numExcept)

(* appends an async test case to test_suite
	 that checks the result of (comp res (f arg)) 
	 with the proper status *)
let createTestCase arg f comp res status = 
	addTestCaseToSuite (arg
										  >>| f 
										  >>| comp res 
										  >>= handleTestResult status)

(* appends an async test case to test_suite
	 that checks the equality of res and (f arg) 
	 with the proper status *)
let createEqTestCase arg f res status = createTestCase arg f (=) res status

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
