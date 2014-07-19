open Core.Std;;
open Async.Std;;

let fields = ["Ask";"Open";"PreviousClose";"ChangeinPercent";"Bid";"EBITDA"];;

let shorten s = String.sub s 0 (min 7 (String.length s));;

let print_vals vals f = List.fold vals ~init:() ~f:f;;

let gd = 
	fun _ -> Scraper.get_data fields ["GOOG";"MSFT";"STT";"BAC"]
				>>| function None -> printf "Nothing reported!"
									 | Some l -> 
										 print_vals l (fun _ (ticker, vals) -> 
												print_string ticker;
											  print_vals vals (fun _ (_, v) -> print_string ("\t|\t" ^ (shorten v))); 
												print_string "\n");;

print_vals ("Ticker"::fields) (fun _ field -> print_string ((shorten field) ^ "\t|\t"));;

print_string "\n";;

let rec loop = 
	fun _ -> 
		after (sec 3.)
		>>= gd
		>>= loop;;

loop ();;

let () = Core.Std.never_returns (Async.Std.Scheduler.go ())
