open Core.Std;;
open Async.Std;;

let fields = ["Ask";"Open";"PreviousClose";"ChangeinPercent";"Bid";"EBITDA"];;

let gd = 
	fun _ -> Scraper.get_data fields ["GOOG";"MSFT";"STT";"BAC"]
				>>| function None -> printf "Nothing reported!"
									 | Some l -> 
										 List.fold l ~init:() ~f:(fun _ (ticker, vals) -> print_string ticker;
											 List.fold vals ~init:() ~f:(fun _ (_, v) -> print_string ("\t \t" ^ v)); print_string "\n");;

List.fold 
	("Ticker"::fields) 
	~init:() 
	~f:(fun _ field -> print_string (field ^ "\t|\t"));;

print_string "\n";;

let rec loop = 
	fun _ -> 
		after (sec 3.)
		>>= gd
		>>= loop;;

loop ();;

let () = Core.Std.never_returns (Async.Std.Scheduler.go ())
