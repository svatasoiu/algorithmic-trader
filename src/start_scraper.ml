open Core.Std;;
open Async.Std;;

let fields = ["Ask";"Open";"PreviousClose";"ChangeinPercent";"Bid";"EBITDA"];;

let gd = 
	fun _ -> Scraper.BasicScraper.get_data fields ["GOOG";"MSFT";"STT";"BAC"]
				>>| Scraper.BasicScraper.print_data;;

Scraper.print_vals ("Ticker"::fields) (fun _ field -> print_string ((Scraper.shorten field) ^ "\t|\t"));;

print_string "\n";;

let rec loop = 
	fun _ -> 
		after (sec 3.)
		>>= gd
		>>= loop;;

loop ();;

let () = never_returns (Scheduler.go ())
