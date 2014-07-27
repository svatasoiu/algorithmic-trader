open Core.Std;;
open Async.Std;;

let fields = ["Open";"Close"];;

let gd = Scraper.BasicScraper.get_hist_data fields "YHOO" "2010-05-01" "2010-05-10"
				 >>| Scraper.BasicScraper.print_hist_data;;

Scraper.print_vals ("Ticker"::"Date"::fields) (fun _ field -> print_string ((Scraper.shorten field 7) ^ "\t\t|\t"));;

print_string "\n";;

gd >>> fun _-> shutdown 0;;

let () = never_returns (Scheduler.go ())
