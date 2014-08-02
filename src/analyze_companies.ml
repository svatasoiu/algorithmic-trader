open Core.Std;;
open Async.Std;;
open Analyzer;;

module Mov = MovingAverageAnalyzer(Scraper.BasicScraper)

let print_res year comp p inv =
	print_string ((Int.to_string year) ^ "["^comp^"]: $" ^ (Float.to_string p) ^ " on " ^ (Float.to_string inv) ^ " @ " ^ (Printf.sprintf "%.2f" (100. *. p /. inv)) ^ "% return\n")

let analyze_company comp start_date end_date =
		Scraper.BasicScraper.get_hist_data ["Adj_Close"] comp start_date end_date 
		>>| Mov.create_buy_sell_stream
		>>| (fun (closes,_,act) -> ((eval_strategy closes act), List.hd_exn closes))
		>>> fun (profit,c) -> (print_res (1995) comp profit c);;

analyze_company "AAPL" "2000-02-02" "2011-07-11";;
analyze_company "MSFT" "2000-02-02" "2011-07-11";;
analyze_company "C" "2000-02-02" "2011-07-11";;
analyze_company "YHOO" "2000-02-02" "2011-07-11";;

let () = Core.Std.never_returns (Scheduler.go ())
