open Core.Std;;
open Async.Std;;
open Analyzer;;

module Mov = MovingAverageAnalyzer(Scraper.BasicScraper)

let print_res year comp p inv =
	print_string ((Int.to_string year) ^ "["^comp^"]: $" ^ (Float.to_string p) ^ " on " ^ (Float.to_string inv) ^ " @ " ^ (Printf.sprintf "%.2f" (100. *. p /. inv)) ^ "% return\n")

let analyze_company comp start_date end_date =
		Scraper.BasicScraper.get_hist_data ["Adj_Close"] comp start_date end_date 
		>>| Mov.create_buy_sell_stream
		>>| (fun (_,closes,_,act) -> ((eval_strategy closes act), List.hd_exn closes))
		>>> fun (profit,c) -> (print_res (1995) comp profit c);;

let tickers = ["X";"FDX";"VZ";"SPLS";"T";"PEP";"KO";"AAPL";"MSFT";"C";"YHOO";"DJIA"]
let _ = List.iter tickers (fun t -> analyze_company t "2013-01-01" "2014-07-01")

let () = Core.Std.never_returns (Scheduler.go ())
