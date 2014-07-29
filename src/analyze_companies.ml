open Core.Std;;
open Async.Std;;
open Analyzer;;

module Mov = MovingAverageAnalyzer(Scraper.BasicScraper)

let create_date_range i = 
	((Int.to_string i) ^ "-01-01",
	(Int.to_string i) ^ "-12-31");;

let print_res year comp p inv =
	print_string ((Int.to_string year) ^ "["^comp^"]: $" ^ (Float.to_string p) ^ " on " ^ (Float.to_string inv) ^ " @ " ^ (Printf.sprintf "%.2f" (100. *. p /. inv)) ^ "% return\n")

let analyze_company comp start range =
	for i = 0 to range do
		(let (s,e) = create_date_range (start + i) in
		(Scraper.BasicScraper.get_hist_data ["Close"] comp s e) 
		>>| Mov.create_buy_sell_stream
		>>| (fun (closes,_,act) -> ((eval_strategy closes act), List.hd_exn closes))
		>>> fun (profit,c) -> (print_res (start+i) comp profit c))
	done;;

analyze_company "YHOO" 1997 16;;
analyze_company "MSFT" 1987 26;;
analyze_company "CMG" 2007 6;;

let () = Core.Std.never_returns (Scheduler.go ())
