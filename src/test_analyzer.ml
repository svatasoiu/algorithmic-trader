open Core.Std;;
open Async.Std;;

module A = Analyzer.EmptyAnalyzer(Scraper.BasicScraper)

let test_empty = 
	(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"]) 
	>>| A.analyze
	>>| (=) A.empty_portfolio
	>>| fun t -> print_string (if t then "[SUCC]: Empty Analyzer" else "[FAIL]: Empty Analyzer");;

let () = never_returns (Scheduler.go ())
