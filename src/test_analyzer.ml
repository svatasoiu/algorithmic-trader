open Core.Std;;
open Async.Std;;
open TestSuite;;

module A = Analyzer.EmptyAnalyzer(Scraper.BasicScraper)

let _ = 
	createEqTestCase 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"])
		A.analyze
		A.empty_portfolio
		"Empty Analyzer";;

let _ = 
	createEqTestCase 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"])
		A.analyze
		A.empty_portfolio
		"Empty Analyzer 2";;

let _ = 
	createEqTestCase 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"])
		A.analyze
		(A.add_to_portfolio A.empty_portfolio "A" 0.5)
		"Empty Analyzer fail";;

run_suite ();;

let () = never_returns (Scheduler.go ())
