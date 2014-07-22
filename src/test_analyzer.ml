open Async.Std;;
open TestSuite;;

module A = Analyzer.EmptyAnalyzer(Scraper.BasicScraper)
module E = Analyzer.EqualAnalyzer(Scraper.BasicScraper)

let _ = 
	assert_eq 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"] >>| A.analyze)
		A.empty_portfolio
		"Empty Analyzer"
		A.portfolio_to_string;;

let _ = 
	assert_eq 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"] >>| A.analyze)
		A.empty_portfolio
		"Empty Analyzer 2"
		A.portfolio_to_string;;

let _ = 
	assert_eq 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"] >>| A.analyze)
		(A.add_to_portfolio A.empty_portfolio "A" 0.5)
		"Empty Analyzer fail"
		A.portfolio_to_string;;

let _ =
	assert_eq 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"] >>| E.analyze)
		(E.add_to_portfolio E.empty_portfolio "GOOG" 1.0)
		"Equal Analyzer 1 stock"
		E.portfolio_to_string;;

let _ =
	assert_eq 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG";"MSFT"] >>| E.analyze)
		[("GOOG",0.5);("MSFT",0.5)]
		"Equal Analyzer 2 stocks"
		E.portfolio_to_string;;

run_suite ();;

let () = Core.Std.never_returns (Async.Std.Scheduler.go ())
