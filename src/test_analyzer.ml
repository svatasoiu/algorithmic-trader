open Async.Std;;
open Analyzer;;
open TestSuite;;

module A = EmptyAnalyzer(Scraper.BasicScraper)
module E = EqualAnalyzer(Scraper.BasicScraper)

let _ = 
	assert_equal 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"] >>| A.analyze)
		empty_portfolio
		"Empty Analyzer"
		~printer:portfolio_to_string;;

let _ = 
	assert_equal 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"] >>| A.analyze)
		empty_portfolio
		"Empty Analyzer 2"
		~printer:portfolio_to_string;;

let _ = 
	assert_equal 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"] >>| A.analyze)
		(add_to_portfolio empty_portfolio "A" 0.5)
		"Empty Analyzer fail"
		~printer:portfolio_to_string;;

let _ =
	assert_equal 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG"] >>| E.analyze)
		(add_to_portfolio empty_portfolio "GOOG" 1.0)
		"Equal Analyzer 1 stock"
		~printer:portfolio_to_string;;

let _ =
	assert_equal 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG";"MSFT"] >>| E.analyze)
		[("GOOG",0.5);("MSFT",0.5)]
		"Equal Analyzer 2 stocks"
		~printer:portfolio_to_string;;

let _ =
	assert_equal 
		(Scraper.BasicScraper.get_data ["Open";"PreviousClose";"ChangeinPercent"] ["GOOG";"MSFT"] >>| E.analyze)
		[("GOOG",0.4);("MSFT",0.5)]
		"Equal Analyzer 2 stocks fail"
		~printer:portfolio_to_string;;

run_suite ();;

let () = Core.Std.never_returns (Scheduler.go ())
