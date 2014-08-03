open Core.Std;;
open Async.Std;;
open Analyzer;;
open TestSuite;;

module A = EmptyAnalyzer(Scraper.BasicScraper)
module E = EqualAnalyzer(Scraper.BasicScraper)
module Mov = MovingAverageAnalyzer(Scraper.BasicScraper)

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
		(Scraper.BasicScraper.get_hist_data ["Adj_Close"] "MSFT" "2000-02-02" "2012-07-01" >>| Mov.analyze_hist)
		[("GOOG",0.4);("MSFT",0.5)]
		"Moving average Analyzer fail";;

run_suite ();;

let () = Core.Std.never_returns (Scheduler.go ())
