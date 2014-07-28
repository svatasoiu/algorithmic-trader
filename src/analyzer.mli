type portfolio = (string * float) list
val portfolio_to_string : portfolio -> string
val empty_portfolio : portfolio
val add_to_portfolio : portfolio -> string -> float -> portfolio

module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
		  type data = Scr.data
			type hist_data = Scr.hist_data
			val analyze : data -> portfolio
			val analyze_hist : hist_data -> portfolio
		end;;

module EmptyAnalyzer : ANALYZER;;
module EqualAnalyzer : ANALYZER;;
module MovingAverageAnalyzer : ANALYZER;;
