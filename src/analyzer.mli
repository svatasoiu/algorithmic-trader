type portfolio = (string * float) list
val portfolio_to_string : portfolio -> string
val empty_portfolio : portfolio
val add_to_portfolio : portfolio -> string -> float -> portfolio

module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
		  type data = Scr.data
			val analyze : data -> portfolio
		end;;

module EmptyAnalyzer : ANALYZER;;
module EqualAnalyzer : ANALYZER;;
