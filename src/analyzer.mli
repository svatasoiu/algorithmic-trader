module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
		  type data = Scr.data
		  type portfolio = (string * float) list

			val empty_portfolio : portfolio
			val add_to_portfolio : portfolio -> string -> float -> portfolio
			val analyze : data -> portfolio
		end;;

module EmptyAnalyzer : ANALYZER;;
