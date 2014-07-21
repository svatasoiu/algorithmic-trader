module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
		  type data = Scr.data
		  type portfolio

			val empty_portfolio : portfolio
			val analyze : data -> portfolio
		end;;

module EmptyAnalyzer : ANALYZER;;
