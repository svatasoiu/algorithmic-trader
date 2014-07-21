open Core.Std;;
open Async.Std;;

module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
			type data = Scr.data
			type portfolio

			val empty_portfolio : portfolio
			val analyze : data -> portfolio
		end;;

module EmptyAnalyzer : ANALYZER =  
	functor (Scr : Scraper.SCRAPER) ->
		struct 
			type data = Scr.data
			type portfolio = (string * float) list
		
			let empty_portfolio = []
			let analyze d = []
		end;;
