open Core.Std;;
open Async.Std;;

module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
			type data = Scr.data
			type portfolio = (string * float) list

			val empty_portfolio : portfolio
			val add_to_portfolio : portfolio -> string -> float -> portfolio
			val analyze : data -> portfolio
		end;;

module EmptyAnalyzer : ANALYZER =  
	functor (Scr : Scraper.SCRAPER) ->
		struct 
			type data = Scr.data
			type portfolio = (string * float) list
		
			let empty_portfolio = []
			let add_to_portfolio p s f = (s, f)::p 
			let analyze d = []
		end;;
