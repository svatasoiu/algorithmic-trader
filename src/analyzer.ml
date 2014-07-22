open Core.Std;;
open Async.Std;;

let pf_to_string p = 
	List.fold 
		p 
		~init:"" 
		~f:(fun a (s,f) -> s ^ ": " ^ (Float.to_string f) ^ ", " ^ a)

module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
			type data = Scr.data
			type portfolio = (string * float) list

			val empty_portfolio : portfolio
			val portfolio_to_string : portfolio -> string
			val add_to_portfolio : portfolio -> string -> float -> portfolio
			val analyze : data -> portfolio
		end;;

module EmptyAnalyzer : ANALYZER =  
	functor (Scr : Scraper.SCRAPER) ->
		struct 
			type data = Scr.data
			type portfolio = (string * float) list
		
			let empty_portfolio = []
			let portfolio_to_string = pf_to_string

			let add_to_portfolio p s f = (s, f)::p 
			let analyze d = []
		end;;

module EqualAnalyzer : ANALYZER =  
	functor (Scr : Scraper.SCRAPER) ->
		struct 		
			type data = Scr.data
			type portfolio = (string * float) list

			let empty_portfolio = []
			let portfolio_to_string = pf_to_string

			let add_to_portfolio p s f = (s, f)::p 
			let analyze d = 
				let tickers = Scr.extract_tickers d in
				let prop = 1. /. Float.of_int(List.length tickers) in
				List.map tickers (fun t -> (t, prop))
		end;;

