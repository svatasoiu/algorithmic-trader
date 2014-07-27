open Core.Std;;
open Async.Std;;

type portfolio = (string * float) list
let portfolio_to_string p = 
	List.fold 
		p 
		~init:"" 
		~f:(fun a (s,f) -> s ^ ": " ^ (Float.to_string f) ^ ", " ^ a)

let empty_portfolio = []
let add_to_portfolio p s f = (s, f)::p 

module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
			type data = Scr.data
			type hist_data = Scr.hist_data
			val analyze : data -> portfolio
		end;;

module EmptyAnalyzer : ANALYZER =  
	functor (Scr : Scraper.SCRAPER) ->
		struct 
			type data = Scr.data
			type hist_data = Scr.hist_data
			let analyze d = []
		end;;

module EqualAnalyzer : ANALYZER =  
	functor (Scr : Scraper.SCRAPER) ->
		struct 		
			type data = Scr.data
			type hist_data = Scr.hist_data

			let analyze d = 
				let tickers = Scr.extract_tickers d in
				let prop = 1. /. Float.of_int(List.length tickers) in
				List.map tickers (fun t -> (t, prop))
		end;;

module MovingAverageAnalyzer : ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		struct 		
			type data = Scr.data
			type hist_data = Scr.hist_data

			let analyze d = 
				let tickers = Scr.extract_tickers d in
				let prop = 1. /. Float.of_int(List.length tickers) in
				List.map tickers (fun t -> (t, prop))
		end;;
