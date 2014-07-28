open Core.Std;;
open Async.Std;;

type circle = { x : int; y : int; radius : int; color : int }
type position = [`Long | `Stay | `Short]
type portfolio = (string * float) list

let portfolio_to_string p = 
	List.fold 
		p 
		~init:"" 
		~f:(fun a (s,f) -> s ^ ": " ^ (Float.to_string f) ^ ", " ^ a)

let position_compare a b =
	match compare a b with
		1  -> `Long
	| 0  -> `Stay
	| -1 -> `Short
	
let empty_portfolio = []
let add_to_portfolio p s f = (s, f)::p

let moving_average l n = 
	let arr = List.to_array l in
	List.map
		(Array.foldi arr ~init:[] 
			~f:(fun i a x -> 
				try 
					(Array.fold (Array.slice arr i (i + n)) ~init:0. ~f:(+.))::a 
				with _ -> a))
		(fun t -> t /. Float.of_int n)

let derivative l = []
let second_derivative l = []

module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
			type data = Scr.data
			type hist_data = Scr.hist_data

			val analyze : data -> portfolio
			val analyze_hist : hist_data -> portfolio
		end;;

module EmptyAnalyzer : ANALYZER =  
	functor (Scr : Scraper.SCRAPER) ->
		struct 
			type data = Scr.data
			type hist_data = Scr.hist_data
			let analyze d = []
			let analyze_hist d = []
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
			let analyze_hist d = []
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
			
			let rec compress = function
					a::b::xs -> (compare a b)::(compress (b::xs))
				| _ -> []

			(* get points were compress != 0, and create point objects to pass to plotter *)
			let get_points compressed prices offset = 
				let ind = ref offset in
				List.fold 
					(List.zip_exn compressed prices) 
					~init:[]
					~f:(fun a (c, p) ->
						(incr ind; 
						match c with 
							1 -> { x= !ind; y= p; radius= 5; color= Graphics.green}::a
						| 0 -> a
						| -1 -> { x= !ind; y= p; radius= 5; color= Graphics.red}::a))

			(* (field, val) list list -> val list where field=field*)
			let extract_field_from_data d field = 
				let get_field_val l = 
					List.fold_left l ~init:"" ~f:(fun a (f, v) -> if f = field then v else a) in
				List.map d get_field_val

			let analyze_hist d = 
				let l = Scr.hist_data_to_list d in
				let dates = [] in
				let closes = List.map (extract_field_from_data l "Close") Float.of_string in
				let period = 50 in
				let mov_avg = moving_average closes period in
				let long_short = List.map2_exn (List.slice (List.rev closes) (period - 1) (List.length closes)) mov_avg ~f:compare in
				let buy_sell = (List.hd_exn long_short)::(compress long_short) in

				let _ = (try_with (fun () -> 
					Graphics.open_graph ""; 
					Graphics.resize_window 800 500;
					Plotter.graph_stock closes;
					Plotter.graph_stock ~offset:period ~color:Graphics.red (List.rev mov_avg);
					Graphics.wait_next_event [Graphics.Button_up];
					return (Graphics.close_graph ())) >>> function 
																				 Ok ()   -> ()
   																		 | Error _ -> print_string "failure") in

				(Scraper.print_vals 
					buy_sell (fun _ i -> 
						(print_string ((match i with 
															1 -> "Go Long" 
														| 0 -> "Stay" 
														| -1 -> "Go Short" 
														| _ -> "Unknown") ^ "\n")));
				List.map closes (fun t -> (Float.to_string t, 1.)))
		end;;

