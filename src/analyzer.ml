open Core.Std;;
open Async.Std;;

type position = [`Long | `Stay | `Short]
type action 	= [`Buy | `None | `Sell]

type portfolio = (string * float) list

let period = 200;;
let debug = true;;

let portfolio_to_string p = 
	List.fold 
		p 
		~init:"" 
		~f:(fun a (s,f) -> s ^ ": " ^ (Float.to_string f) ^ ", " ^ a)

let mov_avg_comp a b =
	if a < b
	then `Short
	else `Long
	
let pos_to_act = function `Long -> `Buy | `Stay -> `None | `Short -> `Sell

let act a b = match (a,b) with
	| (`Long, `Long) | (`Short, `Short) -> `None
	| (`Long, `Short) -> `Sell | (`Short, `Long) -> `Buy
	| _ -> `None (* shouldn't happen *)

let get_last_action l =
	let rec aux last = 
		function `None::a' -> aux last a'
		| `Buy::a' -> aux `Buy a'
		| `Sell::a' -> aux `Sell a'
		| [] -> last
	in aux `None l

let eval_strategy closes strategy =
	let rec aux cs strat acc =
		match (cs, strat) with 
		|	(c::c', p::s) -> 
				aux c' s (match p with
										`Buy -> (if debug then (print_string ("Bought $" ^ (Float.to_string c) ^ "\n")); acc -. c)
									| `None -> acc
									| `Sell -> (if debug then (print_string ("Sold $" ^ (Float.to_string c) ^ "\n")); acc +. c))
		|	(_, _) -> acc
	in let res = aux closes strategy 0.
	in match (List.hd_exn strategy, get_last_action strategy) with
		 | (`Buy, `Buy) -> res +. (List.last_exn closes) 
		 | (`Sell, `Sell) -> res -. (List.last_exn closes)
		 | _ -> res (* shouldn't happen *)
(*
let eval_s closes strategy =
	let first = List.hd_exn strategy in
	match *)

let empty_portfolio = []
let add_to_portfolio p s f = (s, f)::p

let moving_average l n = 
	let arr = List.to_array l in
	List.rev 
		(List.map
			(Array.foldi arr ~init:[] 
				~f:(fun i a x -> 
					try 
						(Array.fold (Array.slice arr i (i + n)) ~init:0. ~f:(+.))::a 
					with _ -> a))
			(fun t -> t /. Float.of_int n))

let derivative l = []
let second_derivative l = []

module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
			type data = Scr.data
			type hist_data = Scr.hist_data

			val analyze : data -> portfolio
			val analyze_hist : hist_data -> portfolio
			val create_buy_sell_stream : hist_data -> (float list * float list * action list)
		end;;

module EmptyAnalyzer : ANALYZER =  
	functor (Scr : Scraper.SCRAPER) ->
		struct 
			type data = Scr.data
			type hist_data = Scr.hist_data
			let analyze d = []
			let analyze_hist d = failwith "Not Implemented"
			let create_buy_sell_stream d = failwith "Not Implemented"
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
			let analyze_hist d = failwith "Not Implemented"
			let create_buy_sell_stream d = failwith "Not Implemented"
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
					a::b::xs -> (act a b)::(compress (b::xs))
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
							`Buy -> (!ind, p, 5, Graphics.green)::a
						| `None -> a
						| `Sell -> (!ind, p, 5, Graphics.red)::a))

			(* (field, val) list list -> val list where field=field*)
			let extract_field_from_data d field = 
				let get_field_val l = 
					List.fold_left l ~init:"" ~f:(fun a (f, v) -> if f = field then v else a) in
				List.map d get_field_val

			let create_buy_sell_stream d = 
				let l = Scr.hist_data_to_list d in
				let closes = List.map (extract_field_from_data l "Adj_Close") Float.of_string in
				let mov_avg = moving_average closes period in
				let long_short = List.map2_exn (List.slice closes (period - 1) (List.length closes)) mov_avg ~f:mov_avg_comp in
				(closes, mov_avg, (pos_to_act (List.hd_exn long_short))::(compress long_short))

			let analyze_hist d = 
				let (closes, mov_avg, buy_sell) = create_buy_sell_stream d in

				let _ = (try_with (fun () -> 
					let g = new Plotter.plotter in
					(Graphics.open_graph ""; Graphics.resize_window 800 500;
					g#set_scale closes;
					g#graph_stock closes;
					g#graph_stock ~offset:(period - 1) ~color:Graphics.red mov_avg;
					g#draw_circles ~text:true (get_points buy_sell mov_avg (period - 2));
					Graphics.wait_next_event [Graphics.Button_up];
					return (Graphics.close_graph ()))) >>> function 
																				 Ok ()   -> ()
   																		 | Error _ -> print_string "Done") in

				let profit = eval_strategy closes buy_sell in
				let _ = print_string ("RESULT: $" ^ (Float.to_string profit) ^ "\n") in
				List.map closes (fun t -> (Float.to_string t, 1.))
		end;;

