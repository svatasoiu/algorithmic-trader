type position = [`Long | `Stay | `Short]
type action 	= [`Buy | `None | `Sell]
type portfolio = (string * float) list

val mov_avg_comp : 'a -> 'a -> [> `Long | `Short ]
val pos_to_act : position -> action
val act : position -> position -> action
val get_last_action : [< `Buy | `None | `Sell] list -> [> `Buy | `None | `Sell]

val portfolio_to_string : portfolio -> string
val empty_portfolio : portfolio
val add_to_portfolio : portfolio -> string -> float -> portfolio
val moving_average : float list -> int -> float list 
val eval_strategy : ?conservative:bool -> float list -> action list -> float

module type ANALYZER =
	functor (Scr : Scraper.SCRAPER) ->
		sig
		  type data = Scr.data
			type hist_data = Scr.hist_data
			val analyze : data -> portfolio
			val analyze_hist : hist_data -> portfolio
			val create_buy_sell_stream : hist_data -> (float list * float list * float list * action list)
		end;;

module EmptyAnalyzer : ANALYZER;;
module EqualAnalyzer : ANALYZER;;
module MovingAverageAnalyzer : ANALYZER;;
