open Core.Std;;
open Async.Std;;

let shorten s n = String.sub s 0 (min n (String.length s))
let print_vals vals f = List.iter vals ~f:f

let date_range_to_annual_periods start_date end_date =
	let rec aux s e acc =
		if (Date.diff e s) > 400 
		then let prev = Date.add_months e (-12)
				 in aux s prev ((Date.add_days prev 1, e)::acc)
		else (s, e)::acc
	in let ranges = aux (Date.of_string start_date) (Date.of_string end_date) []
	in List.map ranges (fun (s, e) -> (Date.to_string s, Date.to_string e))

module type SCRAPER =
	sig
		type data
		type hist_data

		val extract_tickers : data -> string list
		val get_data : string list -> string list -> data Deferred.t
		val get_hist_data : string list -> string -> string -> string -> hist_data Deferred.t
		val hist_data_to_list : hist_data -> (string * string) list list
		val print_data : data -> unit
		val print_hist_data : hist_data -> unit
	end;;

module BasicScraper : SCRAPER =
	struct
		type data = (string * (string * string) list) list option 

		(*                ticker    date     (field,   value) list  *)
		type hist_data = string * (string * (string * string) list) list 

		let remove_first_chr str = String.sub str 1 (String.length str - 1)

		let csv l = remove_first_chr (List.fold_left l ~init:"" ~f:(fun a x -> a ^ "," ^ x))

		let create_yql_query indicators tickers =
			"select " ^ (csv indicators) ^ " from yahoo.finance.quotes where symbol in ('" ^ (csv tickers) ^ "')"

		let create_hist_yql_query indicators tickers start_date end_date =
			"select " ^ (csv indicators) ^ " from yahoo.finance.historicaldata where symbol in ('" ^ (csv tickers) ^ "') and startDate='" ^ start_date ^ "' and endDate='" ^ end_date ^ "'"

		let create_uri indicators tickers =
			let base_uri = Uri.of_string "http://query.yahooapis.com/v1/public/yql?format=json&diagnostics=true&env=http://datatables.org/alltables.env" in
			Uri.add_query_param base_uri ("q", [create_yql_query indicators tickers])
 
		let create_hist_uri indicators tickers start_date end_date =
			let base_uri = 
				Uri.of_string "http://query.yahooapis.com/v1/public/yql?diagnostics=true&env=store://datatables.org/alltableswithkeys&format=json" in
			Uri.add_query_param base_uri ("q", [create_hist_yql_query indicators tickers start_date end_date])

		let js_to_string = function `String s -> s | _ -> "";;

		let get_fields_from_json json fields =
			let open Yojson.Basic.Util in
			try 
				let quote = json |> member "query" |> member "results" |> member "quote" in
				let extract_fields = 
					fun js -> 
						List.map fields (fun f -> (f, js_to_string (js |> member f))) in
				match quote with
				 | `List l -> List.map l extract_fields 
				 | `Assoc _ -> [extract_fields quote]
				 | _ -> []
			with _ -> []


		let get_hist_fields_from_json json fields =
			let open Yojson.Basic.Util in
			try 
				let quote = json |> member "query" |> member "results" |> member "quote" in
				let extract_fields = 
					fun js -> 
						(js_to_string (js |> member "Date"), List.map fields (fun f -> (f, js_to_string (js |> member f)))) in
				match quote with
				 | `List l -> List.map l extract_fields 
				 | _ -> []
			with _ -> []

		let extract_tickers = 
			function None -> []
						 | Some l -> List.map l (fun (ticker, _) -> ticker)

		let get_data fields tickers =
			Cohttp_async.Client.get (create_uri fields tickers)
			>>= fun (_, body) -> 
			Pipe.to_list (Cohttp_async.Body.to_pipe body)
			>>| fun strings ->
			List.zip tickers (get_fields_from_json (Yojson.Basic.from_string (String.concat strings)) fields)

		let get_hist_data fields ticker start_date end_date =
			let ranges = date_range_to_annual_periods start_date end_date in
		  let uris = List.map ranges (fun (s,e) -> create_hist_uri ("Date"::fields) [ticker] s e) in
			Deferred.all (List.map 
				uris 
				(fun uri -> 
					Cohttp_async.Client.get uri
					>>= fun (_, body) -> 
					Pipe.to_list (Cohttp_async.Body.to_pipe body)
					>>| fun strings -> get_hist_fields_from_json (Yojson.Basic.from_string (String.concat strings)) fields))
			>>| fun l -> (ticker, List.concat (List.rev l)) 

		let hist_data_to_list (_, l) = List.rev (List.map l (fun (_,t) -> t))

		let print_data = 
			function None -> printf "Nothing reported!"
						 | Some l -> 
								 print_vals l (fun (ticker, vals) -> 
																	print_string ticker;
																	print_vals vals (fun (_, v) -> print_string ("\t|\t" ^ (shorten v 7))); 
																	print_string "\n")
		
		let print_hist_data (ticker, l) = 
			(print_string ticker;
			print_vals l (fun (date, d) -> 
				print_string ("\t\t|\t" ^ (shorten date 10));
				print_vals d (fun (_, v) -> 
					print_string ("\t|\t" ^ (shorten v 7))); 
				print_string "\n"))
	end;;
