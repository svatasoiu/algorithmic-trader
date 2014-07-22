open Core.Std;;
open Async.Std;;

let shorten s = String.sub s 0 (min 7 (String.length s))
let print_vals vals f = List.fold vals ~init:() ~f:f

module type SCRAPER =
	sig
		type data
		val extract_tickers : data -> string list
		val get_data : string list -> string list -> data Deferred.t
		val print_data : data -> unit
	end;;

module BasicScraper : SCRAPER =
	struct
		type data = (string * (string * string) list) list option 

		let remove_first_chr str = String.sub str 1 (String.length str - 1)

		let csv l = remove_first_chr (List.fold_left l ~init:"" ~f:(fun a x -> a ^ "," ^ x))

		let create_yql_query indicators tickers =
			"select " ^ (csv indicators) ^ " from yahoo.finance.quotes where symbol in ('" ^ (csv tickers) ^ "')"

		let create_uri indicators tickers =
			let base_uri = Uri.of_string "http://query.yahooapis.com/v1/public/yql?format=json&diagnostics=true&env=http://datatables.org/alltables.env" in
			Uri.add_query_param base_uri ("q", [create_yql_query indicators tickers])
 
		let get_fields_from_json json fields =
			let open Yojson.Basic.Util in
			let quote = json |> member "query" |> member "results" |> member "quote" in
			let extract_fields = 
				fun js -> 
					List.map fields (fun f -> (f, match js |> member f with `String s -> s | _ -> "")) in
			match quote with
			 | `List l -> List.map l extract_fields 
			 | `Assoc _ -> [extract_fields quote]
			 | _ -> []

		let extract_tickers = 
			function None -> []
						 | Some l -> List.map l (fun (ticker, _) -> ticker)

		let get_data fields tickers =
			Cohttp_async.Client.get (create_uri fields tickers)
			>>= fun (_, body) -> 
			Pipe.to_list (Cohttp_async.Body.to_pipe body)
			>>| fun strings ->
			List.zip tickers (get_fields_from_json (Yojson.Basic.from_string (String.concat strings)) fields)

		let print_data = 
			function None -> printf "Nothing reported!"
						 | Some l -> 
								 print_vals l (fun _ (ticker, vals) -> 
																	print_string ticker;
																	print_vals vals (fun _ (_, v) -> 
																								print_string ("\t|\t" ^ (shorten v))); 
																	print_string "\n")
	end;;
