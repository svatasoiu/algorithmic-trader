open Core.Std;;
open Async.Std;;

let fields_default = ["Open";"Close"];;

let gd = fun fs ticker start_date end_date ->
					 Scraper.BasicScraper.get_hist_data fs ticker start_date end_date
					 >>| Scraper.BasicScraper.print_hist_data;;

let command =
  Command.basic
    ~summary:"Runs historical scraper"
    Command.Spec.(
      empty
			+> flag "-f" (listed string) ~doc:" fields"
			+> flag "-t" (optional_with_default "YHOO" string) ~doc:" ticker"
      +> flag "-s" (optional_with_default "2010-05-01" string) ~doc:" start of date range"
			+> flag "-e" (optional_with_default "2010-05-10" string) ~doc:" end of date range"
    )
    (fun fs ticker start_date end_date () -> 
			let fields = (match fs with [] -> fields_default | xs -> xs) in
			Scraper.print_vals ("Ticker"::"Date"::fields) (fun _ field -> print_string ((Scraper.shorten field 7) ^ "\t\t|\t"));
			print_string "\n";
			gd fs ticker start_date end_date >>> fun _-> shutdown 0
		)

let () = Command.run command

(* gd >>> fun _-> shutdown 0;; *)

let () = never_returns (Scheduler.go ())
