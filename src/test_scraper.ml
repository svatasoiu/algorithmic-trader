open OUnit;;
open Core.Std;;

let test_scraper = "BasicScraper" >:::
[
	"shorten" >:: (fun () ->
		for i = 1 to 100 do
			let d = Random.int i in 
			let s = Int.to_string (Random.bits ()) in
			assert_equal (String.length (Scraper.shorten s d)) (min (String.length s) d)
		done;
		assert_equal "short" (Scraper.shorten "shortless" 5)
	);
	
	"date_range_convert" >:: (fun () ->
		assert_equal (Scraper.date_range_to_annual_periods "2010-04-02" "2011-04-02") [("2010-04-02", "2011-04-02")];
		assert_equal (Scraper.date_range_to_annual_periods "2008-04-02" "2011-05-02") 
									[("2008-04-02", "2009-05-02");("2009-05-03", "2010-05-02");("2010-05-03", "2011-05-02")]
								 ~printer:(fun l -> List.fold l ~init:"" ~f:(fun x (a,b) -> x^","^a^"->"^b))
	)
]

open Analyzer;;
let test_analyzer = "Analyzer" >:::
[
	"mov_avg_comp" >:: (fun () ->
		assert_equal (mov_avg_comp 1. 2.) `Short;
		assert_equal (mov_avg_comp 2. 2.) `Long;
		assert_equal (mov_avg_comp 3. 2.) `Long
	);

	"pos_to_act" >:: (fun () ->
		assert_equal (pos_to_act `Long) `Buy;
		assert_equal (pos_to_act `Stay) `None;
		assert_equal (pos_to_act `Short) `Sell
	);

	"act" >:: (fun () ->
		assert_equal (act `Long `Long) `None;
		assert_equal (act `Long `Short) `Sell;
		assert_equal (act `Short `Long) `Buy
	);

	"get_last_action" >:: (fun () ->
		assert_equal (get_last_action [`Buy;`Sell;`None]) `Sell;
		assert_equal (get_last_action [`Buy]) `Buy;
		assert_equal (get_last_action [`None;`Sell]) `Sell;
		assert_equal (get_last_action [`None;`Sell;`Sell;`Buy]) `Buy;
		assert_equal (get_last_action [`None;`Sell;`Sell;`Buy]) `Buy;
		assert_equal (get_last_action [`None;`None]) `None
	);

	"moving_average" >:: (fun () ->
		assert_equal (moving_average [1.;2.] 2) [1.5];
		assert_equal (moving_average [1.;2.] 1) [1.;2.];
		assert_equal (moving_average [2.;1.;4.] 2) [1.5;2.5]
	);

	"eval_strategy" >:: (fun () ->
		assert_equal ~printer:Float.to_string (eval_strategy ~conservative:true [1.;2.;4.;5.;] [`Buy;`None;`Sell;`None]) 3.;
		assert_equal ~printer:Float.to_string (eval_strategy ~conservative:true [1.;2.;5.] [`Buy;`None;`None]) 4.;
		assert_equal ~printer:Float.to_string (eval_strategy ~conservative:true [1.;2.;4.;5.] [`Sell;`Sell;`Buy;`None]) (-.6.)
	)
]

let _ = run_test_tt test_scraper
let _ = run_test_tt test_analyzer
