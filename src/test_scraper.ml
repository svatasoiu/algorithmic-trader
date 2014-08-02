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

let _ = run_test_tt test_scraper
