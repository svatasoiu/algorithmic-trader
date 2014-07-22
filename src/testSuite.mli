(*val create_test_case : 'a Async.Std.Deferred.t -> ('a -> 'b) -> ('c -> 'b -> bool) -> 'c -> string -> unit*)
val assert_test : ('a -> 'a -> bool) -> 'a Async.Std.Deferred.t -> 'a -> string -> ('a -> string) -> unit
val assert_eq : 'a Async.Std.Deferred.t -> 'a -> string -> ('a -> string) -> unit
val run_suite : 'a -> unit
