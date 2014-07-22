val assert_test : ('a -> 'a -> bool) -> 'a Async.Std.Deferred.t -> 'a -> ?printer:('a -> string) -> string -> unit
val assert_equal : 'a Async.Std.Deferred.t -> 'a -> ?printer:('a -> string) -> string -> unit
val run_suite : 'a -> unit
