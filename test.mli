type test

val (>:) : string -> (unit -> bool) -> test

type suite

val (>::) : string -> test list -> suite

val run_test : test -> unit
val run_suite : suite -> unit