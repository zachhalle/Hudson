type test = {
  name : string ;
  body : unit -> bool
}

let (>:) name body = {
  name = name ;
  body = fun () -> 
    try body () with
    | _ -> false
}

type suite = {
  name : string ;
  tests : test list
}

let (>::) name tests = {
  name = name ;
  tests = tests
}

let run_test test =
  if not (test.body ()) then
    Printf.printf "Test failed: \'%s\'.\n" test.name

let run_suite suite =
  Printf.printf "Running test suite \'%s\'.\n" suite.name;
  let rec loop tests =
    match tests with
    | [] -> (0, 0)
    | test :: tests ->
      if not (test.body ()) then begin
        Printf.printf "Test failed: \'%s\'.\n" test.name;
        let passed, total = loop tests in
        passed, total + 1
      end else
        let passed, total = loop tests in
        passed + 1, total + 1
  in
  let passed, total = loop suite.tests in
  Printf.printf "Suite \'%s\' results: [%d/%d] tests passed.\n" suite.name passed total