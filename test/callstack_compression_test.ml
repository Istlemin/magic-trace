open Core
open Magic_trace_lib
open Import

let compressed_test_string =
  "(\n\
  \    ((new_symbols())(callstack()))\n\
  \    ((new_symbols())(callstack()))\n\
  \    ((new_symbols())(callstack()))\n\
  \    ((new_symbols(((From_perf _start)0)))(callstack((0 0))))\n\
  \    ((new_symbols((Untraced 1)))(callstack((1 0)(0 0))))\n\
  \    ((new_symbols())(callstack((0 0))))\n\
  \    ((new_symbols(((From_perf _dl_start)2)))(callstack((2 0)(0 0))))\n\
  \    ((new_symbols())(callstack((1 1)(0 0))))\n\
  \    ((new_symbols())(callstack((2 1))))\n\
  \    ((new_symbols())(callstack((1 2))))\n\
  \    ((new_symbols())(callstack((2 1))))\n\
  \    )"
;;

let decompressed_test_string =
  "(\n\
  \    ()\n\
  \    ()\n\
  \    ()\n\
  \    ((From_perf _start))\n\
  \    (Untraced (From_perf _start))\n\
  \    ((From_perf _start))\n\
  \    ((From_perf _dl_start) (From_perf _start))\n\
  \    (Untraced (From_perf _start) (From_perf _start))\n\
  \    ((From_perf _dl_start) (From_perf _start))\n\
  \    (Untraced (From_perf _start) (From_perf _start))\n\
  \    ((From_perf _dl_start) (From_perf _start))\n\
  \    )"
;;

let%expect_test "decompress" =
  let compression_events =
    [%of_sexp: Callstack_compression.compression_event list]
      (Sexp.of_string compressed_test_string)
  in
  let state = Callstack_compression.init () in
  List.iter compression_events ~f:(fun comp_event ->
    let callstack = Callstack_compression.decompress_callstack ~state comp_event in
    print_s [%sexp (callstack : Symbol.t list)]);
  [%expect
    {|
      ()
      ()
      ()
      ((From_perf _start))
      (Untraced (From_perf _start))
      ((From_perf _start))
      ((From_perf _dl_start) (From_perf _start))
      (Untraced (From_perf _start) (From_perf _start))
      ((From_perf _dl_start) (From_perf _start))
      (Untraced (From_perf _start) (From_perf _start))
      ((From_perf _dl_start) (From_perf _start))|}]
;;

let%expect_test "compress" =
  let compression_events =
    [%of_sexp: Symbol.t list list] (Sexp.of_string decompressed_test_string)
  in
  let state = Callstack_compression.init () in
  List.iter compression_events ~f:(fun callstack ->
    let comp_event = Callstack_compression.compress_callstack ~state callstack in
    print_s [%sexp (comp_event : Callstack_compression.compression_event)]);
  [%expect
    {|
    ((new_symbols ()) (callstack ()))
    ((new_symbols ()) (callstack ()))
    ((new_symbols ()) (callstack ()))
    ((new_symbols (((From_perf _start) 0))) (callstack ((0 0))))
    ((new_symbols ((Untraced 1))) (callstack ((1 0) (0 0))))
    ((new_symbols ()) (callstack ((0 0))))
    ((new_symbols (((From_perf _dl_start) 2))) (callstack ((2 0) (0 0))))
    ((new_symbols ()) (callstack ((1 1) (0 0))))
    ((new_symbols ()) (callstack ((2 1))))
    ((new_symbols ()) (callstack ((1 2))))
    ((new_symbols ()) (callstack ((2 1))))|}]
;;
