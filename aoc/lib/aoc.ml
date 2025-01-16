open! Core
open! Async

module Day_1 = struct
  (** Read a file of lines, each line has two integers *)
  let read_file () =
    let%map input = Reader.file_contents "in.txt" in
    let lines = String.split_lines input in
    let ints =
      List.map lines ~f:(fun line -> Scanf.sscanf line "%d %d" (fun a b -> a, b))
    in
    ints
  ;;

  let p1 () =
    let%map ints = read_file () in
    let l1 = List.map ints ~f:(fun (a, _b) -> a) |> List.sort ~compare:Int.compare in
    let l2 = List.map ints ~f:(fun (_a, b) -> b) |> List.sort ~compare:Int.compare in
    let zipped = List.zip_exn l1 l2 in
    let res1 = List.fold zipped ~init:0 ~f:(fun acc (a, b) -> Int.abs (a - b) + acc) in
    print_s [%message "Day 1, Part 1" (res1 : int)];
    let l2_cts =
      List.fold
        l2
        ~init:Int.Map.empty
        ~f:(Int.Map.update ~f:(Option.value_map ~default:1 ~f:(fun v -> v + 1)))
    in
    let res2 =
      List.fold l1 ~init:0 ~f:(fun acc x ->
          match Int.Map.find l2_cts x with
          | None -> acc
          | Some v -> acc + (x * v))
    in
    print_s [%message "Day1, Part 2" (res2 : int)]
  ;;

  let%expect_test "Day 1" =
    let%bind () = p1 () in
    [%expect {|  |}]
  ;;
end

let command =
  Async_command.async_or_error
    ~summary:"Advent of Code 2024"
    (let%map_open.Command () = return () in
     fun () ->
       let%bind () = Day_1.p1 () in
       Deferred.Or_error.return ())
;;
