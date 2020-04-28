[@@@ocaml "-33"]
open Core
(* open Generalized_gpw *)
open Majority_gpw
(* open Gpw *)
open Utils
open Gnuplot

module Spec : Majority_alg.SPEC = struct

  let constant_Maj_B = 30

  let constant_2D_A = 5

end

module MajoritySampler = Majority_alg.MajoritySamplingAlgorithm (Spec) (MajorityGPW)


let plot_output x =
  Series.points_xy x
  |> fun data ->
  Gnuplot.with_  ~verbose:true (fun gp ->
      Gnuplot.plot ~range:Range.(X (0.0, 20.0)) gp data;
      ignore (Scanf.scanf "%c" (fun _ -> ()));
    )

let print_output ~f ~filename data =
  List.map ~f data
  |> Out_channel.write_lines filename

  


let () =
  i_to_j 4 100 |> List.map ~f:(fun size ->
      let instance = MajorityGPW.generate_true_instance size in
      let count = 10 in
      Float.of_int size, i_to_j 0 count |> List.map ~f:(fun _ ->
          let (result, _) = MajoritySampler.evaluate instance in
          if result then 0 else 1)
            |> List.fold ~init:0 ~f:(+)
            |> Float.of_int
            |> fun v -> v /. (Float.of_int count)
    )
  |> print_output
    ~f:(fun (a,b) -> Printf.sprintf "%2f %2f" a b)
    ~filename:"data/plt1_1inst_is.dat";

  i_to_j 4 100 |> List.map ~f:(fun size ->
      let instance = MajorityGPW.generate_false_instance size in
      let count = 10 in
      Float.of_int size, i_to_j 0 count |> List.map ~f:(fun _ ->
          let (result, _) = MajoritySampler.evaluate instance in
          if result then 1 else 0)
            |> List.fold ~init:0 ~f:(+)
            |> Float.of_int
            |> fun v -> v /. (Float.of_int count)
    )
  |> print_output
    ~f:(fun (a,b) -> Printf.sprintf "%2f %2f" a b)
    ~filename:"data/plt1_0inst_is.dat"



  (* List.iter ~f:(fun (size,no_false_positives) ->
   *     Printf.printf "%3d: %3f\n" size no_false_positives
   *   ) *)



