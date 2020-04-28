[@@@ocaml "-33"]
open Core
(* open Generalized_gpw *)
(* open Gpw *)
open Utils
open Gnuplot

module Spec : Majority_alg.SPEC = struct

  let constant_Maj_B = 10

  let constant_2D_A = 10

end

module MajoritySampler = Majority_alg.MajoritySamplingAlgorithm (Spec) (Majority_gpw.MajorityGPW)


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

  


let generate_input_tests () =
  i_to_j 4 100 |> List.map ~f:(fun size ->
      let instance = Majority_gpw.MajorityGPW.generate_true_instance size in
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
      let instance = Majority_gpw.MajorityGPW.generate_false_instance size in
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

let generate_experiment_1 () =
  let size = 20 in
  let samples = 100 in
  i_to_j 0 10
  |> List.map ~f:(fun valid_count ->
      let instance =
        Majority_gpw.MajorityGPW.generate_false_instance
          ~options:(Majority_gpw.MajorityGPW.FixedValidCount valid_count) size in
      i_to_j 0 samples
      |> List.map ~f:begin fun _ ->
        fst (MajoritySampler.evaluate instance)
      end
      |> List.map ~f:(fun v ->  if v then 1 else 0)
      |> List.fold ~init:0 ~f:(+)
      |> fun v -> valid_count, Float.(of_int v / of_int samples)
    )
  |> print_output
    ~f:(fun (valid_count,v) -> Printf.sprintf "%d %f" valid_count v )
    ~filename:"../data/experiment1.dat"

let generate_experiment_2 () =
  let samples = 100 in
  i_to_j 4 21
  |> List.map ~f:(fun input_size ->
      let instance =
        Majority_gpw.MajorityGPW.generate_true_instance input_size in
      i_to_j 0 samples
      |> List.map ~f:begin fun _ ->
        snd (MajoritySampler.evaluate instance)
      end
      |> List.fold ~init:0 ~f:(+)
      |> fun v -> input_size, Float.(of_int v / of_int samples)
    )
  |> print_output
    ~f:(fun (valid_count,v) -> Printf.sprintf "%d %f" valid_count v )
    ~filename:"../data/experiment2a.dat";
  i_to_j 4 21
  |> List.map ~f:(fun input_size ->
      let instance =
        Majority_gpw.MajorityGPW.generate_false_instance input_size in
      i_to_j 0 samples
      |> List.map ~f:begin fun _ ->
        snd (MajoritySampler.evaluate instance)
      end
      |> List.fold ~init:0 ~f:(+)
      |> fun v -> input_size, Float.(of_int v / of_int samples)
    )
  |> print_output
    ~f:(fun (valid_count,v) -> Printf.sprintf "%d %f" valid_count v )
    ~filename:"../data/experiment2b.dat"


let generate_experiment_3 () =
  let samples = 100 in
  i_to_j 4 31
  |> List.map ~f:(fun input_size ->
      i_to_j 0 samples
      |> List.concat_map ~f:begin fun _ ->
         let instance = Majority_gpw.MajorityGPW.generate_true_instance input_size in
         i_to_j 0 10
          |> List.map ~f:(fun _ -> snd (MajoritySampler.evaluate instance))
      end
      |> List.fold ~init:0 ~f:(+)
      |> fun v -> input_size, Float.(of_int v / of_int samples)
    )
  |> print_output
    ~f:(fun (valid_count,v) -> Printf.sprintf "%d %f" valid_count v )
    ~filename:"../data/experiment3a.dat";
  i_to_j 4 31
  |> List.map ~f:(fun input_size ->
      i_to_j 0 samples
      |> List.concat_map ~f:begin fun _ ->
        let instance = Majority_gpw.MajorityGPW.generate_true_instance
                         ~options:(Majority_gpw.MajorityGPW.RandomPointers) input_size in
         i_to_j 0 10
          |> List.map ~f:(fun _ -> snd (MajoritySampler.evaluate instance))
      end
      |> List.fold ~init:0 ~f:(+)
      |> fun v -> input_size, Float.(of_int v / of_int samples)
    )
  |> print_output
    ~f:(fun (valid_count,v) -> Printf.sprintf "%d %f" valid_count v )
    ~filename:"../data/experiment3b.dat";
  i_to_j 4 31
  |> List.map ~f:(fun input_size ->
      i_to_j 0 samples
      |> List.concat_map ~f:begin fun _ ->
        let instance = Majority_gpw.MajorityGPW.generate_false_instance
                         ~options:(Majority_gpw.MajorityGPW.RandomPointers) input_size in
         i_to_j 0 10
          |> List.map ~f:(fun _ -> snd (MajoritySampler.evaluate instance))
      end
      |> List.fold ~init:0 ~f:(+)
      |> fun v -> input_size, Float.(of_int v / of_int samples)
    )
  |> print_output
    ~f:(fun (valid_count,v) -> Printf.sprintf "%d %f" valid_count v )
    ~filename:"../data/experiment3c.dat";
  i_to_j 4 31
  |> List.map ~f:(fun input_size ->
      i_to_j 0 samples
      |> List.concat_map ~f:begin fun _ ->
        let instance = Majority_gpw.MajorityGPW.generate_false_instance
                         ~options:(Majority_gpw.MajorityGPW.RandomPointers) input_size in
         i_to_j 0 10
          |> List.map ~f:(fun _ -> snd (MajoritySampler.evaluate instance))
      end
      |> List.fold ~init:0 ~f:(+)
      |> fun v -> input_size, Float.(of_int v / of_int samples)
    )
  |> print_output
    ~f:(fun (valid_count,v) -> Printf.sprintf "%d %f" valid_count v )
    ~filename:"../data/experiment3d.dat"

let () =
  generate_experiment_1 ();
  generate_experiment_2 ();
  generate_experiment_3 ()
