open Core
open Gpw
open Utils

module type SPEC = sig

  (* number of samples to draw when evaluating each 2D instance *)
  val constant_2D_A : int

  (* number of samples to estimate if majority *)
  val constant_Maj_B : int

end

module MajoritySamplingAlgorithm (Spec: SPEC) (Function: FUNCTION with type p = int) = struct

  open Spec

  let evaluate (instance: Function.t)  =
    (* variable to count the number of queries *)
    let seen_elements = (Hash_set.create (module Int_triple)) in
    let query_count = ref 0 in
    (* query operation that tracks number of queries *)
    let query (i,j,k) =
      if not (Hash_set.mem seen_elements (i,j,k)) then begin
        query_count := !query_count + 1;
        Hash_set.add seen_elements (i,j,k);
      end;
      Function.get_value instance (i,j,k) in
    (* input consists of n x n x n matrix *)
    let n = Function.get_params instance in
    (* repeatedly draw k samples *)
    (* if 1-instance, each slice has a 1/2 prob of being valid,
       #no-valid X =  x_1 + ... + x_k 
       - expected value is k/2
       Chernoff:
       P[ X \leq (1 - \delta) k/2 ] \leq \exp\{ - 1/2 * k/2 * \delta * 2 \} = 1/3
       thus: \delta = \sqrt{4/k\log(3)} then the probability of error is \leq 1/3.
       Hence,
       P[X \leq k/2 - \sqrt{k}] \leq {1/3} if sampling from 1-instance
    *)
    let no_samples = if constant_Maj_B >= n then n /2 else constant_Maj_B in
    let threshhold =
      let open Float in
      let no_samples = of_int no_samples in
      to_int (no_samples / 2.0  - sqrt (no_samples)) |> fun x -> Int.(x + 1)  in
    let count = ref 0 in
    (* muks et al's algorithm for sampling *)
    let validate_plane instance =
      (*  specialize the query operation to this plane *)
      let query (i,j) = query (instance, i,j) in
      let verify_1_instance principal_column =
        (* verify the principal column and extract the outgoing pointer *)
        let valid_principal_column () =
          let rec loop r acc =
            match r < n with
            | false -> acc
            | true ->
              let index = (principal_column, r) in
              let (value,ptr) = query index in
              match value, ptr, acc with
              (* if mutliple outgoing pointers/not all elements are 1, exit *)
              | _, Some _, Some _ | false, _, _ -> None
              | true, None, Some ptr | true, Some ptr, None -> loop (r + 1) (Some ptr)
              | true, None, None -> loop (r + 1) None
          in
          loop 0 None
        in
        let verify_pointer_chain ptr =
          let columns = Hash_set.of_list (module Int) (i_to_j 0 n) in
          Hash_set.remove columns principal_column;
          let rec loop posn count = if count > 0 then
              match posn with
              | Some (_v, i, j) ->
                let (value, next_posn) = query (i,j) in
                if not value then begin
                  Hash_set.remove columns i;
                  loop next_posn (count - 1)
                end
                else false
              | None -> loop posn (count - 1)
            else (Hash_set.length columns) = 0  in
          let result = loop ptr (n - 1) in
          result
        in
        let ptr_chain = valid_principal_column () in
        verify_pointer_chain (ptr_chain)
      in
      (* non-eliminated columns *)
      let columns = Hash_set.of_list (module Int) (i_to_j 0 n) in
      let follow_pointer (j,i) =
        let step = ref 0 in
        let flag = ref false in
        let coordinate = ref (j,i) in
        let discarded = Hash_set.create (module Int) in
        if Hash_set.length columns > 0 then begin
          while not !flag && !step <= 100 * n * (Hash_set.length discarded) / (Hash_set.length columns) do
            step := !step + 1;
            let (value,ptr) = query !coordinate in
            begin
              match value with
              | true -> flag := true
              | false ->
                if Hash_set.mem (Hash_set.diff columns discarded) j then Hash_set.add discarded j 
            end;
            match ptr with
            | None -> flag := true
            | Some (_, j,i) -> coordinate := (j,i)
          done;
          (* update set of columns with S/D *)
          Hash_set.filter_inplace columns ~f:(fun a -> not @@ Hash_set.mem discarded a)
        end
      in
      let bound =
        let open Float in
        let n = of_int n in
        of_int constant_2D_A *  n * Float.log (n) |> to_int
      in
      for _ = 0 to bound do
        let j = List.random_element (Hash_set.to_list columns) in
        let i = Random.int n in
        match j with
        | Some j -> follow_pointer (j,i)
        | None -> ()
      done;
      if Hash_set.length columns = 1 then
        begin
          let column = (Hash_set.to_array columns).(0)  in
          verify_1_instance column
        end
      else false
    in
    for _ = 0 to no_samples do
      (*  select a random plane *)
      let plane = Random.int n in
      if validate_plane plane then begin
        count := !count + 1;
      end
    done;
    (* if fewer than this threshold of 1-instances then 0-instance (1/3 prob of incorrect)  *)
    (if !count < threshhold then false else true), !query_count

end
