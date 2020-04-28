open Core
open Utils

module GeneralizedGPW (* : FUNCTION *) = struct

  type t = (bool * (int * int * int) option) array * (int * int)

  type p = int * int

  let get_params (_, pair) = pair 

  let generate_instance (n, k) =
    let n = n * k in
    let array = Array.create ~len:(n * n * n) (false, None) in
    for i = 0 to n -1   do
      for j = 0 to n - 1 do
        for k = 0 to n  - 1 do
          let value = Random.bool () in
          let ptr = match Random.bool () with
            | true -> None
            | false ->
              let x = Random.int n in
              let y = Random.int n in
              let z = Random.int n in
              Some (x,y,z)
          in
          let index = i * n * n + j * n + k in
          array.(index) <- (value,ptr)
        done
      done
    done;
    array, (n, k)

  let get_value (array, (n, k')) (i,j,k) =
    let nk = n * k' in
    let index = nk * nk * i + nk * j + k in
    array.(index)

  let to_string ((elements, (n, k)): t) : string =
    let cell_to_string (x,y,z) =
      let string = ref "" in
      string := !string ^ "[";
      for a = 0 to k - 1 do
        string := !string ^ "[";
        for c = 0 to k - 1 do
          for b = 0 to k - 1 do
            let index =  (x * k + a, y * k + b, z * k + c) in
            let value : bool = fst @@ get_value (elements, (n,k)) index in
            let ptr  = snd @@ get_value (elements, (n, k)) index in
            string := !string ^ (Bool.to_string value);
            match ptr with
            | None -> string := !string ^ "[   NONE   ]"
            | Some (i,j,k) -> string := !string ^ (Printf.sprintf "(%2d, %2d, %2d)" i j k);
              if not (j = n - 1) then string := !string ^ " ";
          done;
          string := !string ^ "\n";
      done;
      string := !string ^ "]\n";
    done;
    !string
    in
    let string = ref "" in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        for k = 0 to n - 1 do
          string := !string ^ (Printf.sprintf "{%d,%d,%d}: " i j k);
          string := !string ^ (cell_to_string (i, j, k))
        done
      done
    done;
    !string

  (* n is the number of inner cubes, and k is the size of each inner cube *)
  let generate_true_instance (n,k) =
    let nk = n * k in
    let array = Array.create ~len:(nk * nk * nk) (false, None) in
    let set_value (i,j,k) v =
      let index = nk * nk * i + nk * j + k in
      let (_, ptr) = array.(index) in
      array.(index) <- (v, ptr) in
    let set_ptr (i,j,k) ptr =
      let index = nk * nk * i + nk * j + k in
      let (v, _) = array.(index) in
      array.(index) <- (v, ptr) in
    let principal_cell =
      let x = Random.int n in
      let y = Random.int n in
      let z = Random.int n in
      (x,y,z) in
    (* fill principal cell *)
    let () =
      let cells = i_to_j 0 (k * k * k) in
      List.iter cells ~f:(fun cell ->
          let (x,y,z) = to_triple k cell in
          let index =
            let (bx,by,bz) = principal_cell in
            (bx * k + x, by * k + y, bz *k + z)
          in
          set_value index true
        )
    in
    let prev_value = ref None in
    (* random permutation of non-principal cells *)
    let () =
      let non_principal_cells =
        let ls = i_to_j 0 (n * n * n) in
        let (bx,by,bz) = principal_cell in
        let ls = rem (bx * n * n + by * n + bz) ls in
        List.permute ls
      in
      List.iter non_principal_cells ~f:(fun cell ->
          let (x,y,z) = to_triple n cell in
          let count = ref 0 in
          let prev_added = ref false in
          for a = 0 to k - 1 do
            for b = 0 to k - 1 do
              for c = 0 to k - 1 do
                if Random.bool () &&  !count < k * k * k - 1 then begin
                  set_value (x * k + a, y * k + b, z * k + c) true;                
                  count := !count + 1;
                end
                else if not !prev_added then begin
                  (* add pointer from link to first prev value *)
                  prev_added := true;
                  set_ptr (x * k + a, y * k + b, z * k + c) !prev_value;
                  prev_value := Some (x * k + a,y * k + b,z * k + c);
                end
              done;
            done;
          done)
    in
    let () =
      let x = Random.int k in
      let y = Random.int k in
      let z = Random.int k in
      let index =
        let (bx,by,bz) = principal_cell in
        (bx * k + x, by * k + y, bz *k + z)
      in
      set_ptr index !prev_value
    in
    array, (n,k)

  let generate_false_instance (n,k) =
    let nk = n * k in
    let array = Array.create ~len:(nk * nk * nk) (false, None) in
    let set_value (i,j,k) v =
      let index = nk * nk * i + nk * j + k in
      let (_, ptr) = array.(index) in
      array.(index) <- (v, ptr) in
    let set_ptr (i,j,k) ptr =
      let index = nk * nk * i + nk * j + k in
      let (v, _) = array.(index) in
      array.(index) <- (v, ptr) in
    (* iterate through the cells*)
    let cells = i_to_j 0 (n * n * n) in
    List.iter cells ~f:(fun cell ->
        let (x,y,z) = to_triple n cell in
        (* set some (incomplete) proportion of the cells to be raised *)
        let bound = Random.int (k * k * k - 2) in
        for _ = 0 to bound  do
          let a = Random.int k in
          let b = Random.int k in
          let c = Random.int k in
          set_value (x * k + a, y * k + b, z * k + c) true;
          let ptr_value = match Random.bool () with
            | true -> None
            | false ->
              let x = Random.int nk in
              let y  = Random.int nk in
              let z = Random.int nk in
              Some (x,y,z) in
          set_ptr (x * k + a, y * k + b, z * k + c) ptr_value;
        done;
      );
    array, (n, k)
  
end
