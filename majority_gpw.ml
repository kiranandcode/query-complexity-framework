open Core

module MajorityGPW = struct

  type t = (bool * (int * int * int) option) array * int

  type p = int

  type o = FakePointers | RandomPointers | FixedValidCount of int | RandomValidCount

  let get_params (_, n) = n

  let generate_instance ?options:(_ :o option) (n: p) =
    let array = Array.create ~len:(n * n * n) (false, None) in
    for i = 0 to n - 1  do
      for j = 0 to n - 1 do
        for k = 0 to n - 1 do
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
    array, n

  let get_value (array, n) (i,j,k) =
    let index = n * n * i + n * j + k in
    array.(index)


  let to_string ((elements, n): t) : string =
    let string = ref "" in
    for i = 0 to n - 1 do
      for k = 0 to n - 1 do
        for j = 0 to n - 1 do
          let value : bool = fst @@ get_value (elements, n) (i,j,k) in
          let ptr  = snd @@ get_value (elements, n) (i,j,k) in
          string := !string ^ (Printf.sprintf "%8b" value);
          match ptr with
          | None -> string := !string ^ "[   NONE   ]"
          | Some (i,j,k) -> string := !string ^ (Printf.sprintf "(%2d, %2d, %2d)" i j k);
          if not (j = n - 1) then string := !string ^ " ";
        done;
        string := !string ^ "\n";
      done;
      string := !string ^ "\n========================================\n";
    done;
    !string
      
  let generate_true_instance ?options:(_options :o option) n =
    let array = Array.create ~len:(n * n * n) (false, None) in
    let set_value (i,j,k) v =
      let index = n * n * i + n * j + k in
      let (_, ptr) = array.(index) in
      array.(index) <- (v, ptr) in
    let set_ptr (i,j,k) ptr =
      let index = n * n * i + n * j + k in
      let (v, _) = array.(index) in
      array.(index) <- (v, ptr) in
    for i = 0 to n - 1  do
      for j = 0 to n - 1 do
        for _ = 0 to n - 2 do
          let index = Random.int n in
          set_value (i,j, index) true
        done
      done;
    done;
    let set = ref (Set.empty (module Int)) in
    while Set.length !set <= n / 2 + 1 do
      let index = Random.int n in
      set := (Set.add !set index)
    done;
    Set.iter !set ~f:(fun i ->
        let principal = Random.int n in
        let pointer_prev = ref None in
        let seen = ref (Set.empty (module Int)) in
        while Set.length !seen < n  do
          let j  = Random.int n in
          if Set.mem !seen j then () else begin
            begin
              match Int.(j = principal) with
              | true -> (* setup principal column *)
                for k = 0 to n -1  do
                  set_value (i,j,k) true
                done
              | false -> (* select base element *)
                let k = Random.int n in
                for k' = 0 to n - 1 do
                  match k = k', _options with
                  | true, _ ->
                    set_value (i,j,k) false;
                    set_ptr (i,j,k) !pointer_prev;
                    pointer_prev := Some (i,j,k)
                  | _, Some FakePointers ->
                    let pointer =
                      let y = Random.int n in
                      let z = Random.int n in
                      Some (i,y,z)
                    in
                    set_ptr (i,j,k) pointer
                  | _, Some RandomPointers ->
                    let pointer =
                      match Random.bool () with
                      | true -> 
                        let y = Random.int n in
                        let z = Random.int n in
                        Some (i,y,z)
                      | false -> None
                    in
                    set_ptr (i,j,k) pointer
                  | _, _ -> ()
                done;
            end;
            seen := (Set.add !seen j)
          end
        done;
        (* Connect to the pointer chain *)
        let base = Random.int n in
        set_ptr (i, principal, base) !pointer_prev
      );
    array, n
  
  let generate_false_instance ?options:(_options :o option) n =
    let array = Array.create ~len:(n * n * n) (false, None) in
    let set_value (i,j,k) v =
      let index = n * n * i + n * j + k in
      let (_, ptr) = array.(index) in
      array.(index) <- (v, ptr) in
    let set_ptr (i,j,k) ptr =
      let index = n * n * i + n * j + k in
      let (v, _) = array.(index) in
      array.(index) <- (v, ptr) in
    for i = 0 to n - 1  do
      for j = 0 to n - 1 do
        for _ = 0 to n - 2 do
          let index = Random.int n in
          set_value (i,j, index) true
        done
      done;
    done;
    let set = ref (Set.empty (module Int)) in
    let bound = match _options with
      | Some RandomValidCount -> Random.int (n / 2 - 1)
      | Some (FixedValidCount n) -> n
      | _ -> (n / 2 - 1)
    in
    while Set.length !set < bound do
      let index = Random.int n in
      set := (Set.add !set index)
    done;
    Set.iter !set ~f:(fun i ->
        let principal = Random.int n in
        let pointer_prev = ref None in
        let seen = ref (Set.empty (module Int)) in
        while Set.length !seen < n  do
          let j  = Random.int n in
          if Set.mem !seen j then () else begin
            begin
              match Int.(j = principal) with
              | true -> (* setup principal column *)
                for k = 0 to n -1  do
                  set_value (i,j,k) true
                done
              | false -> (* select base element *)
                let k = Random.int n in
                for k' = 0 to n - 1 do
                  match k = k', _options with
                  | true, _ ->
                    set_value (i,j,k) false;
                    set_ptr (i,j,k) !pointer_prev;
                    pointer_prev := Some (i,j,k)
                  | _, Some FakePointers ->
                    let pointer =
                      let y = Random.int n in
                      let z = Random.int n in
                      Some (i,y,z)
                    in
                    set_ptr (i,j,k) pointer
                  | _, Some RandomPointers ->
                    let pointer =
                      match Random.bool () with
                      | true -> 
                        let y = Random.int n in
                        let z = Random.int n in
                        Some (i,y,z)
                      | false -> None
                    in
                    set_ptr (i,j,k) pointer
                  | _, _ -> ()
                done
            end;
            seen := (Set.add !seen j)
          end
        done;
        (* Connect to the pointer chain *)
        let base = Random.int n in
        set_ptr (i, principal, base) !pointer_prev
      );
    array, n

end
