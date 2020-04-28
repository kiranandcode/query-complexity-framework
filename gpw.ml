    
module type FUNCTION = sig

  (* instance input *)
  type t

  (* params *)
  type p

  (* options for generation *)
  type o

  val to_string: t -> string

  val generate_instance: ?options:o -> p -> t

  val generate_true_instance: ?options:o ->  p -> t

  val generate_false_instance: ?options:o ->  p -> t

  val get_value: t -> int * int * int -> bool * ((int * int * int) option) 

  val get_params: t -> p

end

