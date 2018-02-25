module Error : sig
  exception Exit of int

  val err : string -> 'a
end