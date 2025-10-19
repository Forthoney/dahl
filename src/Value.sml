structure Value =
struct
  datatype t =
    Nil
  | Boolean of bool
  | String of string
  | Number of real
end
