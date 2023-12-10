open Solvers.Signature

module Solver : Solver = struct
  let task1 data =
    string_of_int (String.length data)

  let task2 data _part1 =
    string_of_int (String.length data)
end;;