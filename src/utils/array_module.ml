let update_array arr i x =
  arr.(i) <- x;
  arr

let update_2darray arr i j x =
  arr.(j).(i) <- x;
  arr