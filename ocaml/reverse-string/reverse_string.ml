let reverse_string s =
  let len = String.length s in
  String.init len (fun i -> s.[len - i - 1])
