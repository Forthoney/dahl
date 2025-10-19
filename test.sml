open TextIO
val _ = scanStream (Int.scan StringCvt.DEC) stdIn
val s = inputLine stdIn
