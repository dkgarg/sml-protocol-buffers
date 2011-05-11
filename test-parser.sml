structure TestParser = struct

fun test (f: string) = 
    case PBParser.parse_pb_file f of
	NONE => ()
      | SOME proto =>
	print (Syntax.proto_to_string proto)

end
