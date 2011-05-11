(*
Copyright (c) <2011> <Deepak Garg dg@cs.cmu.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

signature PBPARSER = sig
    val parse_pb: string -> Syntax.proto Option.option
    val parse_pb_file: string -> Syntax.proto Option.option
end


structure PBParser :> PBPARSER = struct

structure PBP = PBParseFn(PBLexer)

fun parse_pb (s: string): Syntax.proto Option.option = 
    let val sm = AntlrStreamPos.mkSourcemap()
	val lex = PBLexer.lex sm
	val strm = PBLexer.streamify (fn () => s ^ " %#parse_end#%")
	val (r, _, errs) = PBP.parse lex strm
	val errString = String.concatWith "\n" 
			(map (AntlrRepair.repairToString
			      PBTokens.toString sm) errs)
    in
	(print errString); 
	r
    end
    
(* Read entire file as a string -- be careful, this may cause memory exhaustion *)
fun readFile (f: string): string =
    if (f <> "-") then
	let val inp = BinIO.openIn f
	    val s = BinIO.inputAll inp
	    val s' = Byte.bytesToString s
	in 
	    (BinIO.closeIn(inp); s')
	end
    else (* read stdIn *)
	let fun recread s = 
		let val snew = TextIO.inputLine (TextIO.stdIn)
		in
		    case snew of 
			NONE => s
		      | SOME s' => recread (s ^ s')
		end
	in
	    recread ""
	end

fun parse_pb_file s = parse_pb (readFile s)

end

