(*

Copyright (c) <2011> <Deepak Garg garg.deepak@gmail.com> 
                     <Ankur Goyal ankrgyl@gmail.com>

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

structure CompilerSML = struct

local
    open Syntax
in

exception Impossible
exception Unimplemented

fun compile_proto (packagename: string, p: proto): string = 
    let 
	fun compile_proto_h (p: proto) = 
	    case p of
		[] => ""
	      | (decl :: ds) =>
		(compile_decl decl) ^ (compile_proto_h ds)
    in
	"structure " ^ packagename ^ " = struct\n" ^ 
	(compile_proto_h p) ^
	"end\n"
    end

and compile_decl d =
    case d of
	MessageD md => compile_messagedecl md
      | EnumD ed => compile_enumdecl ed
      | _ => ""

and compile_messagedecl (Messagedecl (id, fielddecllist)) = 

    let fun mapper (MessagedeclF md) = compile_messagedecl md
	  | mapper (EnumdeclF ed) = compile_enumdecl ed
	  | mapper _ = ""

	val helper_decls = String.concatWith "" (List.map mapper fielddecllist)

	val type_decls = List.filter (fn TypedeclF _ => true | _ => false) fielddecllist

	fun compile_field (TypedeclF (id, m, t, _)) =
	    (case m of
		 Required => id ^ ": " ^ (compile_gentype t) 
	       | Optional => id ^ ": " ^ (compile_gentype t) ^ " Option.option"
	       | Repeated => id ^ ": " ^ (compile_gentype t) ^ " List.list"
	    )
	  | compile_field _ = raise Impossible

	val compiled_types = String.concatWith ", " (List.map compile_field type_decls)
    in
	helper_decls ^ 
	"type " ^ id ^ " = {" ^ compiled_types ^ "}\n"
    end

and compile_enumdecl (Enumdecl (id, efielddecllist)) = 
    
    let val fieldnames = List.map (fn (Efielddecl (i,_)) => i) efielddecllist
	val constructors = String.concatWith " | " fieldnames

    in
	"datatype " ^ id ^ " = " ^ constructors ^ "\n"
    end

and compile_gentype t = 
    case t of
	Double => "real"
      | Float => "real"
      | Int32 => "Int32.int"
      | Int64 => "Int64.int"
      | Uint32 => "Int64.int"
      | Uint64 => raise Unimplemented
      | Bool => "bool"
      | String => "string"
      | Bytes => "Word8.word list"
      | Unit => "unit"
      | UserT (q, id) => String.concatWith "." (q @ [id])

end

end
