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

structure Syntax = struct

type fieldnumber = int
type identifier = string

type qualified = identifier list (* A.B.x, etc. *)

datatype modifier = Required | Optional | Repeated

datatype gentype = Double | Float | Int32 | Int64 | Uint32 
		  | Uint64 | Bool | String | Bytes | Unit 
		  | UserT of (qualifier * identifier)
	
datatype fielddecl = TypedeclF of identifier * modifier * gentype * fieldnumber
		   | MessagedeclF of messagedecl
		   | EnumdeclF of enumdecl

and efielddecl = Efielddecl of identifier * fieldnumber

and messagedecl = Messagedecl of identifier * fielddecl list

and enumdecl = Enumdecl of identifier * efielddecl list


datatype rpcdecl = Rpcdecl of identifier * gentype * gentype
                   (* rp ID (ARGTYPE) returns (RETTYPE) *)

datatype servicedecl = Servicedecl of identifier * rpcdecl list

datatype package = Package of string list 
 (* List is the components of the name separated by "." *)

datatype import = Import of string

datatype decl = PackageD of package 
	      | ImportD of import 
	      | MessageD of messagedecl
	      | EnumD of enumdecl
	      | ServiceD of servicedecl

type proto = decl list 



(* Functions to convert the syntax to string *)

fun map_and_concat (f: 'a -> string) (l: 'a list): string = 
    String.concatWith "" (List.map f l)


fun fieldnumber_to_string i = Int.toString i

fun identifier_to_string id = id

fun modifier_to_string Required = "required"
  | modifier_to_string Optional = "optional"
  | modifier_to_string Repeated = "repeated"

fun qualifier_to_string q = 
    String.concatWith "." q

fun gentype_to_string t =
    case t of
	Double => "double"
      | Float => "float"
      | Int32 => "int32"
      | Int64 => "int64"
      | Uint32 => "uint32"
      | Uint64 => "uint64"
      | Bool => "bool"
      | String => "string"
      | Bytes => "bytes"
      | Unit => "unit"
      | UserT (q,id) => 
	case q of
	    [] => identifier_to_string id
	  | _ => (qualifier_to_string q) ^ "." ^ (identifier_to_string id)


fun whitespace 0 = ""
  | whitespace n = " " ^ (whitespace (n-1))

val std_indent = whitespace 3

fun fielddecl_to_string indent f =
    case f of
	TypedeclF (id, m, t, f) =>
	indent ^ (modifier_to_string m) ^ " " ^ 
	(gentype_to_string t) ^ " " ^ (identifier_to_string id) ^ " = " ^
	(fieldnumber_to_string f) ^ ";\n"

      | MessagedeclF md =>
	messagedecl_to_string indent md

      | EnumdeclF ed =>
	enumdecl_to_string indent ed

and efielddecl_to_string indent (Efielddecl (id, f)) =
    indent ^ (identifier_to_string id) ^ " = " ^ (fieldnumber_to_string f) ^ ";\n"

and messagedecl_to_string indent (Messagedecl(id, fdl)) =
    indent ^ "message " ^ (identifier_to_string id) ^ " {\n" ^
    (map_and_concat (fielddecl_to_string (indent ^ std_indent)) fdl) ^
    indent ^ "}\n"

and enumdecl_to_string indent (Enumdecl (id, efdl)) =
    indent ^ "enum " ^ (identifier_to_string id) ^ " {\n" ^
    (map_and_concat (efielddecl_to_string (indent ^ std_indent)) efdl) ^
    indent ^ "}\n"


fun rpcdecl_to_string indent (Rpcdecl (id, ta, tr)) = 
    indent ^ "rpc " ^ (identifier_to_string id) ^ "(" ^
    (gentype_to_string ta) ^ ") returns (" ^
    (gentype_to_string tr) ^ ");\n"


fun servicedecl_to_string (Servicedecl (id, rl)) =
    "service " ^ (identifier_to_string id) ^ " {\n" ^
    (map_and_concat (rpcdecl_to_string std_indent) rl) ^ 
    "}\n"

fun package_to_string (Package sl) = 
    if sl = [] then "" 
    else
	"package " ^ (String.concatWith "." sl) ^ ";\n"

fun import_to_string (Import s) =
    "import \"" ^ s ^ "\";\n"

fun decl_to_string d =
    case d of
	PackageD p => package_to_string p
      | ImportD i => import_to_string i
      | MessageD m => messagedecl_to_string (whitespace 0) m
      | EnumD e => enumdecl_to_string (whitespace 0) e
      | ServiceD s => servicedecl_to_string s

fun proto_to_string p =
    map_and_concat decl_to_string p

end
