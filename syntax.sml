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

datatype modifier = Required | Optional | Repeated

datatype gentype = Double | Float | Int32 | Int64 | Uint32 
		  | Uint64 | Bool | String | Bytes | Unit 
		  | UserT of identifier
	
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

end
