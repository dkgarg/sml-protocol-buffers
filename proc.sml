(*

Copyright (c) <2011> <Deepak Garg garg.deepak@gmail.com> 

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

structure Proc = struct

type packeddecl = Syntax.package * Syntax.proto

fun packeddecl_to_string (pkg, dl) = 
    (Syntax.package_to_string pkg) ^
    (Syntax.proto_to_string dl)

fun import_path_to_string (fl: string list) = 
    "Import trace is:\n" ^ (String.concatWith "\n" fl)

fun check_no_package (dl: Syntax.proto) = 
    case dl of
	[] => true
      | (Syntax.PackageD _) :: _ => false
      | _ :: dl' => check_no_package dl'

exception DuplicatePackageDeclaration

fun extract_package (fl: string list) (dl: Syntax.proto): packeddecl =
    case dl of
	[] => (Syntax.Package [], [])
      | (Syntax.PackageD pck) :: dl' =>
	if (check_no_package dl')
	then 
	    (pck, dl')
	else
	    (print ("More than one package declaration in file: " ^ (hd fl) ^ "\n" ^
		    (import_path_to_string fl));
	     raise DuplicatePackageDeclaration
	    )
      | d :: dl' =>
	let val (pck, dl'') = extract_package fl dl'
	in
	    (pck, d :: dl'')
	end

fun realpath f = (SOME (OS.FileSys.realPath f)) handle _ => NONE

exception ImportPathNotResolved
exception ImportPathCycle
exception ImportNotParsed

(* Output the imports of a file recursively. The first argument is the
list of files that have been seen in outer contexts, in reverse order,
i.e., most recent context is first. *)

fun expand_paths (fl: string list) (dl: Syntax.proto): packeddecl list = 
    case dl of
	[] => []
      | (d :: dl) =>
	(case d of
	     Syntax.ImportD (Syntax.Import i) => 
	     (* Import declaration; expand *)
	     (case (realpath i) of
		  NONE => (print ("Could not resolve import path: " ^ i ^ "\n" ^
				  (import_path_to_string fl));
			   raise ImportPathNotResolved)
			  
		| SOME path =>
		  if (List.exists (fn f => f = path) fl)
		  then 
		      (* Recursive import! *)
		      (print ("Path: " ^ path ^ " imported recursively\n" ^
			      (import_path_to_string fl));
		       raise ImportPathCycle)
		  else
		      (case PBParser.parse_pb_file path of
			   NONE => (print ("Could not not parse imported file: " ^ path ^ "\n" ^
					   (import_path_to_string fl));
				    raise ImportNotParsed)
				   
			 | SOME dl' => 
			   let val (pkg, dl'') = extract_package (path :: fl) dl'
			   in
			       (pkg, dl'') :: ((expand_paths (path :: fl) dl'') @ (expand_paths fl dl))
			   end
		      )
	     )
	   | _ => 
	     (* Non-import declaration; continue *)
	     expand_paths fl dl
	)


end
