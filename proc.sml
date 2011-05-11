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

fun import_path_to_string (fl: string list) = 
    "Import trace is:\n" ^ (String.concatWith "\n" fl)

fun realpath f = (SOME (OS.FileSys.realPath f)) handle _ => NONE

exception ImportPathNotResolved
exception ImportPathCycle
exception ImportNotParsed
exception DuplicatePackageDeclaration


datatype protofiletree = ProtoFileTree of Syntax.package Option.option * Syntax.proto * protofiletree list

(* Output the imports of a file recursively. The first argument is the
list of files that have been seen in outer contexts, in reverse order,
i.e., most recent context is first. *)

fun expand_paths (fl: string list) (dl: Syntax.proto): protofiletree = 
    case dl of
	[] => ProtoFileTree (NONE, [], [])
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
			   let val ret' = expand_paths (path :: fl) dl'
			       val (ProtoFileTree (pkg_opt, proto, tree)) = expand_paths fl dl
			   in
			       ProtoFileTree (pkg_opt, proto, ret' :: tree)
			   end
		      )
	     )
	   | Syntax.PackageD pkg =>
	     let val (ProtoFileTree (pkg_opt, proto, tree)) = expand_paths fl dl
	     in
		 case pkg_opt of
		     NONE => ProtoFileTree (SOME pkg, proto, tree)
		   | SOME _ => 
		     (print ("More than one package declaration in file: " ^ (hd fl) ^ "\n" ^
			     (import_path_to_string fl));
		      raise DuplicatePackageDeclaration
		     )
	     end
	   | _ => 
	     (* Non-import, non-package declaration; continue *)
	     let val (ProtoFileTree (pkg_opt, proto, tree)) = expand_paths fl dl
	     in
		 ProtoFileTree (pkg_opt, d :: proto, tree)
	     end
	)

exception DuplicateSymbolDefinition
(* checks that name occurs in root exactly once. throws DuplicateSymbolDefinition if it appears twice *)
fun find_symbol name (root : protofiletree) (found : bool) : bool = (
    case root of
        ProtoFileTree (pkg_opt, [], []) => found
      | ProtoFileTree (pkg_opt, d :: dl, subtree) => (
            case d of 
                (Syntax.MessageD (Syntax.Messagedecl (ident, _))) =>
                    find_symbol name (ProtoFileTree (pkg_opt, dl, subtree)) (check_dup ident name found)
               | (Syntax.EnumD (Syntax.Enumdecl (ident, _))) => 
                    find_symbol name (ProtoFileTree (pkg_opt, dl, subtree)) (check_dup ident name found)
               | _ => find_symbol name (ProtoFileTree (pkg_opt, dl, subtree)) found
            )
      | ProtoFileTree (pkg_opt, [], (child :: subtree)) =>
          let
            val found' = find_symbol name child found
          in
            find_symbol name (ProtoFileTree (pkg_opt, [], subtree)) found'
          end
    )
(* checks whether s matches s', and if it does, that this is not a duplicated definition *)
and check_dup ident ident' found = (
  if ident = ident' andalso found then raise DuplicateSymbolDefinition
  else (ident = ident' orelse found)
  )
end
