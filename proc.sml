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

structure Proc = struct

fun import_path_to_string (fl: string list) = 
    "Import trace is:\n" ^ (String.concatWith "\n" fl)

fun realpath f = (SOME (OS.FileSys.realPath f)) handle _ => NONE

exception ImportPathNotResolved
exception ImportPathCycle
exception ImportNotParsed
exception DuplicatePackageDeclaration


datatype protofiletree = ProtoFileTree of Syntax.package * Syntax.proto * protofiletree list
(* If Syntax.package = [], then no package was specified *)

(* Convert a protofiletree to string by a post-order traversal *)

fun protofiletree_to_string (ProtoFileTree (pkg, proto, tl)): string = 
    let val s1 = String.concatWith "" (List.map protofiletree_to_string tl)
	val s2 = Syntax.package_to_string pkg
	val s3 = Syntax.proto_to_string proto
    in
	s1 ^ s2 ^ s3 ^ "--------\n"
    end


(* Process a file by converting its imports into a tree
   structure. *)

fun expand_paths (fl: string list) (dl: Syntax.proto): protofiletree = 
    case dl of
	[] => ProtoFileTree (Syntax.Package [], [], [])
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
	     (* Package declaration *)
	     let val (ProtoFileTree (pkg', proto, tree)) = expand_paths fl dl
	     in
		 case pkg' of
		     Syntax.Package [] => ProtoFileTree (pkg, proto, tree)
		   | Syntax.Package _ => 
		     (print ("More than one package declaration in file: " ^ (hd fl)  ^ "\n" ^
			     (import_path_to_string fl));
		      raise DuplicatePackageDeclaration
		     )
	     end
	   | _ => 
	     (* Non-import, non-package declaration *)
	     let val (ProtoFileTree (pkg, proto, tree)) = expand_paths fl dl
	     in
		 ProtoFileTree (pkg, d :: proto, tree)
	     end
	)


(* Check that the field numbers are all unique *)

exception DuplicateFieldNumber

local
    open Syntax 
in
fun check_fn_proto [] = ()
  | check_fn_proto (d :: dl) =
    (case d of
	 PackageD _ => ()
       | ImportD _ => ()
       | MessageD md => check_fn_messagedecl md
       | EnumD ed => check_fn_enumdecl ed
       | ServiceD _ => ();
     check_fn_proto dl
    )
and check_fn_messagedecl (Messagedecl (_, fielddecllist)) =
    check_fn_fielddecllist [] fielddecllist

and check_fn_fielddecllist inlist [] = ()
  | check_fn_fielddecllist inlist ((TypedeclF (_, _, _, n)) :: dl) =
    if (List.exists (fn i => i = n) inlist)
    then 
	(print ("Duplicate field number: " ^ (Int.toString n) ^
		" in message declaration\n");
	 raise DuplicateFieldNumber)
    else 
	check_fn_fielddecllist (n :: inlist) dl
  | check_fn_fielddecllist inlist ((MessagedeclF md) :: dl) =
    (check_fn_messagedecl md; check_fn_fielddecllist inlist dl)
  | check_fn_fielddecllist inlist ((EnumdeclF ed) :: dl) =
    (check_fn_enumdecl ed; check_fn_fielddecllist inlist dl)

and check_fn_enumdecl (Enumdecl (_, efielddecllist)) =
    check_fn_efielddecllist [] efielddecllist

and check_fn_efielddecllist inlist [] = ()
  | check_fn_efielddecllist inlist ((efl as (Efielddecl (_, n))) :: dl) =
    if (List.exists (fn i => i = n) inlist)
    then 
	(print ("Duplicate field number: " ^ (Int.toString n) ^
		" in enum declaration\n");
	 raise DuplicateFieldNumber)
    else 
	check_fn_efielddecllist (n :: inlist) dl
end (* local *)


exception DuplicateSymbolDefinition
(* checks that name occurs in root exactly once. throws DuplicateSymbolDefinition if it appears twice *)
fun find_symbol (qual, name: Syntax.qualifier*Syntax.identifier) (root: protofiletree) (found: bool) : bool = (
    case root of
        ProtoFileTree (pkg, [], []) => found
      | ProtoFileTree (pkg, d :: dl, subtree) => (
            case d of 
                (Syntax.MessageD (Syntax.Messagedecl (ident, _))) =>
                let
                  val found' = check_dup (qual, name) (pkg, ident) found
                in
                    find_symbol (qual, name) (ProtoFileTree (pkg, dl, subtree)) found'
                end
               | (Syntax.EnumD (Syntax.Enumdecl (ident, _))) => 
                let
                  val found' = check_dup (qual, name) (pkg, ident) found
                in
                    find_symbol (qual, name) (ProtoFileTree (pkg, dl, subtree)) found'
                end
               | _ => find_symbol (qual, name) (ProtoFileTree (pkg_opt, dl, subtree)) found
            )
      | ProtoFileTree (pkg, [], (child :: subtree)) =>
          let
            val found' = find_symbol (qual, name) child found
          in
            find_symbol (qual, name) (ProtoFileTree (pkg, [], subtree)) found'
          end
    )
(* checks whether s matches s', and if it does, that this is not a duplicated definition *)
and check_dup (qual, ident) (qual', ident') found =
let
  fun qualify q i = String.concatWith "." (q @ [i])
  val qi = qualify qual ident
  val qi' = qualify qual' ident'
in
  if qi = qi' andalso found then raise DuplicateSymbolDefinition
  else (qi = qi' orelse found)
end
