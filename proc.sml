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
			   NONE => (print ("Could not parse imported file: " ^ path ^ "\n" ^
					   (import_path_to_string fl));
				    raise ImportNotParsed)
				   
			 | SOME dl' => 
			   let val ret' = expand_paths (path :: fl) dl'
			       val (ProtoFileTree (pkg', proto, tree)) = expand_paths fl dl
			   in
			       ProtoFileTree (pkg', proto, ret' :: tree)
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


(* Check that field numbers and declared nested types are all
   unique. This does not check for duplication of declared identifiers at
   the top level *)

exception DuplicateFieldNumber
exception DuplicateIdentifier

local
    open Syntax 
in
fun check_unique_proto [] = ()
  | check_unique_proto (d :: dl) =
    (case d of
	 PackageD _ => ()
       | ImportD _ => ()
       | MessageD md => check_unique_messagedecl md
       | EnumD ed => check_unique_enumdecl ed
       | ServiceD _ => ();
     check_unique_proto dl
    )
and check_unique_messagedecl (Messagedecl (_, fielddecllist)) =
    check_unique_fielddecllist [] [] fielddecllist

and check_unique_fielddecllist numlist idlist [] = ()
  | check_unique_fielddecllist numlist idlist ((TypedeclF (id, _, _, n)) :: dl) =
    if (List.exists (fn i => i = n) numlist)
    then 
	(print ("Duplicate field number: " ^ (Int.toString n) ^
		" in message declaration\n");
	 raise DuplicateFieldNumber)
    else if (List.exists (fn id' => id = id') idlist)
    then 
	(print ("Duplicate identifier declaration: " ^ (Syntax.identifier_to_string id) ^
		" in message declaration\n");
	 raise DuplicateIdentifier)
    else
	check_unique_fielddecllist (n :: numlist) (id :: idlist) dl
  | check_unique_fielddecllist numlist idlist ((MessagedeclF (md as Messagedecl(id, _))) :: dl) =
    (check_unique_messagedecl md; 
     if (List.exists (fn id' => id = id') idlist)
     then 
	 (print ("Duplicate identifier declaration: " ^ (Syntax.identifier_to_string id) ^
		 " in message declaration\n");
	  raise DuplicateIdentifier)
     else
	 check_unique_fielddecllist numlist (id :: idlist) dl
    )
  | check_unique_fielddecllist numlist idlist ((EnumdeclF (ed as Enumdecl(id, _))) :: dl) =
    (check_unique_enumdecl ed; 
     if (List.exists (fn id' => id = id') idlist)
     then 
	 (print ("Duplicate identifier declaration: " ^ (Syntax.identifier_to_string id) ^
		 " in message declaration\n");
	  raise DuplicateIdentifier)
     else
	 check_unique_fielddecllist numlist (id :: idlist) dl
    )

and check_unique_enumdecl (Enumdecl (_, efielddecllist)) =
    check_unique_efielddecllist [] [] efielddecllist

and check_unique_efielddecllist numlist idlist [] = ()
  | check_unique_efielddecllist numlist idlist ((efl as (Efielddecl (id, n))) :: dl) =
    if (List.exists (fn i => i = n) numlist)
    then 
	(print ("Duplicate field number: " ^ (Int.toString n) ^
		" in enum declaration\n");
	 raise DuplicateFieldNumber)
    else if (List.exists (fn id' => id = id') idlist)
    then
	(print ("Duplicate identifier declaration: " ^ (Syntax.identifier_to_string id) ^
		" in message declaration\n");
	 raise DuplicateIdentifier)
    else
	check_unique_efielddecllist (n :: numlist) (id :: idlist) dl

fun check_unique_protofiletree (ProtoFileTree (_, dl, tl)) = 
    (check_unique_proto dl;
     List.foldr (fn (t, ()) => check_unique_protofiletree t) () tl
    )
    
end (* local *)



structure Set: sig
    type ''a set
    exception AlreadyExists
    val empty: ''a set
    val add: ''a set -> ''a -> ''a set
    val exists: ''a set -> ''a -> bool
    val toList: ''a set -> ''a list
end = 
struct

type ''a set = (''a, unit) KeyMap.keymap

exception AlreadyExists

val empty = KeyMap.empty

fun add s a = (KeyMap.add s a ()) 
    handle KeyMap.AlreadyExists => raise AlreadyExists

fun exists s a = case KeyMap.find s a of
		     NONE => false
		   | SOME () => true

fun toList s = List.map #1 (KeyMap.toList s)
end



(* Construct a list of qualified identifiers declared at the top level in a protofiletree *)


exception Unimplemented

fun list_ids_proto (p: Syntax.proto) (qual_opt: Syntax.qualifier Option.option)
    : (Syntax.qualifier * Syntax.identifier) Set.set = raise Unimplemented


(* exception DuplicateSymbolDefinition *)
(* (\* checks that name occurs in root exactly once. throws DuplicateSymbolDefinition if it appears twice *\) *)
(* fun find_symbol (qual, name: Syntax.qualifier*Syntax.identifier) (root: protofiletree) (found: bool) : bool = ( *)
(*     case root of *)
(*         ProtoFileTree (pkg, [], []) => found *)
(*       | ProtoFileTree (pkg, d :: dl, subtree) => ( *)
(*             case d of  *)
(*                 (Syntax.MessageD (Syntax.Messagedecl (ident, _))) => *)
(*                 let *)
(*                   val found' = check_dup (qual, name) (pkg, ident) found *)
(*                 in *)
(*                     find_symbol (qual, name) (ProtoFileTree (pkg, dl, subtree)) found' *)
(*                 end *)
(*                | (Syntax.EnumD (Syntax.Enumdecl (ident, _))) =>  *)
(*                 let *)
(*                   val found' = check_dup (qual, name) (pkg, ident) found *)
(*                 in *)
(*                     find_symbol (qual, name) (ProtoFileTree (pkg, dl, subtree)) found' *)
(*                 end *)
(*                | _ => find_symbol (qual, name) (ProtoFileTree (pkg_opt, dl, subtree)) found *)
(*             ) *)
(*       | ProtoFileTree (pkg, [], (child :: subtree)) => *)
(*           let *)
(*             val found' = find_symbol (qual, name) child found *)
(*           in *)
(*             find_symbol (qual, name) (ProtoFileTree (pkg, [], subtree)) found' *)
(*           end *)
(*     ) *)
(* (\* checks whether s matches s', and if it does, that this is not a duplicated definition *\) *)
(* and check_dup (qual, ident) (qual', ident') found = *)
(* let *)
(*   fun qualify q i = String.concatWith "." (q @ [i]) *)
(*   val qi = qualify qual ident *)
(*   val qi' = qualify qual' ident' *)
(* in *)
(*   if qi = qi' andalso found then raise DuplicateSymbolDefinition *)
(*   else (qi = qi' orelse found) *)
(* end *)

end
