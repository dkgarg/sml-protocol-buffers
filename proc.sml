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

type idset = (Syntax.qualifier * Syntax.identifier) Set.set

exception Unimplemented

(* goes through the file, adding each declaration to the set with the given qualifier *)
fun list_ids_proto (set: idset) (qual: Syntax.qualifier) (p: Syntax.proto) : idset = 
    List.foldr (fn (d, set') => list_ids_decl set' qual d) set p

(* adds the declaration's name to the set *)
and list_ids_decl set qual d = 
    case d of
	Syntax.PackageD _ => set
      | Syntax.ImportD _ => set
      | Syntax.MessageD (Syntax.Messagedecl (id, fl)) => 
	((Set.add set (qual, id)) 
	 handle Set.AlreadyExists => 
		raise DuplicateIdentifier
	)
      | Syntax.EnumD (Syntax.Enumdecl (id, fl)) =>
        ((Set.add set (qual, id))
         handle Set.AlreadyExists =>
		raise DuplicateIdentifier
	)
      | Syntax.ServiceD _ => set

(* traverses over a protofiletree, and adds the qualified name of each variable to the set *)
fun list_ids_protofiletree (set : idset) (ProtoFileTree (Syntax.Package pkg, proto, pl): protofiletree) : idset =
let
  val set' = list_ids_proto set pkg proto
in
  List.foldr (fn (p, set'') => list_ids_protofiletree set'' p) set' pl
end



exception UnboundIdentifier
exception PrematureQualifier
local
  type qualifiedname = Syntax.qualifier * Syntax.identifier
  type context = qualifiedname Set.set
in
  fun add_message_decls (Syntax.Messagedecl (ident, nil)) (vars: context) = vars
    | add_message_decls (Syntax.Messagedecl (ident, d :: dl)) (vars: context) = (
        case d of 
            Syntax.MessagedeclF (Syntax.Messagedecl (ident', _)) =>
            let
              val vars' = (Set.add vars (nil, ident')) handle Set.AlreadyExists => vars
            in
              add_message_decls (Syntax.Messagedecl (ident, dl)) vars'
            end
          | Syntax.EnumdeclF (Syntax.Enumdecl (ident', _)) =>
            let
              val vars' = (Set.add vars (nil, ident')) handle Set.AlreadyExists => vars
            in
              add_message_decls (Syntax.Messagedecl (ident, dl)) vars'
            end
          |  _ => 
            add_message_decls (Syntax.Messagedecl (ident, dl)) vars 
       )

  (* Checks that all of the UserT field declarations in the specified message are properly defined.
   * pkg refers to the package containing the message.
   * vars is assumed to contain all of the declarations in that file, without qualification *)
  fun check_closed_message 
    ((Syntax.Package pkg) : Syntax.package) 
    (Syntax.Messagedecl (ident, dl): Syntax.messagedecl) 
    (vars: context) =
  let
    val vars' = add_message_decls (Syntax.Messagedecl (ident, dl)) vars
    fun loop nil = ()

      (* check the user-type declaration against the map *)
      | loop ((Syntax.TypedeclF (_, _, Syntax.UserT (qual, ident'), _)) :: dl) =
          if (Set.exists vars' (qual, ident')) then loop dl
          else (print ("unbound identifier " ^ (String.concatWith "." (qual @ [ident'])) ^ "\n"); raise UnboundIdentifier)

      (* everything else is built in *)
      | loop ((Syntax.TypedeclF _) :: dl) = loop dl

      (* recursively check this message decl *)
      | loop ((Syntax.MessagedeclF md) :: dl) = (check_closed_message (Syntax.Package pkg) md vars'; loop dl)

      (* enums don't contain any inner user types *)
      | loop ((Syntax.EnumdeclF _) :: dl) = loop dl
  in
    loop dl
  end

  fun check_closed_file ((pkg, proto) : (Syntax.package * Syntax.proto)) (vars : context) : unit =
  let
    (* add to the context the declarations in this file. if the file has no package name, this will
    * throw an exception, and we can just ignore it. *)
    val vars' = (list_ids_proto vars nil proto) handle DuplicateIdentifier => vars
    fun loop nil = ()
      | loop ((Syntax.MessageD md) :: dl) = (check_closed_message pkg md vars'; loop dl)
      | loop (_ :: dl) = loop dl
  in
    loop proto
  end

  fun check_closed_protofiletree (root as ProtoFileTree (pkg, proto, pl): protofiletree) =
  let
    (*val vars = list_ids_protofiletree Set.empty root *)
    val vars = foldr (fn (p, set) => list_ids_protofiletree set p) Set.empty pl
    val _ = check_closed_file (pkg, proto) vars
  in
    (List.map check_closed_protofiletree pl; ())
  end


  exception MissingPackageDeclaration

  fun check_has_packages _ = ()
  (*
  fun check_has_packages (ProtoFileTree ([], _, pl)) = raise MissingPackageDeclaration
    | check_has_packages (ProtoFileTree (_, _, pl)) = 
      (List.map check_has_packages pl;
       ()
      )
  *)
end

end
