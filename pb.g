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

%name PB;

%tokens
    : MESSAGE ("message")
    | IMPORT ("import")
    | ENUM ("enum")
    | REQUIRED ("required")
    | OPTIONAL ("optional")
    | REPEATED ("repeated")
    | PACKAGE ("package")
    | DOT (".")
    | EQ ("=")
    | SEMICOLON (";")
    | LBRACE ("{")
    | RBRACE ("}")
    | SERVICE ("service")
    | LPAREN ("(")
    | RPAREN (")")
    | RPC ("rpc")
    | RETURNS ("returns")
    | DOUBLE ("double")
    | FLOAT ("float")
    | INT32 ("int32")
    | INT64 ("int64")
    | UINT32 ("uint32")
    | UINT64 ("uint64")
    | BOOL ("bool")
    | STRING ("string")
    | BYTES ("bytes")
    | UNIT ("unit")
    | ID of string
    | FIELDNUMBER of int
    | STR of string
    ;

%start proto;


proto
    : ( decl )* 
    ;

decl
    : package => (Syntax.PackageD package)
    | import => (Syntax.ImportD import)
    | messagedecl => (Syntax.MessageD messagedecl)
    | enumdecl => (Syntax.EnumD enumdecl)
    | servicedecl => (Syntax.ServiceD servicedecl)
    ;

package
    : "package" packagename ";" => (Syntax.Package packagename)
    ;

packagename
    : ID => ([ID])
    | ID "." packagename => (ID :: packagename)
    ;

import
    : "import" STR ";" => (Syntax.Import STR)
    ;


servicedecl
    : "service" ID "{" rpcdecllist "}" 
        => ( Syntax.Servicedecl (ID, rpcdecllist) )
    ;


rpcdecllist
    : (rpcdecl)*
    ;

rpcdecl
    : "rpc" ID "(" gentype ")" "returns" "(" gentype ")"
        => ( Syntax.Rpcdecl (ID, gentype1, gentype2) )
    ;


messagedecl 
    : "message" ID "{" fielddecllist "}"
        => ( Syntax.Messagedecl (ID, fielddecllist) )
    ;

fielddecllist
    : (fielddecl)*
    ;

fielddecl
    : modifier gentype ID "=" FIELDNUMBER ";"
        => ( Syntax.TypedeclF (ID, modifier, gentype, FIELDNUMBER) )
    | messagedecl => ( Syntax.MessagedeclF messagedecl )
    | enumdecl => ( Syntax.EnumdeclF enumdecl )
    ;

enumdecl
    : "enum" ID "{" efielddecllist "}"
        => ( Syntax.Enumdecl (ID, efielddecllist) )
    ;

efielddecllist 
    : (efielddecl)* 
    ;

efielddecl
    : ID "=" FIELDNUMBER ";"
        => (Syntax. Efielddecl (ID, FIELDNUMBER))
    ;


gentype
    : "double" => ( Syntax.Double )
    | "float" => ( Syntax.Float )
    | "int32" => ( Syntax.Int32 )
    | "int64" => ( Syntax.Int64 )
    | "uint32" => ( Syntax.Uint32 )
    | "uint64" => ( Syntax.Uint64 )
    | "bool" => ( Syntax.Bool )
    | "string" => ( Syntax.String )
    | "bytes" => ( Syntax.Bytes )
    | "unit" => ( Syntax.Unit )
    | ID => ( Syntax.UserT ID )
    ;


modifier
    : "required" => ( Syntax.Required )
    | "optional" => ( Syntax.Optional )
    | "repeated" => ( Syntax.Repeated )
    ;

