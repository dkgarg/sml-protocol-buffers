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

%name PBLexer;

%let ws = [\ \t\n\r];
%let validstrvalue = [^\"\n];
%let endline = [\n\r];

%let stringlimit = "\"";
%let digit = [0-9];
%let lletter = [a-z];
%let uletter = [A-Z];
%let letter = {uletter}|{lletter};
%let special = "_"|"'"|"@"|"-"|"$"|"#"|"/";
%let genchar={lletter}|{uletter}|{digit}|{special};

%let openlinecomment="%";

%let iden=({letter}|"_")({letter}|{special}|{digit})*;
%let integer=("-")?{digit}+;

%let date=({digit}+":"{digit}+":"{digit}+)(":"{digit}+":"{digit}+":"{digit}+)? ;

%let str={stringlimit}{validstrvalue}*{stringlimit};

%let eoflimiter = "%#parse_end#%";


%defs (
  open PBTokens
  type lex_result = token
  fun eof() = EOF

  fun extractstring s = String.substring (s, 1, (String.size s) - 2)


);

%states LINECOMMENT;


<INITIAL> {ws} => (continue());
{eoflimiter} => (eof());

<INITIAL> {openlinecomment} => (YYBEGIN(LINECOMMENT); continue());
<LINECOMMENT> {endline} => (YYBEGIN(INITIAL); continue());
<LINECOMMENT> . => (continue());

<INITIAL> "message" => (MESSAGE);
<INITIAL> "import" => (IMPORT);
<INITIAL> "enum" => (ENUM);
<INITIAL> "required" => (REQUIRED);
<INITIAL> "optional" => (OPTIONAL);
<INITIAL> "repeated" => (REPEATED);
<INITIAL> "package" => (PACKAGE);
<INITIAL> "." => (DOT);
<INITIAL> "=" => (EQ);
<INITIAL> ";" => (SEMICOLON);
<INITIAL> "{" => (LBRACE);
<INITIAL> "}" => (LBRACE);
<INITIAL> "service" => (SERVICE);
<INITIAL> "(" => (LPAREN);
<INITIAL> ")" => (RPAREN);
<INITIAL> "rpc" => (RPC);
<INITIAL> "returns" => (RETURNS);
<INITIAL> "double" => (DOUBLE);
<INITIAL> "float" => (FLOAT);
<INITIAL> "int32" => (INT32);
<INITIAL> "int64" => (INT64);
<INITIAL> "uint32" => (UINT32);
<INITIAL> "uint64" => (UINT64);
<INITIAL> "bool" => (BOOL);
<INITIAL> "string" => (STRING);
<INITIAL> "bytes" => (BYTES);
<INITIAL> "unit" => (UNIT);

<INITIAL> {str} => (STR (extractstring (yytext)));

<INITIAL> {iden} => (ID (yytext));

<INITIAL> {integer} => (FIELDNUMBER (Option.valOf (Int.fromString yytext)));

<INITIAL> . => (print ("Unknown symbol: " ^ yytext ^ "\n"); continue());

