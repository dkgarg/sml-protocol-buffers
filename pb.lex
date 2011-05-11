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

%let opencomment="(*";
%let closecomment="*)";
%let openlinecomment="%";

%let iden=({letter}|"_")({letter}|{special}|{digit})*;
%let integer=(("~") | ("-"))?{digit}+;

%let date=({digit}+":"{digit}+":"{digit}+)(":"{digit}+":"{digit}+":"{digit}+)? ;

%let str="\""{validstrvalue}*"\"";

%let eoflimiter = "%#parse_end#%";

%let start_formula = "%#parse_formula#%";
%let start_term = "%#parse_term#%";


%defs (
  open FolTokens
  type lex_result = token
  fun eof() = EOF

  fun extractstring s = String.substring (s, 1, (String.size s) - 2)


  fun toMonth (1) = Date.Jan
  | toMonth (2) = Date.Feb
  | toMonth (3) = Date.Mar
  | toMonth (4) = Date.Apr
  | toMonth (5) = Date.May
  | toMonth (6) = Date.Jun
  | toMonth (7) = Date.Jul
  | toMonth (8) = Date.Aug
  | toMonth (9) = Date.Sep
  | toMonth (10) = Date.Oct
  | toMonth (11) = Date.Nov
  | toMonth (12) = Date.Dec
  | toMonth n = toMonth (n mod 12 + 1)
  
  fun string2date s = 
  let val sl = String.tokens (fn (#":") => true | _ => false) s
      val ints = map (Option.valOf o Int.fromString) sl
      val ints2 = if (length(ints) = 6) then ints else ints @ [0,0,0]
      val arr = Array.fromList ints2
  in
	 Date.toTime (Date.date ({year = Array.sub(arr,0), month = toMonth(Array.sub(arr,1)),
				  day = Array.sub(arr,2), hour = Array.sub(arr, 3), 
				  minute = Array.sub(arr,4), second = Array.sub(arr, 5),
		                  offset = NONE}))
	(* offset = NONE means that we are measuring local time. This means that to convert 
           back from Time.time to Date.date we must use Date.fromTimeLocal, not 
           Date.fromUTC. 
         *)
  end


);

%states COMMENT LINECOMMENT;


<INITIAL> {ws} => (continue());
{eoflimiter} => (eof());

<INITIAL> {start_formula} => (START_FORMULA);
<INITIAL> {start_term} => (START_TERM);

<INITIAL> {opencomment} => (YYBEGIN(COMMENT); continue());
<COMMENT> {closecomment} => (YYBEGIN(INITIAL); continue());
<COMMENT> .  => (continue());

<INITIAL> {openlinecomment} => (YYBEGIN(LINECOMMENT); continue());
<LINECOMMENT> {endline} => (YYBEGIN(INITIAL); continue());
<LINECOMMENT> . => (continue());

<INITIAL> ":" => (COLON);
<INITIAL> "(" => (LPAREN);
<INITIAL> ")" => (RPAREN);
<INITIAL> "[" => (LSQ);
<INITIAL> "]" => (RSQ);
<INITIAL> "{" => (LBR);
<INITIAL> "}" => (RBR);
<INITIAL> "<" => (LA);
<INITIAL> ">" => (RA);
<INITIAL> all => (LIT_ALL);
<INITIAL> ex => (LIT_EX);
<INITIAL> and => (LIT_AND);
<INITIAL> or => (LIT_OR);
<INITIAL> imp => (LIT_IMP);
<INITIAL> true => (LIT_TRUE);
<INITIAL> false => (LIT_FALSE);
<INITIAL> any => (LIT_ANY);
<INITIAL> date => (LIT_DATE);
<INITIAL> int => (LIT_INT);
<INITIAL> string => (LIT_STRING);
<INITIAL> COMPILE => (LIT_COMPILE);

<INITIAL> {integer} => (INT (Option.valOf (Int.fromString yytext)));
<INITIAL> {date} => (DATE (string2date yytext));

<INITIAL> {str} => (STR(extractstring (yytext)));

<INITIAL> {iden} => (ID (yytext));

<INITIAL> . => (print ("Unknown symbol: " ^ yytext ^ "\n"); continue());

