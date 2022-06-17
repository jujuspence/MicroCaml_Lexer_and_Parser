open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 

(*let re_Space1 = Str.regexp " \n\t" in*)
let re_Space = Str.regexp "[ \n\t]+" in
let re_TBool = Str.regexp "true" in
let re_FBool = Str.regexp "false" in
let re_PInt = Str.regexp "[0-9]+" in
let re_NInt = Str.regexp "(-[0-9]+)" in
let re_Str = Str.regexp "\"[^\"]*\"" in 
let re_Id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
let re_LParen = Str.regexp "(" in 
let re_RParen = Str.regexp ")" in
let re_Equal = Str.regexp "=" in
let re_NotEqual = Str.regexp "<>" in
let re_Greater = Str.regexp ">" in
let re_Less = Str.regexp "<" in
let re_GreaterEqual = Str.regexp ">=" in
let re_LessEqual = Str.regexp "<=" in
let re_Or = Str.regexp "||" in
let re_And = Str.regexp "&&" in
let re_Not = Str.regexp "not" in
let re_If = Str.regexp "if " in
let re_Then = Str.regexp "then " in
let re_Else = Str.regexp "else " in
let re_Add = Str.regexp "\\+" in
let re_Sub = Str.regexp "-" in
let re_Mult = Str.regexp "\\*" in
let re_Div = Str.regexp "/" in
let re_Concat = Str.regexp "\\^" in
let re_Let = Str.regexp "let " in
let re_Def = Str.regexp "def " in
let re_In = Str.regexp "in " in
let re_Rec = Str.regexp "rec " in
let re_Fun = Str.regexp "fun " in
let re_Arrow = Str.regexp "->" in
let re_DoubleSemi = Str.regexp ";;" in

let rec tok pos = 

if pos >= String.length input then
	[]
else if Str.string_match re_Space input pos then
        tok (pos+1)
else if (Str.string_match re_PInt input pos) then 
	let val1 = Str.matched_string input in
	Tok_Int(int_of_string val1)::(tok (pos+String.length val1))
else if (Str.string_match re_NInt input pos) then
	let val1 = Str.matched_string input in
	let drop_paren = String.sub val1 1 ((String.length val1) -2) in
	Tok_Int((int_of_string drop_paren))::(tok (pos+String.length val1))

else if (Str.string_match re_LParen input pos) then
	Tok_LParen::(tok (pos+1))
else if Str.string_match re_RParen input pos then
	Tok_RParen::(tok (pos+1))
else if Str.string_match re_Equal input pos then
	Tok_Equal::(tok (pos+1))
else if Str.string_match re_NotEqual input pos then
	Tok_NotEqual::(tok (pos+2))
else if Str.string_match re_Arrow input pos then
        Tok_Arrow::(tok (pos+2))
else if Str.string_match re_Greater input pos then
	Tok_Greater::(tok (pos+1))
else if Str.string_match re_Less input pos then
	Tok_Less::(tok (pos+1))
else if Str.string_match re_GreaterEqual input pos then
	Tok_GreaterEqual::(tok (pos+2))
else if Str.string_match re_LessEqual input pos then
	Tok_LessEqual::(tok (pos+2))
else if Str.string_match re_Or input pos then
	Tok_Or::(tok (pos+2))
else if Str.string_match re_And input pos then
	Tok_And::(tok (pos+2))
else if Str.string_match re_Not input pos then
	Tok_Not::(tok (pos+3))
else if Str.string_match re_If input pos then
	Tok_If::(tok (pos+2))
else if Str.string_match re_Then input pos then
	Tok_Then::(tok (pos+4))
else if Str.string_match re_Else input pos then
	Tok_Else::(tok (pos+4))
else if Str.string_match re_Add input pos then
	Tok_Add::(tok (pos+1))
else if Str.string_match re_Sub input pos then
	Tok_Sub::(tok (pos+1))
else if Str.string_match re_Mult input pos then
	Tok_Mult::(tok (pos+1))
else if Str.string_match re_Div input pos then
	Tok_Div::(tok (pos+1))
else if Str.string_match re_Concat input pos then
	Tok_Concat::(tok (pos+1))
else if Str.string_match re_Let input pos then
	Tok_Let::(tok (pos+3))
else if Str.string_match re_Def input pos then
	Tok_Def::(tok (pos+3))
else if Str.string_match re_In input pos then
	Tok_In::(tok (pos+2))
else if Str.string_match re_Rec input pos then
	Tok_Rec::(tok (pos+3))
else if Str.string_match re_Fun input pos then
	Tok_Fun::(tok (pos+3))
else if Str.string_match re_DoubleSemi input pos then
	Tok_DoubleSemi::(tok (pos+2))
else if Str.string_match re_TBool input pos then
	let val1 = Str.matched_string input in
		Tok_Bool(bool_of_string val1)::(tok (pos+String.length val1))
else if Str.string_match re_FBool input pos then
	let val1 = Str.matched_string input in
		Tok_Bool(bool_of_string val1)::(tok (pos+String.length val1))
else if Str.string_match re_Id input pos then
	let val1 = Str.matched_string input in
		Tok_ID (val1)::(tok (pos+String.length val1))
else if Str.string_match re_Str input pos then
	let val1 = Str.matched_string input in
	let noQuotes = String.sub val1 1 ((String.length val1)-2) in
	(Tok_String noQuotes)::(tok (pos+String.length val1))

(*else if Str.string_match re_Space input pos then 
	tok (pos + Str.match_end())*)
else raise (InvalidInputException "tokenize") 

in tok 0
