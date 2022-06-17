open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = match toks with
|(Tok_Let)::t -> let rest = (match_token toks (Tok_Let)) in
		(parse_let rest)
|(Tok_Fun)::t -> let rest = match_token toks (Tok_Fun) in
		parse_fun rest
|(Tok_If)::t -> (parse_if toks) (*let rest = match_token toks (Tok_If) in
		parse_if rest*)
|_ -> parse_or toks
 

and parse_let tok = 
	let is_rec = parse_rec tok in
	let new_tok = if is_rec then (match_token tok (Tok_Rec)) else tok in
	match lookahead new_tok with
	|Some Tok_ID(x)-> let drop_id = match_token new_tok (Tok_ID(x)) in
		let drop_equals = match_token drop_id Tok_Equal in
		let (t',exp') = parse_expr drop_equals in
		let drop_in = match_token t' Tok_In in
		let (t'',exp'') = parse_expr drop_in in
		(t'', Let(x,is_rec,exp',exp''))
	|_ -> raise (InvalidInputException("Parse Let failed"))


and parse_fun tok = match tok with
|Tok_ID(x)::t -> let drop_id = match_token tok (Tok_ID(x)) in
		let drop_arrow = match_token drop_id Tok_Arrow in
		let (t',exp) = parse_expr drop_arrow in
		(t', Fun(x,exp))
|_ -> raise (InvalidInputException("Parse Fun failed"))


and parse_if tok = (match tok with
|Tok_If::t -> let drop_if = match_token tok (Tok_If) in
		let (t',exp') = parse_expr drop_if in
		let drop_then = match_token t' (Tok_Then) in
		let (t'',exp'') = parse_expr drop_then in
		let drop_else = match_token t'' (Tok_Else) in
		let (t''',exp''') = parse_expr drop_else in
		(t''', If(exp',exp'',exp'''))
|_ -> raise (InvalidInputException("Parse If failed")))


and parse_rec tok = 
	match tok with
	|Tok_Rec::t -> true
	|_ -> false


and parse_or tok = 
	let (t',exp) = (parse_and tok) in
	match t' with
	|Tok_Or::t -> let (t'',exp'') = (parse_or (match_token t' Tok_Or)) in
		(t'',Binop(Or,exp,exp''))
	|_ -> (t',exp)

and parse_and tok = 
	let (t',exp) = (parse_equals tok) in
	match t' with
	|Tok_And::t -> let (t'',exp') = (parse_and (match_token t' Tok_And)) in
		 (t'', Binop(And,exp,exp'))
	|_ -> (t',exp)

and parse_relational tok = 
	let (t',exp) = (parse_add tok) in
	match t' with
	|Tok_Less::t -> let (t'',exp') = (parse_relational (match_token t' Tok_Less))
		in (t'',Binop(Less,exp,exp'))
	|Tok_Greater::t -> let (t'',exp') = (parse_relational (match_token t' Tok_Greater)) in
		(t'',Binop(Greater,exp,exp'))
	|Tok_LessEqual::t -> let (t'',exp') = (parse_relational (match_token t' Tok_LessEqual)) in
		(t'',Binop(LessEqual,exp,exp'))
|Tok_GreaterEqual::t -> let (t'',exp') = (parse_relational (match_token t' Tok_GreaterEqual)) in
		(t'',Binop(GreaterEqual,exp,exp'))
	|_ -> (t',exp)


and parse_equals tok = 
	let (t',exp) = (parse_relational tok) in
	match t' with
	|Tok_Equal::t -> let (t'',exp') = (parse_equals (match_token t' Tok_Equal)) in 
		(t'',Binop(Equal,exp,exp'))
	|Tok_NotEqual::t -> let (t'',exp') = (parse_equals (match_token t' Tok_NotEqual)) in
		(t'', Binop(NotEqual,exp,exp'))
	|_ -> (t',exp)


and parse_add tok = 
	let (t',exp) = (parse_multiply tok) in
	match t' with
	|Tok_Add::t -> let (t'',exp') = (parse_relational (match_token t' Tok_Add)) in
		(t'',Binop(Add,exp,exp'))
	|Tok_Sub::t -> let (t'',exp') = (parse_relational (match_token t' Tok_Sub)) in
		(t'',Binop(Sub,exp,exp'))
	|_ -> (t',exp)

and parse_multiply tok = 
	let (t',exp) = (parse_concat tok) in
	match t' with
	|Tok_Mult::t -> let (t'',exp') = (parse_multiply (match_token t' Tok_Mult)) in
		(t'', Binop(Mult,exp,exp'))
	|Tok_Div::t -> let (t'',exp') = (parse_multiply (match_token t' Tok_Div)) in
		(t'',Binop(Div,exp,exp')) 
	| _ -> (t',exp)


and parse_concat tok = 
	let (t',exp) = (parse_unary tok) in
	match t' with
	|Tok_Concat::t -> let (t'',exp') = (parse_concat (match_token t' Tok_Concat)) in
		(t'',Binop(Concat,exp,exp'))
	|_ -> (t',exp) 

and parse_unary tok = 
	match tok with
	|Tok_Not::t -> let (t',exp) = (parse_unary (match_token tok Tok_Not)) in
		(t',Not(exp))
	|_ ->(parse_functionCall tok)


and parse_functionCall tok = 
	let (t',exp) = parse_primary tok in
	match t' with
	|Tok_Int(x)::t -> let (t'',exp') = parse_primary t' in
		(t'', FunctionCall(exp,exp'))
	|Tok_Bool(x)::t -> let (t'',exp') = parse_primary t' in
		(t'',FunctionCall(exp,exp'))
	|Tok_String(x)::t -> let (t'',exp') = parse_primary t' in
		(t'',FunctionCall(exp,exp'))
	|Tok_ID(x)::t -> let (t'',exp') = parse_primary t' in 
		(t'',FunctionCall(exp,exp'))
	|Tok_LParen::t -> let (t'',exp') = parse_primary t' in
		(t'',FunctionCall(exp,exp'))
	|_ -> (t',exp)


and parse_primary tok = 
	match tok with
	|Tok_Int(x)::t -> let rest = (match_token tok (Tok_Int(x))) in
		(rest,Value(Int(x)))
	|Tok_Bool(x)::t -> let rest = (match_token tok (Tok_Bool(x))) in 
		(rest,Value(Bool(x)))
	|Tok_String(x)::t -> let rest = match_token tok (Tok_String(x)) in
		(rest,Value(String(x)))
	|Tok_ID(x)::t -> let rest = match_token tok (Tok_ID(x)) in 
		(rest, ID(x))
	|Tok_LParen::t -> let drop_lparen = match_token tok Tok_LParen in
		let (t',exp) = parse_expr drop_lparen in
		let drop_rparen = match_token t' Tok_RParen in
		(drop_rparen, exp)
	|t -> raise (InvalidInputException("Parse primary failed"))

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = (match toks with
|Tok_Def::t -> let drop_def = match_token toks Tok_Def in
		(match drop_def with
		|Tok_ID(x)::t -> let drop_id = (match_token drop_def (Tok_ID(x))) in
			let drop_equal = match_token drop_id Tok_Equal in
			let (t',exp) = parse_expr drop_equal in
			let (a,b) = (match t' with
			|Tok_DoubleSemi::t -> let drop_semi = match_token t' Tok_DoubleSemi in
			 (drop_semi,exp)
			|_ -> raise (InvalidInputException("No semi in Def"))) in (a,Def(x,b)) 
		|_ -> raise (InvalidInputException "No ID in Def"))
|Tok_DoubleSemi::t -> let (t',exp) = parse_DoubleSemi toks in (t',NoOp)
|t -> let (t',exp) = parse_expr t in
		(match t' with
		|Tok_DoubleSemi::t -> let drop_semi = match_token t' (Tok_DoubleSemi) in (drop_semi,Expr(exp))
		|_ -> raise (InvalidInputException "No double-semi in Def")))

and parse_DoubleSemi tok = match tok with
|Tok_DoubleSemi::t -> ([],match_token tok Tok_DoubleSemi)
|_ -> raise (InvalidInputException "Parse Double-Semi failure")
