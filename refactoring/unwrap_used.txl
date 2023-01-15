% Using Base grammar for Rust
include "rust.grm"

define func_return_entry
 [stringlit] '-> [id]
end define

% Main translation rule to match Rust programs
function main
    export FuncReturnMap [repeat func_return_entry]  
    % TODO: this can be extended with predefined funcs/user defined funcs
      ".ok" -> 'Ok
      "std :: fs :: read_to_string" -> 'Ok
      "fs :: read_to_string" -> 'Ok
      "read_to_string" -> 'Ok
      "spawn" -> 'Ok
      "wait_with_output" -> 'Ok
      "wait" -> 'Ok
      "parse_query" -> 'Ok
      "try_from" -> 'Ok
      "from_utf8"  -> 'Ok
      "take" -> 'Some
      ".get" -> 'Some
      "splitup" -> 'Ok
      "parent" -> 'Some
      "file_stem" -> 'Some
    replace [program] 
	    RustProgram [program]
    construct Message [id]
		  _ [message "Congratulations, matched it successfully!"]
    by
	    RustProgram 
          [fixUnwrapUsedOnLetStmt1]
          [fixUnwrapUsedOnLetStmt2]
end function

rule fixUnwrapUsedOnLetStmt1
  import FuncReturnMap [repeat func_return_entry]
  replace $ [repeat Statement]
    Stmt [Statement] Stmts [repeat Statement]
  deconstruct Stmt
    LetStmt [LetStatement]
  deconstruct LetStmt
    OuterAttr [OuterAttribute*] 'let Pat [Pattern] ColonType [COLON_Type?] '= RightExpr [Expression] ';
  deconstruct * [PathExprSegment] RightExpr
    'unwrap Colon [COLON_COLON_GenericArgs?]  % whether unwrap exists
  construct KeepPart [Expression]
    RightExpr [removeUnwrap]
  deconstruct RightExpr
    PrefixExpr [Prefix_Expressions*] ExprWOWB [ExpressionWithOrWithoutBlock]  % ExprWOWB is fs :: read_to_string
    '( OptionalParams [CallParams?] ') '.
    'unwrap() 
  construct CloselyPreFunc [stringlit]
	  _ [quote ExprWOWB]
  deconstruct * [func_return_entry] FuncReturnMap
    CloselyPreFunc -> EnclosePat [id]
  construct IfLetExprBlock [ExpressionStatement]
    'if 'let EnclosePat '( Pat ') '= KeepPart
    '{
    Stmts
    '}
  by 
    IfLetExprBlock
end rule

rule fixUnwrapUsedOnLetStmt2
  import FuncReturnMap [repeat func_return_entry]
  replace $ [repeat Statement]
    Stmt [Statement] Stmts [repeat Statement]
  deconstruct Stmt
    LetStmt [LetStatement]
  deconstruct LetStmt
    OuterAttr [OuterAttribute*] 'let Pat [Pattern] ColonType [COLON_Type?] '= RightExpr [Expression] ';
  deconstruct * [PathExprSegment] RightExpr
    'unwrap Colon [COLON_COLON_GenericArgs?]  % whether unwrap exists

  construct KeepPart [Expression]
    RightExpr [removeUnwrap]

  deconstruct RightExpr
    PrefixExpr [Prefix_Expressions*] ExprWOWB [ExpressionWithOrWithoutBlock] 
    InPoExprs [Infix_Postfix_Expressions*]
  construct PreFuncStartIndex [number]
    _ [length InPoExprs] [- 3]
  construct PreFuncEndIndex [number]
    _ [length InPoExprs] [- 3]
  construct CloseFuncName [Infix_Postfix_Expressions*]
    InPoExprs [select PreFuncStartIndex PreFuncEndIndex]
  construct CloselyPreFunc [stringlit]
	  _ [quote CloseFuncName]
  
  deconstruct * [func_return_entry] FuncReturnMap
    CloselyPreFunc -> EnclosePat [id]

  construct IfLetExprBlock [ExpressionStatement]
    'if 'let  EnclosePat '( Pat ') '= KeepPart
    %'if 'let  EnclosePat '( Pat ') '=  PrefixExpr ExprWOWB CloseFuncName
    '{
    Stmts
    '}
  by 
    IfLetExprBlock
end rule

function removeUnwrap
  replace $ [Expression]
    PrefixExpr [Prefix_Expressions*] ExprWOWB [ExpressionWithOrWithoutBlock] 
    InPoExprs [Infix_Postfix_Expressions*]
  %(construct EmptyRepeat [Infix_Postfix_Expressions*] 
    _)%
  construct InPoExprsRmUnwrap [Infix_Postfix_Expressions*]
    InPoExprs [rebuildExprs] 
  construct Length [number]
    _ [length InPoExprsRmUnwrap] [- 1] % -2 is just reach the goal!!!
  construct removelastPara [Infix_Postfix_Expressions*]
    InPoExprsRmUnwrap [head Length]
  by
    PrefixExpr ExprWOWB removelastPara % already removed "unwrap"
end function

function rebuildExprs
  replace * [repeat Infix_Postfix_Expressions]
	  InPoExprs [repeat Infix_Postfix_Expressions]
  construct NewInPoExprs [repeat Infix_Postfix_Expressions]
	  _ [addNonUnwrap each InPoExprs]
  by
	  NewInPoExprs
end function

function addNonUnwrap InPoExpr [Infix_Postfix_Expressions]
  deconstruct not InPoExpr
	  '.unwrap
  replace [repeat Infix_Postfix_Expressions]
	  InPoExprs [repeat Infix_Postfix_Expressions]
  by
	  InPoExprs [. InPoExpr]
end function



