% Using Base grammar for Rust
include "rust.grm" % Note: different from the innersource rust grammar

define func_return_entry
 [stringlit] '-> [id]
end define

define var_type_entry
  [id] '-> [id]
end define

% Main translation rule to match Rust programs
function main
    export FuncReturnMap [repeat func_return_entry]  
    % TODO: this can be extended with predefined funcs/user defined funcs
      "file_stem" -> 'Some
      "std :: str :: from_utf8" -> 'Ok
      ".parent" -> 'Some
      ".ok" -> 'Ok
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
      "file_stem" -> 'Some      
      ".lock" -> 'Some
      ".last" -> 'Some
      ".next" -> 'Some
    export TempVarNo [number]
      0
    export VarTypeTable [repeat var_type_entry]
      _
    export ArithInconsistentVars [repeat id]
      _
    % export UseWrappingTag [id]
    %  'no
    replace [program] 
	    RustProgram [program]
    construct Message [id]
		  _ [message "Congratulations, matched it successfully!"]
    by
	    RustProgram 
          [fixUnwrapUsedOnLetStmt1] % TEST: let _foo = read_to_string("address.txt").unwrap();
          [fixUnwrapUsedOnLetStmt2] % TEST: let source = read_to_string(file).ok().unwrap();
          [fixUnwrapUsedOnLetStmt3] % unwrap is in between and belong to Infix_Postfix_Expressions*
          % [fixUnwrapUsedOnLetStmt3] 
          [fixUnwrapUsedOnExprStmt1]
          [fixUnwrapUsedOnExprStmt2]
          [fixUnwrapUsedOnExprStmt3]
          [fixUnwrapUsedOnTokenTree] % 处理unwrap在token tree中 let file_name = path
                                     % .join(format!("{}.rs.1",path.file_stem().unwrap().to_string_lossy()));
          % 需要统计unwrap used cases (i.e., stmt) in idiomatic projects, 
          % So that we mainly fix in this selected cases with most occurrences (since unwrap can occur everywhere)
          [fixExhaustiveStruct]
          [fixAsConversion]
          
          %[fixIntArithmetic]拆成下面三个
          [collectVarDefinedType]
          [collectTypeInconsistentArithmetic]
          [importWrapping] 
          [modifyVarDefInWrapping] % modify var type in Wrapping
end function

rule fixUnwrapUsedOnLetStmt1
  import FuncReturnMap [repeat func_return_entry]
  replace $ [repeat Statement]
    Stmt [Statement] Stmts [repeat Statement]
  deconstruct Stmt
    LetStmt [LetStatement]
  deconstruct not Stmt % ensure will only closes siblings
    _ [Expression] 
  deconstruct LetStmt
    OuterAttr [OuterAttribute*] 'let Pat [Pattern] ColonType [COLON_Type?] '= RightExpr [Expression] ';

  % whether unwrap is in the end
  deconstruct RightExpr
    PrefixExpr [Prefix_Expressions*] ExprWOWB [ExpressionWithOrWithoutBlock] 
    InPoExprs [Infix_Postfix_Expressions*]
  construct InPoExprsLength [number]
    _ [length InPoExprs]
  construct LastFuncStartIndex [number]
    InPoExprsLength [- 1]
  construct LastFuncEndIndex [number]
    InPoExprsLength [- 1]

  construct LastFuncName [Infix_Postfix_Expressions*]
    InPoExprs [select LastFuncStartIndex LastFuncEndIndex]
  construct LastFuncStr [stringlit]
	  _ [quote LastFuncName]
  where
    LastFuncStr [= ".unwrap"] 
  where
    InPoExprsLength [= 3] % ("address.txt") .unwrap ()

  deconstruct RightExpr
    _ [Prefix_Expressions*] _ [ExpressionWithOrWithoutBlock]  % ExprWOWB is fs :: read_to_string
    '( OptionalParams [CallParams?] ') '.
    'unwrap() 
  construct CloselyPreFunc [stringlit]
	  _ [quote ExprWOWB]
  deconstruct * [func_return_entry] FuncReturnMap
    CloselyPreFunc -> EnclosePat [id]

  construct IfLetExprBlock [ExpressionStatement]
    'if 'let EnclosePat '( Pat ') '= PrefixExpr ExprWOWB '( OptionalParams ')
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
  deconstruct not Stmt % ensure will only closes siblings
    _ [Expression] 
  deconstruct LetStmt
    OuterAttr [OuterAttribute*] 'let Pat [Pattern] ColonType [COLON_Type?] '= RightExpr [Expression] ';
  
   % whether unwrap is in the end
  deconstruct RightExpr
    PrefixExpr [Prefix_Expressions*] ExprWOWB [ExpressionWithOrWithoutBlock] 
    InPoExprs [Infix_Postfix_Expressions*]
  construct InPoExprsLength [number]
    _ [length InPoExprs]
  construct LastFuncStartIndex [number]
    InPoExprsLength [- 1]
  construct LastFuncEndIndex [number]
    InPoExprsLength [- 1]

  construct LastFuncName [Infix_Postfix_Expressions*]
    InPoExprs [select LastFuncStartIndex LastFuncEndIndex]
  construct LastFuncStr [stringlit]
	  _ [quote LastFuncName]
  where
    LastFuncStr [= ".unwrap"] 
  where not
    InPoExprsLength [= 3] % .ok() .unwrap ()

  construct PreFuncStartIndex [number]
    InPoExprsLength [- 3]
  construct PreFuncEndIndex [number]
    InPoExprsLength [- 3]

  construct CloseFuncName [Infix_Postfix_Expressions*]
    InPoExprs [select PreFuncStartIndex PreFuncEndIndex]

  construct CloselyPreFunc [stringlit]
	  _ [quote CloseFuncName]
  
  deconstruct * [func_return_entry] FuncReturnMap
    CloselyPreFunc -> EnclosePat [id]

  construct Until [number]
    InPoExprsLength [- 2] %.ok
  construct removelastPara [Infix_Postfix_Expressions*]
    InPoExprs [head Until]

  construct IfLetExprBlock [ExpressionStatement]
    'if 'let  EnclosePat '( Pat ') '= PrefixExpr ExprWOWB removelastPara
    '{
    Stmts
    '}
  by 
    IfLetExprBlock
end rule

rule fixUnwrapUsedOnLetStmt3
  import FuncReturnMap [repeat func_return_entry]
  replace $ [repeat Statement]
    Stmt [Statement] Stmts [repeat Statement]
  deconstruct Stmt
    LetStmt [LetStatement]
  deconstruct not Stmt % ensure will only closes siblings
    _ [Expression] 
  deconstruct LetStmt
    OuterAttr [OuterAttribute*] 'let Pat [Pattern] ColonType [COLON_Type?] '= RightExpr [Expression] ';

  % whether contains unwrap
  deconstruct * [PathExprSegment] RightExpr
    'unwrap Colon [COLON_COLON_GenericArgs?]
  
  % unwrap is not in the end
  deconstruct RightExpr
    PrefixExpr [Prefix_Expressions*] ExprWOWB [ExpressionWithOrWithoutBlock] 
    InPoExprs [Infix_Postfix_Expressions*]
  construct InPoExprsLength [number]
    _ [length InPoExprs]
  construct LastFuncStartIndex [number]
    InPoExprsLength [- 1]
  construct LastFuncEndIndex [number]
    InPoExprsLength [- 1]

  construct LastFuncName [Infix_Postfix_Expressions*]
    InPoExprs [select LastFuncStartIndex LastFuncEndIndex]
  construct LastFuncStr [stringlit]
	  _ [quote LastFuncName]
  where not
    LastFuncStr [= ".unwrap"] 

  export IncreIndex [number]
    0

  construct UnwrapIndexes [repeat number]
	  _ [checkUnwrapIndex1 each InPoExprs]  % TODO: 这里是unwrap 在第三个repeat里

  %construct Message [id]
  %  _ [message UnwrapIndexes]

  deconstruct UnwrapIndexes
    II [number] III [number*]
  construct preII [number]
    II [- 1]
  construct postII [number]
    II [+ 2]
  construct LeftSlices [Infix_Postfix_Expressions*]
    InPoExprs [head preII]
  
  construct rightSlices [Infix_Postfix_Expressions*]
    InPoExprs [tail postII]
  
  export IncreIndex 
    0
  
  construct TempStr [id]
	  _ [quote 't_] [quote Pat]

  construct IPreFuncIndex [number]
    II [- 2]
  construct IPreFunc [Infix_Postfix_Expressions*]
    InPoExprs [select IPreFuncIndex IPreFuncIndex]
  deconstruct IPreFunc
    CloselyPreFuncName [Infix_Postfix_Expressions] _ [Infix_Postfix_Expressions*]

  %construct Message1 [id]
  % _ [message CloselyPreFuncName]

  construct CloselyPreFunc [stringlit]
	  _ [quote CloselyPreFuncName]
  deconstruct * [func_return_entry] FuncReturnMap
    CloselyPreFunc -> EnclosePat [id]
  
  by  
    'if 'let EnclosePat '( TempStr ') = PrefixExpr ExprWOWB LeftSlices
    '{
      'let Pat '= TempStr rightSlices ';
      %'let nihao '= '2 ';
    Stmts
    '}
end rule

function checkUnwrapIndex1 InPoExpr [Infix_Postfix_Expressions]
  import IncreIndex [number]
  export IncreIndex
    IncreIndex [+ 1]
  deconstruct InPoExpr
    '.unwrap
  replace [repeat number]
    Indexes [repeat number]
  by
    Indexes [. IncreIndex]
end function

function checkUnwrapIndex2 TokenTree [TokenTree]
  import IncreIndex [number]
  export IncreIndex
    IncreIndex [+ 1]
  deconstruct TokenTree
    'unwrap
  replace [repeat number]
    Indexes [repeat number]
  by
    Indexes [. IncreIndex]
end function

rule fixUnwrapUsedOnExprStmt1
  import FuncReturnMap [repeat func_return_entry]
  import TempVarNo [number]
  export TempVarNo
    TempVarNo [+ 1]
  replace $ [repeat Statement]
    Stmt [Statement] Stmts [repeat Statement]
  deconstruct Stmt
    ExprStmt [ExpressionStatement]
  
  deconstruct not Stmt % ensure will only closes siblings
    _ [Expression] 
  
  %Find smallest expr containing unwrap and unwrap is in the end
  deconstruct * [Expression] ExprStmt
    PrefixExpr [Prefix_Expressions*] ExprWOWB [ExpressionWithOrWithoutBlock] 
    InPoExpr1 [Infix_Postfix_Expressions] '.unwrap  _ [Infix_Postfix_Expressions]
    %InPoExprs [Infix_Postfix_Expressions*]
  
  %(construct InPoExprsStr [stringlit]
    _ [quote InPoExprs]
  construct InPoExprsStrLength [number]
    _ [# InPoExprsStr]
  construct InPoExprsStrLastStartIndex [number]
    InPoExprsStrLength [- 10]
  construct InPoExprsLastStr [stringlit]
    InPoExprsStr [: InPoExprsStrLastStartIndex InPoExprsStrLength]
  where 
    InPoExprsStr [= ".unwrap ()"])%
  
  %check tail is .unwrap

  construct UnwrapBigPartStr [stringlit]
    _ [unparse ExprWOWB] [+ " "] [unparse InPoExpr1] [+ ".unwrap ()"]

  %find unwrap occurs
  construct ExprStmtStr [stringlit]
    _ [unparse ExprStmt]

  construct UnwrapBigPartIndex [number] 
    _ [index ExprStmtStr UnwrapBigPartStr]

  %construct Message [id]
  %  _ [message UnwrapBigPartIndex]
  deconstruct not UnwrapBigPartIndex
    0
  construct LeftEndIndex [number]
    UnwrapBigPartIndex [- 1]
  construct leftPart [stringlit]
    ExprStmtStr [: 1 LeftEndIndex]
  
  construct ExprStmtStrLength [number]
    _ [# ExprStmtStr] 

  construct IfLetPosStr [stringlit]
    _ [unparse ExprWOWB] [+ " "] [unparse InPoExpr1]
  construct IfLetPos [id]
    _ [+ IfLetPosStr]

  construct UnwrapBigPartStrLength [number]
    _ [# UnwrapBigPartStr]

  construct RightStartIndex [number]
    UnwrapBigPartIndex [+ UnwrapBigPartStrLength] 
  construct rightPart [stringlit]
    ExprStmtStr [: RightStartIndex ExprStmtStrLength]
  
  construct TempVarStr [id]
	  _ [quote 't_] [quote TempVarNo]

  construct NewExprStmt [stringlit]
    _ [+ leftPart] [quote 't_] [quote TempVarNo] [+ rightPart]

  construct NewExpr [id]
    _ [+ NewExprStmt]
  
  construct CloselyPreFunc [stringlit]
	  _ [quote ExprWOWB]
  deconstruct * [func_return_entry] FuncReturnMap
    CloselyPreFunc -> EnclosePat [id]
  export TempVarNo
    TempVarNo [+ 1]
  by
    'if 'let EnclosePat '( TempVarStr ') '= IfLetPos
    '{
      NewExpr
      Stmts
    '}
end rule
%unwrap occurs in which part

rule fixUnwrapUsedOnExprStmt2
  import FuncReturnMap [repeat func_return_entry]
  import TempVarNo [number]
  export TempVarNo
    TempVarNo [+ 1]
  replace $ [repeat Statement]
    Stmt [Statement] Stmts [repeat Statement]
  deconstruct Stmt
    ExprStmt [ExpressionStatement]
  deconstruct not Stmt % ensure will only closes siblings
    _ [Expression] 
  
  %Find smallest expr containing unwrap and unwrap is in the end
  deconstruct * [Expression] ExprStmt
    PrefixExpr [Prefix_Expressions*] ExprWOWB [ExpressionWithOrWithoutBlock] 
    InPoExprs1 [Infix_Postfix_Expressions] InPoExprs2 [Infix_Postfix_Expressions] '.unwrap  _ [Infix_Postfix_Expressions]

  construct UnwrapBigPartStr [stringlit]
    _ [unparse PrefixExpr] [+ " "] [unparse ExprWOWB] [unparse InPoExprs1] [+ " "] [unparse InPoExprs2] [+ ".unwrap ()"]

  %find unwrap occurs
  construct ExprStmtStr [stringlit]
    _ [unparse ExprStmt]

  construct UnwrapBigPartIndex [number] 
    _ [index ExprStmtStr UnwrapBigPartStr]

  %construct Message [id]
  %  _ [message UnwrapBigPartIndex]

  deconstruct not UnwrapBigPartIndex
    0
  construct LeftEndIndex [number]
    UnwrapBigPartIndex [- 1]
  construct leftPart [stringlit]
    ExprStmtStr [: 1 LeftEndIndex]
  
  construct ExprStmtStrLength [number]
    _ [# ExprStmtStr] 

  construct IfLetPosStr [stringlit]
    _ [unparse PrefixExpr] [unparse ExprWOWB] [unparse InPoExprs1] [unparse InPoExprs2]
  construct IfLetPos [id]
    _ [+ IfLetPosStr]

  construct UnwrapBigPartStrLength [number]
    _ [# UnwrapBigPartStr]

  construct RightStartIndex [number]
    UnwrapBigPartIndex [+ UnwrapBigPartStrLength] 
  construct rightPart [stringlit]
    ExprStmtStr [: RightStartIndex ExprStmtStrLength]
  
  construct TempVarStr [id]
	  _ [quote 't_] [quote TempVarNo]

  construct NewExprStmt [stringlit]
    _ [+ leftPart] [quote 't_] [quote TempVarNo] [+ rightPart]

  construct NewExpr [id]
    _ [+ NewExprStmt]
  
  construct CloselyPreFunc [stringlit]
	  _ [quote InPoExprs1]
  deconstruct * [func_return_entry] FuncReturnMap
    CloselyPreFunc -> EnclosePat [id]
  export TempVarNo
    TempVarNo [+ 1]
  by
    'if 'let EnclosePat '( TempVarStr ') '= IfLetPos
    '{
      NewExpr
      Stmts
    '}
end rule

rule fixUnwrapUsedOnExprStmt3 % 当前先只处理ifExpression, 其他含{}的expression可修改此规则
  import FuncReturnMap [repeat func_return_entry]
  import TempVarNo [number]
  export TempVarNo
    TempVarNo [+ 1]
  replace $ [repeat Statement]
    IfExprStmt [IfExpression] Stmts [repeat Statement]
  deconstruct IfExprStmt
    'if ExcptExpr[ExpressionExceptStructExpression] BlockExpr [BlockExpression] ElseExpr [ElseExpression?]
  deconstruct ExcptExpr
    PrefixExpr [Prefix_Expressions*] ExprWOWB [ExpressionWithOrWithoutBlock] 
    InPoExprs [Infix_Postfix_Expressions*]

  export IncreIndex [number]
    0
  
  construct UnwrapIndexes [repeat number]
	  _ [checkUnwrapIndex1 each InPoExprs]

  deconstruct UnwrapIndexes
    II [number] III [number*]
  construct preII [number]
    II [- 1]
  construct postII [number]
    II [+ 2]
  construct LeftSlices [Infix_Postfix_Expressions*]
    InPoExprs [head preII]
  
  construct rightSlices [Infix_Postfix_Expressions*]
    InPoExprs [tail postII]

  export IncreIndex 
    0
  construct IPreFuncIndex [number]
    II [- 2]
  construct IPreFunc [Infix_Postfix_Expressions*]
    InPoExprs [select IPreFuncIndex IPreFuncIndex]
  deconstruct IPreFunc
    CloselyPreFuncName [Infix_Postfix_Expressions] _ [Infix_Postfix_Expressions*]

  %construct Message1 [id]
  % _ [message CloselyPreFuncName]

  construct CloselyPreFunc [stringlit]
	  _ [quote CloselyPreFuncName]
  deconstruct * [func_return_entry] FuncReturnMap
    CloselyPreFunc -> EnclosePat [id]

  construct TempVarStr [id]
	  _ [quote 't_] [quote TempVarNo]

  export TempVarNo
    TempVarNo [+ 1]

  by  
    'if 'let EnclosePat '( TempVarStr ') = PrefixExpr ExprWOWB LeftSlices
    '{
      'if TempVarStr rightSlices BlockExpr ElseExpr
      Stmts
    '}

end rule

rule fixUnwrapUsedOnTokenTree %find the most closed pre comma
  % check in let expression
  import FuncReturnMap [repeat func_return_entry]
  import TempVarNo [number]
  export TempVarNo
    TempVarNo [+ 1]
  replace $ [repeat Statement]
    Stmt [Statement] Stmts [repeat Statement]
  deconstruct Stmt
    LetStmt [LetStatement]
  deconstruct not Stmt % ensure will only closes siblings
    _ [Expression] 
  deconstruct LetStmt
    OuterAttr [OuterAttribute*] 'let Pat [Pattern] ColonType [COLON_Type?] '= RightExpr [Expression] ';

   % whether contains unwrap
  deconstruct * [token_or_key] RightExpr
    'unwrap

  construct RightExprStr [stringlit]
    _ [unparse RightExpr]
  construct UnwrapStartIndex [number] 
    _ [index RightExprStr ".unwrap ()"]
  deconstruct not UnwrapStartIndex
    0
  construct UnwrapRightStartIndex [number] 
    UnwrapStartIndex [+ 10]
  construct CommaStartIndex [number] 
    _ [index RightExprStr ","]  % string between comma and unwrap will be put in if let expr
  deconstruct not CommaStartIndex
    0

  deconstruct * [TokenTree*] RightExpr
    TTs [TokenTree*]

  export IncreIndex [number]
    0
  construct UnwrapIndexes [repeat number]
	  _ [checkUnwrapIndex2 each TTs] 

  deconstruct UnwrapIndexes
    II [number] III [number*]

  export IncreIndex 
    0

  construct IPreFuncIndex [number]
    II [- 3]
  construct IPreFunc [TokenTree*]
    TTs [select IPreFuncIndex IPreFuncIndex]
  deconstruct IPreFunc
    CloselyPreFuncName [TokenTree] _ [TokenTree*]
  
  construct CloselyPreFunc [stringlit]
	  _ [quote CloselyPreFuncName]
  deconstruct * [func_return_entry] FuncReturnMap
    CloselyPreFunc -> EnclosePat [id]

  construct TempVarStr [stringlit]
	  _ [quote 't_] [quote TempVarNo]
  construct TempVarId [id]
	  _ [+ TempVarStr]

  construct leftSlices [stringlit]
    RightExprStr [: 1 CommaStartIndex]
  construct RightExprLength [number]
    _ [# RightExprStr]
  construct rightSlices [stringlit]
    RightExprStr [: UnwrapRightStartIndex RightExprLength]

  construct auxiliaryNum1 [number]
    CommaStartIndex [+ 2]
  construct auxiliaryNum2 [number]
    UnwrapRightStartIndex [- 11]
  construct RightExprStrInIfLet [stringlit]
    RightExprStr [: auxiliaryNum1 auxiliaryNum2]
  construct RightExprInIfLet [id]
    _ [+ RightExprStrInIfLet]

  construct NewRightStr [stringlit]
    leftSlices [+ TempVarStr] [+ rightSlices]
  construct NewRight [id]
    _ [+ NewRightStr]
  export TempVarNo
    TempVarNo [+ 1]
  by
    'if 'let EnclosePat '( TempVarId ') = RightExprInIfLet
    '{
      'let Pat = NewRight ;
      Stmts
    '}
end rule

rule fixExhaustiveStruct
  replace $ [Item]
    OuterAttr [OuterAttribute*] VisOrMacroItem [VisItem_or_MacroItem]
  deconstruct VisOrMacroItem
    'pub _ [Struct]
  deconstruct not OuterAttr
    '# '[ 'non_exhaustive ']
  construct NewOuterAttr [OuterAttribute]
    '# '[ 'non_exhaustive ']
  by
    NewOuterAttr
    OuterAttr
    VisOrMacroItem 
end rule

rule fixAsConversion
  import TempVarNo [number]
  export TempVarNo
    TempVarNo [+ 1]
  replace $ [repeat Statement]
    Stmt [Statement] Stmts [repeat Statement]
  deconstruct not Stmt % ensure will only closes siblings
    _ [Expression]
  
  construct AllExprs [repeat Expression]
    _ [^ Stmt]  % 最后一个Expression must be the closest and innermost one
  construct NumberOfExprs [number]
    _ [length AllExprs]
  construct LastExprRepeat [repeat Expression]
    AllExprs [tail NumberOfExprs] 
  deconstruct LastExprRepeat
    LastExpr [Expression] _ [repeat Expression]
  
  %find substring containing as
  construct LastExprStr [stringlit]
     _ [quote LastExpr]
  construct AsStartIndex [number] 
    _ [index LastExprStr "as"]  % string between comma and unwrap will be put in if let expr
  deconstruct not AsStartIndex
    0  % We find this as expression

  construct AsSubLeftStrEndIndex [number]
    AsStartIndex [- 2]
  construct AsSubLeftstr [stringlit]
    LastExprStr [: 1 AsSubLeftStrEndIndex]
  construct AsSubLeftId [id]
	  _ [+ AsSubLeftstr]
  construct AsSubRightStrStartIndex [number]
    AsStartIndex [+ 3]
   construct LastExprStrLength [number]
    _ [# LastExprStr]
  construct AsSubRightstr [stringlit]
    LastExprStr [: AsSubRightStrStartIndex LastExprStrLength]
  construct AsSubRightId [id]
	  _ [+ AsSubRightstr]

  construct StmtStr [stringlit]
    _ [quote Stmt]
  construct AsExprStartIndex [number]
    _ [index StmtStr LastExprStr]
  deconstruct not AsExprStartIndex
    0

  construct AsExprLeftStrIndex [number]
    AsExprStartIndex [- 1]
  construct AsExprLeftStr [stringlit]
    StmtStr [: 1 AsExprLeftStrIndex] 
  
  construct AsRightExprStrStartIndex [number]
    AsExprStartIndex [+ LastExprStrLength]
  construct StmtStrLength [number]
    _ [# StmtStr]
  construct AsExprRightstr [stringlit]
    StmtStr [: AsRightExprStrStartIndex StmtStrLength]

  construct TempVarStr [stringlit]
	  _ [quote 't_] [quote TempVarNo]
  construct TempVarId [id]
	  _ [+ TempVarStr]

  construct NewStmtStr [stringlit]
    _ [+ AsExprLeftStr] [+ TempVarStr] [+ AsExprRightstr]

  construct NewStmtId [id]
    _ [+ NewStmtStr]
  
  export TempVarNo
    TempVarNo [+ 1]

  by
    'if 'let 'Ok '( TempVarId ') = AsSubRightId ':: 'try_from '( AsSubLeftId ') 
    '{
      NewStmtId
      Stmts
    '}
end rule

rule collectVarDefinedType
  import VarTypeTable [repeat var_type_entry]
  replace $ [LetStatement]
    LetStmt [LetStatement]
  deconstruct LetStmt
    _ [OuterAttribute*] 'let Pat [id] ColonType [COLON_Type?] '= RightExpr [Expression] ';
  deconstruct ColonType
    ': TypeId [id] 
  construct VarTypeEntry [var_type_entry]
    Pat -> TypeId
  export VarTypeTable
    VarTypeTable [. VarTypeEntry]
  %construct Message [id]
  %  _ [message VarTypeTable]
  by
    LetStmt
end rule

rule collectTypeInconsistentArithmetic %dispose a+b, don't touch a+b+b
  import VarTypeTable [repeat var_type_entry]
  replace $ [Expression]
    PreExpr [Prefix_Expressions*] LeftVar [id] ArithExpr [Infix_ArithmeticOrLogicalExpression]
  where
     ArithExpr [_isAddOp] [_isMinusOp] [_isMultiplyOp] [isDivideOp] [isModOp]
  import RightVar [id]
  
  deconstruct * [var_type_entry] VarTypeTable
     LeftVar -> LeftVarType [id]
  deconstruct * [var_type_entry] VarTypeTable
     RightVar -> RightVarType [id]
  %construct Message [id]
  %  _ [message LeftVarType]
  where LeftVarType [~= RightVarType]

  import ArithInconsistentVars [repeat id]
  export ArithInconsistentVars
    ArithInconsistentVars [. LeftVar] [. RightVar]
  %construct Message [id]
  %  _ [message ArithInconsistentVars]
  by 
    PreExpr LeftVar ArithExpr
end rule 

rule importWrapping
  import ArithInconsistentVars [repeat id]
  replace $ [Crate]
    Items [Item*]
  construct Length [number]
    _ [length ArithInconsistentVars]
  where 
    Length [> 0]
  by 
    'use  'std::num::Wrapping;
    Items
end rule

rule modifyVarDefInWrapping
  import ArithInconsistentVars [repeat id]
  replace $ [LetStatement]
    LetStmt [LetStatement]
  deconstruct LetStmt
    OuterAttrs [OuterAttribute*] 'let Pat [id] ColonType [COLON_Type?] '= RightExpr [Expression] ';

  %check pat is in ArithInconsistentVars
  deconstruct * [id] ArithInconsistentVars
    Pat
    
  by
    OuterAttrs 'let Pat '= 'Wrapping '( RightExpr  ') ';
end rule

function _isAddOp
    match [Infix_ArithmeticOrLogicalExpression]
      '+  RightVar [id]
    export RightVar
end function

function _isMinusOp
    match [Infix_ArithmeticOrLogicalExpression]
      '-  RightVar [id]
    export RightVar
end function

function _isMultiplyOp
    match [Infix_ArithmeticOrLogicalExpression]
      '*  RightVar [id]
    export RightVar
end function

function isDivideOp
    match [Infix_ArithmeticOrLogicalExpression]
      '/  RightVar [id]
    export RightVar
end function

function isModOp
    match [Infix_ArithmeticOrLogicalExpression]
      '%  RightVar [id]
    export RightVar
end function




% 可用as conversion's idea rewrite the transformations for unwraps

% rule 会在每次子树替换后自动搜索整个新树

