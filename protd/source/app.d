import std.stdio;
import pegged.grammar;

mixin(grammar(`
Gram:

TopLevel < (ImportDecl / FunctionDef / GlobalLetDef / TypeDef)+

ImportDecl < :"import" ModuleName :";"
ModuleName < ModuleNameSpace (:"." ModuleNameSpace)*
ModuleNameSpace < Symbol

GlobalLetDef < LetMutDef / LetImmDef
LetMutDef < :"let" :"mut" Symbol TypeAnnotation? :"=" Expr
LetImmDef < :"let" Symbol TypeAnnotation? :"=" Expr

TypeDef <  RecordTypeDecl / ADTTypeDeclare

RecordTypeDecl < :"type" Symbol :"=" :"{" (Symbol TypeAnnotation :";")+ :"}"
ADTTypeDeclare < :"type" ADTBaseConstructor :"=" ADTConstructorList
ADTBaseConstructor < ADTTypeNameWithArgs / ADTTypeNameWithoutArgs
ADTTypeName <~ !ADTKeyword [A-Z_] [a-zA-Z0-9_]*
ADTField < ADTFieldOfDelegate / ADTFieldOfFunction / ADTFieldOfArray / ADTFieldWithArgs / ADTFieldName
ADTFieldArgs < :"(" :")" / :"(" ADTField (:"," ADTField)* :")"
ADTFieldWithArgs < ADTFieldName :"!" ADTFieldArgs
ADTFieldOfArray < (ADTFieldWithArgs / ADTFieldName) ADTArrayBracket+
ADTFieldOfDelegate < :"[" ADTField (:"->" ADTField)+ :"]"
ADTFieldOfFunction < :"<" ADTField (:"->" ADTField)+ :">"
ADTFieldName <~ !ADTKeyword [a-zA-Z_] [a-zA-Z0-9_]*
ADTArrayBracket < ADTUnsizedBracket / ADTSizedBracket
ADTUnsizedBracket < :"[":"]"
ADTSizedBracket < :"[" ADTArraySize :"]"
ADTArraySize <~ [a-zA-Z0-9_]*
ADTTypeNameWithoutArgs < ADTTypeName
ADTTypeNameWithArgs < ADTTypeName ADTParameterList
ADTParameterList < :"(" :")" / :"(" ADTTypeName ("," ADTTypeName)* :")"
ADTConstructorWithField < :"|"? ADTTypeName :"of" ADTField (:"*" ADTField)*
ADTConstructor <  :"|"? ADTTypeName
ADTConstructorDeclare < ADTConstructorWithField / ADTConstructor
ADTConstructorList < ADTConstructorDeclare+
ADTKeyword <~ "of"
ADTInteger <~ digit+

FunctionDef < :"fn" Symbol TemplateArgumentsDefinition? ParameterList TypeAnnotation Block

Block < :"{" Expr* :"}"


Expr < (ParenExpr / ReturnExpr / LetExpr / IfExpr / ForExpr / WhileExpr / LoopExpr / UFCSStyleFuncCallExpr / CallExpr / MathExpr / Variable / Literal) :";"?
ParenExpr < :"(" Expr :")"
LetExpr < LetMutExpr / LetImmExpr
LetMutExpr < :"let" :"mut" Symbol TypeAnnotation? :"=" Expr :"in" Expr
LetImmExpr < :"let" Symbol TypeAnnotation? :"=" Expr :"in" Expr
IfExpr < :"if" Expr :"then" Expr :"else" Expr
ForExpr < :"for" Expr :"to" Expr :"do" Expr
WhileExpr < :"while" Expr :"do"
LoopExpr < :"loop" Expr
UFCSStyleFuncCallExpr < Expr (:"." CallExpr)+
CallExpr < TemplateFunctionCall / FunctionCall
TemplateFunctionCall < Symbol TemplateArguments FunctionCallArguments
FunctionCall < Symbol FunctionCallArguments
FunctionCallArguments < :"(" :")" / :"(" Expr (:"," Expr)* :")"

MathExpr <- Factor AddOrSubExpr*
AddOrSubExpr  <- AddExpr / SubExpr
AddExpr < :"+" Factor
SubExpr < :"-" Factor
Factor   <- Primary MulOrDivExpr*
MulOrDivExpr  <- MulExpr / DivExpr
MulExpr < :"*" Primary
DivExpr < :"/" Primary
Primary  <- '(' Expr ')'/ Expr / '-' Primary

ReturnExpr < :"return" Expr

TypeAnnotation < :":" TypeSpec
TypeSpec < ArrowType / BasicTypes / Symbol
ArrowType < TypeSpec :"->" TypeSpec
BasicTypes < IntType #/ FloatType / StringType / CharType / ByteType / ShortType / LongType
IntType < SizedInt / DefaultInt
SizedInt < :"int" TemplateArguments
DefaultInt < :"int"
#FloatType <  / StringType / CharType / ByteType / ShortType / LongType

TemplateArgumentsDefinition < :"<" TemplateArgument (:"," TemplateArgument)* :">"
TemplateArguments < :"!" :"(" TemplateArgument (:"," TemplateArgument)* :")"
TemplateArgument < Expr

ParameterList < :"(" :")" / :"(" Parameter (:"," Parameter)* :")"
Parameter < ArgumentParameter
ArgumentParameter < Symbol TypeAnnotation

Literal < IntegerLiteral / StringLiteral

IntegerLiteral <~ digit+
Identifier <~ !Keyword [a-zA-Z_] [a-zA-Z0-9_]*
Keyword <- "fn" / "let" / "mut" / "if" / "then" / "else" / "for" / "while" / "loop" / "do" / "to" / "true" / "false" / "import" / "type"
StringLiteral <~ doublequote (DQChar)* doublequote
DQChar <- EscapeSequence / !doublequote .
EscapeSequence <~ backslash ( quote
                            / doublequote
                            / backslash
                            / [abfnrtv]
                            )

Symbol < Identifier
Variable < Identifier
`));

import std.path;
import std.file;

void main() {
  import std.experimental.logger;
  import pegged.peg;

  version (tracer) {
    sharedLog = new TraceLogger("TraceLog.txt");
    setTraceConditionFunction((string ruleName, ref const ParseTree _) => true);
  }

  foreach (entry; dirEntries("../examples/", SpanMode.shallow)) {
    writeln("test for : ", entry);
    string text = readText(entry);
    ParseTree t = Gram(text);
    if (t.successful) {
      t.writeln;
    } else {
      throw new Exception(t.toString);
    }
  }
}
