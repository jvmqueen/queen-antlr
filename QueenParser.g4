//A Queen grammar for ANTLR 4 based on the Java 8 ANTLR Grammar.
//We start from Java 8 and take out/modify stuff that we do not like!
//
//  TERMINOLOGY
//
//  unann -> unannotated
//  lf    -> looking for
//  dim   -> dimension (array dimension square brackets)
parser grammar QueenParser;

options {
    tokenVocab=QueenLexer;
}
/*
 * Productions from §3 (Lexical Structure)
 */

literal
	:	IntegerLiteral
	|	FloatingPointLiteral
	|	BooleanLiteral
	|	CharacterLiteral
	|	StringLiteral
	|	NullLiteral
	;

/*
 * Productions from §4 (Types, Values, and Variables)
 */

primitiveType
	:	annotation* numericType
	|	annotation* BOOLEAN
	;

numericType
	:	integralType
	|	floatingPointType
	;

integralType
	:	BYTE
	|	SHORT
	|	INT
	|	LONG
	|	CHAR
	;

floatingPointType
	:	FLOAT
	|	DOUBLE
	;

referenceType
	:	classOrInterfaceType
	|	typeVariable
	|	arrayType
	;

classOrInterfaceType
	:	(	classType_lfno_classOrInterfaceType
		|	interfaceType_lfno_classOrInterfaceType
		)
		(	classType_lf_classOrInterfaceType
		|	interfaceType_lf_classOrInterfaceType
		)*
	;

classType
	:	annotation* Identifier typeArguments?
	|	classOrInterfaceType '.' annotation* Identifier typeArguments?
	;

classType_lf_classOrInterfaceType
	:	'.' annotation* Identifier typeArguments?
	;

classType_lfno_classOrInterfaceType
	:	annotation* Identifier typeArguments?
	;

interfaceType
	:	classType
	;

interfaceType_lf_classOrInterfaceType
	:	classType_lf_classOrInterfaceType
	;

interfaceType_lfno_classOrInterfaceType
	:	classType_lfno_classOrInterfaceType
	;

typeVariable
	:	annotation* Identifier
	;

arrayType
	:	primitiveType dims
	|	classOrInterfaceType dims
	|	typeVariable dims
	;

dims
	:	dim (dim)*
	;

dim
   : annotation* '[' ']'
   ;

unannDim
   : '[' ']'
   ;

typeParameter
	:	annotation* Identifier typeBound?
	;

typeBound
	:	EXTENDS typeVariable
	|	EXTENDS classOrInterfaceType additionalBound*
	;

additionalBound
	:	'&' interfaceType
	;

typeArguments
	:	'<' typeArgumentList '>'
	;

typeArgumentList
	:	typeArgument (',' typeArgument)*
	;

typeArgument
	:	referenceType
	|	wildcard
	;

wildcard
	:	annotation* '?' wildcardBounds?
	;

wildcardBounds
	:	EXTENDS referenceType
	|	SUPER referenceType
	;

/*
 * Productions from §6 (Names)
 */

packageName
	:	Identifier
	|	packageName '.' Identifier
	;

typeName
	:	Identifier
	|	packageOrTypeName '.' Identifier
	;

packageOrTypeName
	:	Identifier
	|	packageOrTypeName '.' Identifier
	;

expressionName
	:	Identifier
	|	ambiguousName '.' Identifier
	;

methodName
	:	Identifier
	;

ambiguousName
	:	Identifier
	|	ambiguousName '.' Identifier
	;

/*
 * Productions from §7 (Packages)
 */

compilationUnit
	:	packageDeclaration? importDeclaration* typeDeclaration EOF
	;

packageDeclaration
	:	packageModifier* PACKAGE packageName ';'
	;

packageModifier
	:	annotation
	;

importDeclaration
	:	singleTypeImportDeclaration
	|	typeImportOnDemandDeclaration
	;

singleTypeImportDeclaration
	:	IMPORT typeName ';'
	;

typeImportOnDemandDeclaration
	:	IMPORT packageOrTypeName '.' '*' ';'
	;

typeDeclaration
	:	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

/*
 * Productions from §8 (Classes)
 */

classDeclaration
	:	annotation* classModifier* classAbstractOrFinal? IMPLEMENTATION Identifier typeParameters? superclass? superinterfaces? classBody
	;

classModifier
	:	PUBLIC
	|	PROTECTED
	|	PRIVATE
	|	STATIC
	|	STRICTFP
	;

classAbstractOrFinal
   : ABSTRACT
   | FINAL
   ;

typeParameters
	:	'<' typeParameterList '>'
	;

typeParameterList
	:	typeParameter (',' typeParameter)*
	;

superclass
	:	EXTENDS classType
	;

superinterfaces
	:	OF interfaceTypeList
	;

interfaceTypeList
	:	interfaceType (',' interfaceType)*
	;

classBody
	:	'{' classBodyDeclaration* '}'
	;

classBodyDeclaration
	:	classMemberDeclaration
	|	instanceInitializer
	|	staticInitializer
	|	constructorDeclaration
	;

classMemberDeclaration
	:	fieldDeclaration
	|	methodDeclaration
	|	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

fieldDeclaration
	:	annotation* fieldModifier* unannType variableDeclarator ';'
	;

fieldModifier
	:	PROTECTED
	|   PUBLIC
	|	PRIVATE
	|	STATIC
	|	MUTABLE
	|   FINAL
	|	TRANSIENT
	|	VOLATILE
	;

variableDeclaratorList
	:	variableDeclarator (',' variableDeclarator)*
	;

variableDeclarator
	:	variableDeclaratorId ('=' variableInitializer)?
	;

variableDeclaratorId
	:	Identifier
	;

variableInitializer
	:	expression
	|	arrayInitializer
	;

unannType
	:	unannPrimitiveType
	|	unannReferenceType
	;

unannPrimitiveType
	:	numericType
	|	BOOLEAN
	;

unannReferenceType
	:	unannClassOrInterfaceType
	|	unannTypeVariable
	|	unannArrayType
	;

unannClassOrInterfaceType
	:	(	unannClassType_lfno_unannClassOrInterfaceType
		|	unannInterfaceType_lfno_unannClassOrInterfaceType
		)
		(	unannClassType_lf_unannClassOrInterfaceType
		|	unannInterfaceType_lf_unannClassOrInterfaceType
		)*
	;

unannClassType
	:	Identifier typeArguments?
	|	unannClassOrInterfaceType '.' annotation* Identifier typeArguments?
	;

unannClassType_lf_unannClassOrInterfaceType
	:	'.' annotation* Identifier typeArguments?
	;

unannClassType_lfno_unannClassOrInterfaceType
	:	Identifier typeArguments?
	;

unannInterfaceType
	:	unannClassType
	;

unannInterfaceType_lf_unannClassOrInterfaceType
	:	unannClassType_lf_unannClassOrInterfaceType
	;

unannInterfaceType_lfno_unannClassOrInterfaceType
	:	unannClassType_lfno_unannClassOrInterfaceType
	;

unannTypeVariable
	:	Identifier
	;

unannArrayType
	:	unannPrimitiveType dims
	|	unannClassOrInterfaceType dims
	|	unannTypeVariable dims
	;

methodDeclaration
	:	annotation* methodModifier* methodHeader methodBody
	;

methodModifier
	:	PUBLIC
	|	PROTECTED
	|	PRIVATE
	|	ABSTRACT
	|	STATIC
	|	FINAL
	|	SYNCHRONIZED
	|	NATIVE
	|	STRICTFP
	;

methodHeader
	:	result methodDeclarator throws_?
	|	typeParameters annotation* result methodDeclarator throws_?
	;

result
	:	unannType
	|	VOID
	;

methodDeclarator
	:	Identifier '(' formalParameterList? ')'
	;

formalParameterList
	:	formalParameters ',' lastFormalParameter
	|	lastFormalParameter
	;

formalParameters
	:	formalParameter (',' formalParameter)*
	;

formalParameter
	:	formalParameterModifier* unannType variableDeclaratorId
	;

variableModifier
	:	annotation
	|	FINAL
	;

formalParameterModifier
	:	annotation
	|	MUTABLE
	|   FINAL
	;

lastFormalParameter
	:	formalParameterModifier* unannType annotation* '...' variableDeclaratorId
	|	formalParameter
	;

throws_
	:	THROWS exceptionTypeList
	;

exceptionTypeList
	:	exceptionType (',' exceptionType)*
	;

exceptionType
	:	classType
	|	typeVariable
	;

methodBody
	:	block
	|	';'
	;

instanceInitializer
	:	block
	;

staticInitializer
	:	STATIC block
	;

constructorDeclaration
	:	annotation* constructorModifier? constructorDeclarator throws_? constructorBody
	;

constructorModifier
    :	PUBLIC
	|	PROTECTED
	|	PRIVATE
	;

constructorDeclarator
	:	typeParameters? simpleTypeName '(' formalParameterList? ')'
	;

simpleTypeName
	:	Identifier
	;

constructorBody
	:	'{' explicitConstructorInvocation? blockStatements? '}'
	;

explicitConstructorInvocation
	:	typeArguments? THIS '(' argumentList? ')' ';'
	|	typeArguments? SUPER '(' argumentList? ')' ';'
	|	expressionName '.' typeArguments? SUPER '(' argumentList? ')' ';'
	|	primary '.' typeArguments? SUPER '(' argumentList? ')' ';'
	;

/*
 * Productions from §9 (Interfaces)
 */

interfaceDeclaration
	:	normalInterfaceDeclaration
	|	annotationTypeDeclaration
	;

normalInterfaceDeclaration
	:	annotation* interfaceModifier* INTERFACE Identifier typeParameters? extendsInterfaces? interfaceBody
	;

interfaceModifier
	:	PUBLIC
	|	PROTECTED
	|	PRIVATE
	|	ABSTRACT
	|	STATIC
	|	STRICTFP
	;

extendsInterfaces
	:	EXTENDS interfaceTypeList
	;

interfaceBody
	:	'{' interfaceMemberDeclaration* '}'
	;

interfaceMemberDeclaration
	:	constantDeclaration
	|	interfaceMethodDeclaration
	|	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

constantDeclaration
	:	annotation* constantModifier* unannType variableDeclarator ';'
	;

constantModifier
	:	PUBLIC
	|	STATIC
	;

interfaceMethodDeclaration
	:	annotation* interfaceMethodModifier* methodHeader methodBody
	;

interfaceMethodModifier
	:	PUBLIC
	|	ABSTRACT
	|	DEFAULT
	|	STATIC
	|	STRICTFP
	;

annotationTypeDeclaration
	:	annotation* interfaceModifier* '@' INTERFACE Identifier annotationTypeBody
	;

annotationTypeBody
	:	'{' annotationTypeMemberDeclaration* '}'
	;

annotationTypeMemberDeclaration
	:	annotationTypeElementDeclaration
	|	classDeclaration
	|	interfaceDeclaration
	|	';'
	;

annotationTypeElementDeclaration
	:	annotation* annotationTypeElementModifier* unannType Identifier '(' ')' defaultValue? ';'
	;

annotationTypeElementModifier
	:	PUBLIC
	|	ABSTRACT
	;

defaultValue
	:	DEFAULT elementValue
	;

annotation
	:	normalAnnotation
	|	markerAnnotation
	|	singleElementAnnotation
	;

normalAnnotation
	:	'@' typeName '(' elementValuePairList? ')'
	;

elementValuePairList
	:	elementValuePair (',' elementValuePair)*
	;

elementValuePair
	:	Identifier '=' elementValue
	;

elementValue
	:	conditionalExpression
	|	elementValueArrayInitializer
	|	annotation
	;

elementValueArrayInitializer
	:	'{' elementValueList? ','? '}'
	;

elementValueList
	:	elementValue (',' elementValue)*
	;

markerAnnotation
	:	'@' typeName
	;

singleElementAnnotation
	:	'@' typeName '(' elementValue ')'
	;

/*
 * Productions from §10 (Arrays)
 */

arrayInitializer
	:	'{' variableInitializerList? ','? '}'
	;

variableInitializerList
	:	variableInitializer (',' variableInitializer)*
	;

/*
 * Productions from §14 (Blocks and Statements)
 */

block
	:	'{' blockStatements? '}'
	;

blockStatements
	:	blockStatement+
	;

blockStatement
	:	localVariableDeclarationStatement
	|	classDeclaration
	|	statement
	;

localVariableDeclarationStatement
	:	localVariableDeclaration ';'
	;

localVariableDeclaration
	:	variableModifier* unannType variableDeclarator
	;

forInitlocalVariableDeclaration
	:	variableModifier* unannType variableDeclaratorList
	;

statement
	:	statementWithoutTrailingSubstatement
	|	labeledStatement
	|	ifThenStatement
	|	ifThenElseStatement
	|	whileStatement
	|	forStatement
	;

statementNoShortIf
	:	statementWithoutTrailingSubstatement
	|	labeledStatementNoShortIf
	|	ifThenElseStatementNoShortIf
	|	whileStatementNoShortIf
	|	forStatementNoShortIf
	;

statementWithoutTrailingSubstatement
	:	block
	|	emptyStatement
	|	expressionStatement
	|	assertStatement
	|	switchStatement
	|	doStatement
	|	breakStatement
	|	continueStatement
	|	returnStatement
	|	synchronizedStatement
	|	throwStatement
	|	tryStatement
	;

emptyStatement
	:	';'
	;

labeledStatement
	:	Identifier ':' statement
	;

labeledStatementNoShortIf
	:	Identifier ':' statementNoShortIf
	;

expressionStatement
	:	statementExpression ';'
	;

statementExpression
	:	assignment
	|	preIncrementExpression
	|	preDecrementExpression
	|	postIncrementExpression
	|	postDecrementExpression
	|	methodInvocation
	|	classInstanceCreationExpression
	;

ifThenStatement
	:	IF '(' expression ')' statement
	;

ifThenElseStatement
	:	IF '(' expression ')' statementNoShortIf 'else' statement
	;

ifThenElseStatementNoShortIf
	:	IF '(' expression ')' statementNoShortIf 'else' statementNoShortIf
	;

assertStatement
	:	ASSERT expression ';'
	|	ASSERT expression ':' expression ';'
	;

switchStatement
	:	SWITCH '(' expression ')' switchBlock
	;

switchBlock
	:	'{' switchBlockStatementGroup* switchLabel* '}'
	;

switchBlockStatementGroup
	:	switchLabels blockStatements
	;

switchLabels
	:	switchLabel switchLabel*
	;

switchLabel
	:	CASE constantExpression ':'
	|	DEFAULT ':'
	;

whileStatement
	:	WHILE '(' expression ')' statement
	;

whileStatementNoShortIf
	:	WHILE '(' expression ')' statementNoShortIf
	;

doStatement
	:	DO statement WHILE '(' expression ')' ';'
	;

forStatement
	:	basicForStatement
	|	enhancedForStatement
	;

forStatementNoShortIf
	:	basicForStatementNoShortIf
	|	enhancedForStatementNoShortIf
	;

basicForStatement
	:	FOR '(' forInit? ';' expression? ';' forUpdate? ')' statement
	;

basicForStatementNoShortIf
	:	FOR '(' forInit? ';' expression? ';' forUpdate? ')' statementNoShortIf
	;

forInit
	:	statementExpressionList
	|	forInitlocalVariableDeclaration
	;

forUpdate
	:	statementExpressionList
	;

statementExpressionList
	:	statementExpression (',' statementExpression)*
	;

enhancedForStatement
	:	FOR '(' variableModifier* unannType variableDeclaratorId ':' expression ')' statement
	;

enhancedForStatementNoShortIf
	:	FOR '(' variableModifier* unannType variableDeclaratorId ':' expression ')' statementNoShortIf
	;

breakStatement
	:	BREAK Identifier? ';'
	;

continueStatement
	:	CONTINUE Identifier? ';'
	;

returnStatement
	:	RETURN expression? ';'
	;

throwStatement
	:	THROW expression ';'
	;

synchronizedStatement
	:	SYNCHRONIZED '(' expression ')' block
	;

tryStatement
	:	TRY block catches
	|	TRY block catches? finally_
	|	tryWithResourcesStatement
	;

catches
	:	catchClause catchClause*
	;

catchClause
	:	CATCH '(' catchFormalParameter ')' block
	;

catchFormalParameter
	:	variableModifier* catchType variableDeclaratorId
	;

catchType
	:	unannClassType ('|' unannClassType)*
	;

finally_
	:	FINALLY block
	;

tryWithResourcesStatement
	:	TRY resourceSpecification block catches? finally_?
	;

resourceSpecification
	:	'(' resourceList ';'? ')'
	;

resourceList
	:	resource (';' resource)*
	;

resource
	:	variableModifier* unannType variableDeclaratorId '=' expression
	;

/*
 * Productions from §15 (Expressions)
 */

primary
	:	(	primaryNoNewArray_lfno_primary
		|	arrayCreationExpression
		)
		(	primaryNoNewArray_lf_primary
		)*
	;

primaryNoNewArray
	:	literal
	|	typeName (unannDim)* '.' IMPLEMENTATION
	|	VOID '.' IMPLEMENTATION
	|	THIS
	|	typeName '.' THIS
	|	'(' expression ')'
	|	classInstanceCreationExpression
	|	fieldAccess
	|	arrayAccess
	|	methodInvocation
	|	methodReference
	;

primaryNoNewArray_lf_arrayAccess
	:
	;

primaryNoNewArray_lfno_arrayAccess
	:	literal
	|	typeName (unannDim)* '.' IMPLEMENTATION
	|	VOID '.' IMPLEMENTATION
	|	THIS
	|	typeName '.' THIS
	|	'(' expression ')'
	|	classInstanceCreationExpression
	|	fieldAccess
	|	methodInvocation
	|	methodReference
	;

primaryNoNewArray_lf_primary
	:	classInstanceCreationExpression_lf_primary
	|	fieldAccess_lf_primary
	|	arrayAccess_lf_primary
	|	methodInvocation_lf_primary
	|	methodReference_lf_primary
	;

primaryNoNewArray_lf_primary_lf_arrayAccess_lf_primary
	:
	;

primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary
	:	classInstanceCreationExpression_lf_primary
	|	fieldAccess_lf_primary
	|	methodInvocation_lf_primary
	|	methodReference_lf_primary
	;

primaryNoNewArray_lfno_primary
	:	literal
	|	typeName (unannDim)* '.' IMPLEMENTATION
	|	unannPrimitiveType (unannDim)* '.' IMPLEMENTATION
	|	VOID '.' IMPLEMENTATION
	|	THIS
	|	typeName '.' THIS
	|	'(' expression ')'
	|	classInstanceCreationExpression_lfno_primary
	|	fieldAccess_lfno_primary
	|	arrayAccess_lfno_primary
	|	methodInvocation_lfno_primary
	|	methodReference_lfno_primary
	;

primaryNoNewArray_lfno_primary_lf_arrayAccess_lfno_primary
	:
	;

primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary
	:	literal
	|	typeName (unannDim)* '.' IMPLEMENTATION
	|	unannPrimitiveType (unannDim)* '.' IMPLEMENTATION
	|	VOID '.' IMPLEMENTATION
	|	THIS
	|	typeName '.' THIS
	|	'(' expression ')'
	|	classInstanceCreationExpression_lfno_primary
	|	fieldAccess_lfno_primary
	|	methodInvocation_lfno_primary
	|	methodReference_lfno_primary
	;

classInstanceCreationExpression
	:	NEW typeArguments? constructorIdentifier ('.' constructorIdentifier)* typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
	|	expressionName '.' NEW typeArguments? constructorIdentifier typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
	|	primary '.' NEW typeArguments? constructorIdentifier typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
	;

classInstanceCreationExpression_lf_primary
	:	'.' NEW typeArguments? constructorIdentifier typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
	;

classInstanceCreationExpression_lfno_primary
	:	NEW typeArguments? constructorIdentifier ('.' constructorIdentifier)* typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
	|	expressionName '.' NEW typeArguments? constructorIdentifier typeArgumentsOrDiamond? '(' argumentList? ')' classBody?
	;

constructorIdentifier
   : annotation* Identifier
   ;

typeArgumentsOrDiamond
	:	typeArguments
	|	'<' '>'
	;

fieldAccess
	:	primary '.' Identifier
	|	SUPER '.' Identifier
	|	typeName '.' SUPER '.' Identifier
	;

fieldAccess_lf_primary
	:	'.' Identifier
	;

fieldAccess_lfno_primary
	:	SUPER '.' Identifier
	|	typeName '.' SUPER '.' Identifier
	;

arrayAccess
	:	(	expressionName '[' expression ']'
		|	primaryNoNewArray_lfno_arrayAccess '[' expression ']'
		)
		(	primaryNoNewArray_lf_arrayAccess '[' expression ']'
		)*
	;

arrayAccess_lf_primary
	:	(	primaryNoNewArray_lf_primary_lfno_arrayAccess_lf_primary '[' expression ']'
		)
		(	primaryNoNewArray_lf_primary_lf_arrayAccess_lf_primary '[' expression ']'
		)*
	;

arrayAccess_lfno_primary
	:	(	expressionName '[' expression ']'
		|	primaryNoNewArray_lfno_primary_lfno_arrayAccess_lfno_primary '[' expression ']'
		)
		(	primaryNoNewArray_lfno_primary_lf_arrayAccess_lfno_primary '[' expression ']'
		)*
	;

methodInvocation
	:	methodName '(' argumentList? ')'
	|	typeName '.' typeArguments? Identifier '(' argumentList? ')'
	|	expressionName '.' typeArguments? Identifier '(' argumentList? ')'
	|	primary '.' typeArguments? Identifier '(' argumentList? ')'
	|	SUPER '.' typeArguments? Identifier '(' argumentList? ')'
	|	typeName '.' SUPER '.' typeArguments? Identifier '(' argumentList? ')'
	;

methodInvocation_lf_primary
	:	'.' typeArguments? Identifier '(' argumentList? ')'
	;

methodInvocation_lfno_primary
	:	methodName '(' argumentList? ')'
	|	typeName '.' typeArguments? Identifier '(' argumentList? ')'
	|	expressionName '.' typeArguments? Identifier '(' argumentList? ')'
	|	SUPER '.' typeArguments? Identifier '(' argumentList? ')'
	|	typeName '.' SUPER '.' typeArguments? Identifier '(' argumentList? ')'
	;

argumentList
	:	expression (',' expression)*
	;

methodReference
	:	expressionName '::' typeArguments? Identifier
	|	referenceType '::' typeArguments? Identifier
	|	primary '::' typeArguments? Identifier
	|	SUPER '::' typeArguments? Identifier
	|	typeName '.' SUPER '::' typeArguments? Identifier
	|	classType '::' typeArguments? NEW
	|	arrayType '::' NEW
	;

methodReference_lf_primary
	:	'::' typeArguments? Identifier
	;

methodReference_lfno_primary
	:	expressionName '::' typeArguments? Identifier
	|	referenceType '::' typeArguments? Identifier
	|	SUPER '::' typeArguments? Identifier
	|	typeName '.' SUPER '::' typeArguments? Identifier
	|	classType '::' typeArguments? NEW
	|	arrayType '::' NEW
	;

arrayCreationExpression
	:	NEW primitiveType dimExprs dims?
	|	NEW classOrInterfaceType dimExprs dims?
	|	NEW primitiveType dims arrayInitializer
	|	NEW classOrInterfaceType dims arrayInitializer
	;

dimExprs
	:	dimExpr dimExpr*
	;

dimExpr
	:	annotation* '[' expression ']'
	;

constantExpression
	:	expression
	;

expression
	:	lambdaExpression
	|	assignmentExpression
	;

lambdaExpression
	:	lambdaParameters '->' lambdaBody
	;

lambdaParameters
	:	Identifier
	|	'(' formalParameterList? ')'
	|	'(' inferredFormalParameterList ')'
	;

inferredFormalParameterList
	:	Identifier (',' Identifier)*
	;

lambdaBody
	:	expression
	|	block
	;

assignmentExpression
	:	conditionalExpression
	|	assignment
	;

assignment
	:	leftHandSide assignmentOperator expression
	;

leftHandSide
	:	expressionName
	|	fieldAccess
	|	arrayAccess
	;

assignmentOperator
	:	'='
	|	'*='
	|	'/='
	|	'%='
	|	'+='
	|	'-='
	|	'<<='
	|	'>>='
	|	'>>>='
	|	'&='
	|	'^='
	|	'|='
	;

conditionalExpression
	:	conditionalOrExpression
	|	conditionalOrExpression '?' expression ':' conditionalExpression
	;

conditionalOrExpression
	:	conditionalAndExpression
	|	conditionalOrExpression '||' conditionalAndExpression
	;

conditionalAndExpression
	:	inclusiveOrExpression
	|	conditionalAndExpression '&&' inclusiveOrExpression
	;

inclusiveOrExpression
	:	exclusiveOrExpression
	|	inclusiveOrExpression '|' exclusiveOrExpression
	;

exclusiveOrExpression
	:	andExpression
	|	exclusiveOrExpression '^' andExpression
	;

andExpression
	:	equalityExpression
	|	andExpression '&' equalityExpression
	;

equalityExpression
	:	relationalExpression
	|	equalityExpression '==' relationalExpression
	|	equalityExpression '!=' relationalExpression
	;

relationalExpression
	:	shiftExpression
	|	relationalExpression '<' shiftExpression
	|	relationalExpression '>' shiftExpression
	|	relationalExpression '<=' shiftExpression
	|	relationalExpression '>=' shiftExpression
	|	relationalExpression INSTANCEOF referenceType
	;

shiftExpression
	:	additiveExpression
	|	shiftExpression '<' '<' additiveExpression
	|	shiftExpression '>' '>' additiveExpression
	|	shiftExpression '>' '>' '>' additiveExpression
	;

additiveExpression
	:	multiplicativeExpression
	|	additiveExpression '+' multiplicativeExpression
	|	additiveExpression '-' multiplicativeExpression
	;

multiplicativeExpression
	:	unaryExpression
	|	multiplicativeExpression '*' unaryExpression
	|	multiplicativeExpression '/' unaryExpression
	|	multiplicativeExpression '%' unaryExpression
	;

unaryExpression
	:	preIncrementExpression
	|	preDecrementExpression
	|	'+' unaryExpression
	|	'-' unaryExpression
	|	unaryExpressionNotPlusMinus
	;

preIncrementExpression
	:	'++' unaryExpression
	;

preDecrementExpression
	:	'--' unaryExpression
	;

unaryExpressionNotPlusMinus
	:	postfixExpression
	|	'~' unaryExpression
	|	'!' unaryExpression
	|	castExpression
	;

postfixExpression
	:	(	primary
		|	expressionName
		)
		(	postIncrementExpression_lf_postfixExpression
		|	postDecrementExpression_lf_postfixExpression
		)*
	;

postIncrementExpression
	:	postfixExpression '++'
	;

postIncrementExpression_lf_postfixExpression
	:	'++'
	;

postDecrementExpression
	:	postfixExpression '--'
	;

postDecrementExpression_lf_postfixExpression
	:	'--'
	;

castExpression
	:	'(' primitiveType ')' unaryExpression
	|	'(' referenceType additionalBound* ')' unaryExpressionNotPlusMinus
	|	'(' referenceType additionalBound* ')' lambdaExpression
	;
