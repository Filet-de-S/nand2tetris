package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
	"unicode"
)

func main() {
//testComment()
//	testTokens()
//	return

	flsName := getFiles(os.Args[1])

	for _, fName := range flsName.files {
		fName = flsName.dir + fName
		rFile, err := os.Open(fName)
		if err != nil {
			panic("can't open file to read: " + err.Error())
		}
		defer rFile.Close()

		fName = strings.TrimSuffix(fName, ".jack")

		tokenFile, err := os.Create(fName + "T.xml") //todo
		if err != nil {

			panic("can't create file to write: " + err.Error())
		}
		defer tokenFile.Close()

		astFile, err := os.Create(fName + ".xml") //todo
		if err != nil {

			panic("can't create file to write: " + err.Error())
		}
		defer astFile.Close()

		err = compile(rFile, tokenFile, astFile)
		if err != nil {
			fmt.Println("Compile error:", err)
			os.Exit(1)
		}
	}
}

func compile(rFile io.Reader, tokenFile, astFile *os.File) error {
	tkns := tokenize(rFile)
	cpTkns := *tkns
	//printTkns(tkns, tokenFile)

	class, err := compileClass(&cpTkns)
	if err != nil {
		return err
	}
	printClass(class, astFile)

	return nil
}

func printClass(class *Class, astFile *os.File) {
	_, err := astFile.WriteString(
		"<class>\n" +
		"<keyword> class </keyword>\n" +
		"<identifier> "+class.Identifier+" </identifier>\n" +
		"<symbol> { </symbol>\n")
	if err != nil {
		panic("cant write to file: "+err.Error())
	}

	if class.ClassVarDecs != nil {
		printClassVarDecs(class.ClassVarDecs, astFile)
	}
	if class.FuncDec != nil {
		printClassFuncs(class.FuncDec, astFile)
	}

	_, err = astFile.WriteString(
		"<symbol> } </symbol>\n" +
		"</class>\n")
	if err != nil {
		panic("cant write to file: "+err.Error())
	}
}

func printClassFuncs(funcs []FuncDec, file *os.File) {
	//FuncDec{
	//	Typ:        "",
	//	ReturnType: VarType{},
	//	Identifier: "",
	//	Params:     nil,
	//	Body:       RoutineBody{},
	//}

	for i := range funcs {
		f := funcs[i]

		_, err := file.WriteString(
			"<subroutineDec>\n" +
				"<keyword> " + f.Typ + " </keyword>\n" +
				printVarType(f.ReturnType) +
				"<identifier> " + f.Identifier + " </identifier>\n" +
				"<symbol> ( </symbol>\n" +
				"<parameterList>\n" +
				printParamList(f.Params) +
				"</parameterList>\n" +
				"<symbol> ) </symbol>\n" +
				"<subroutineBody>\n" +
				"<symbol> { </symbol>\n" +
				printFuncStmts(f.Body) +
				"<symbol> } </symbol>\n" +
				"</subroutineBody>\n" +
				"</subroutineDec>\n")

		if err != nil {
			panic("cant write to file: " + err.Error())
		}
	}
}

func printFuncStmts(body RoutineBody) (s string) {
	s = printVarDecs(body.VarDecs)
 	s += "<statements>\n" +
		printStatements(body.Statements) +
		"</statements>\n"
 	return s
}

func printStatements(stmts []Statement) (s string) {
	for i := range stmts {
		stmt := stmts[i]
		switch stmt.(type) {
		case LetSt:
			s += printLet(stmt.(LetSt))
		case IfSt:
			s += printIf(stmt.(IfSt))
		case WhileSt:
			s += printWhile(stmt.(WhileSt))
		case DoSt:
			s += printDo(stmt.(DoSt))
		case ReturnSt:
			s += printRet(stmt.(ReturnSt))
		default:
			panic("undefined statement")
		}
	}
	return s
}

func printRet(st ReturnSt) (s string) {
	s = "<returnStatement>\n" +
		"<keyword> return </keyword>\n"
	if st != (ReturnSt{}) {
		s += printExpr(Expression(st))
	}
	return s + "<symbol> ; </symbol>\n" +
		"</returnStatement>\n"
}

func printDo(st DoSt) (s string) {
	s = "<doStatement>\n" +
		"<keyword> do </keyword>\n" +
		printFuncCall(SubroutineCall(st)) +
		"<symbol> ; </symbol>\n" +
		"</doStatement>\n"
	return s
}

func printFuncCall(sc SubroutineCall) (s string) {
	if sc.ClassOrVarName != "" {
		s += "<identifier> " + sc.ClassOrVarName + " </identifier>\n" +
			"<symbol> . </symbol>\n"
	}

	s += "<identifier> " + sc.Name + " </identifier>\n" +
		"<symbol> ( </symbol>\n" +
		"<expressionList>\n"

	for i := range sc.ExprList {
		exp := sc.ExprList[i]
		if i > 0 {
			s += "<symbol> , </symbol>\n"
		}
		s += printExpr(exp)
	}

	s += "</expressionList>\n" +
		"<symbol> ) </symbol>\n"
	return s
}

func printWhile(st WhileSt) (s string) {
	return "<whileStatement>\n" +
		"<keyword> while </keyword>\n" +
		"<symbol> ( </symbol>\n" +
		printExpr(st.Expr)+
		"<symbol> ) </symbol>\n" +
		"<symbol> { </symbol>\n" +
		"<statements>\n" +
		printStatements(st.Statements)+
		"</statements>\n" +
		"<symbol> } </symbol>\n"+
		"</whileStatement>\n"
}

func printIf(st IfSt) (s string) {
	s = "<ifStatement>\n" +
		"<keyword> if </keyword>\n" +
		"<symbol> ( </symbol>\n" +
		printExpr(st.Expr)+
		"<symbol> ) </symbol>\n" +
		"<symbol> { </symbol>\n" +
		"<statements>\n" +
		printStatements(st.Statements)+
		"</statements>\n" +
		"<symbol> } </symbol>\n"

	if st.ElseSts != nil {
		s += "<keyword> else </keyword>\n" +
			"<symbol> { </symbol>\n" +
			"<statements>\n" +
			printStatements(st.ElseSts)+
			"</statements>\n" +
			"<symbol> } </symbol>\n"
	}

	return s + "</ifStatement>\n"
}

func printLet(st LetSt) (s string) {
	s = "<letStatement>\n" +
		"<keyword> let </keyword>\n" +
		"<identifier> "+st.VarName.Name+" </identifier>\n"

	if st.VarName.Expr != (Expression{}) {
		s += "<symbol> [ </symbol>\n" +
			printExpr(st.VarName.Expr) +
			"<symbol> ] </symbol>\n"
	}

	s += "<symbol> = </symbol>\n" +
		printExpr(st.Expr)+
		"<symbol> ; </symbol>\n" +
		"</letStatement>\n"

	return s
}

func printExpr(expr Expression) (s string) {
	s = "<expression>\n" +
		printTerm(expr.Term)

	if expr.OP != "" {
		s += "<symbol> "+expr.OP+" </symbol>\n" +
			printTerm(*expr.OPTerm)
	}

	return s + "</expression>\n"
}

func printTerm(term Term) (s string) {
	s = "<term>\n"

	switch term.Typ {
	case integerConstant, stringConstant, keyword:
		s += "<"+term.Typ+"> "+term.Value.(string)+" </"+term.Typ+">\n"
	case identifier:
		switch term.Value.(type) {
		case SubroutineCall:
			s += printFuncCall(term.Value.(SubroutineCall))
		case Var:
			s += printVar(term.Value.(Var))
		}
	case expression:
		s += "<symbol> ( </symbol>\n" +
			printExpr(term.Value.(Expression)) +
			"<symbol> ) </symbol>\n"
	case unary:
		uo := term.Value.(UnaryOpTerm)
		s += "<symbol> "+uo.UnaryOp+" </symbol>\n" +
			printTerm(uo.Term)
	default:
		panic("undefined term")
	}

	return s + "</term>\n"
}

func printVar(v Var) (s string) {
	s = "<identifier> "+v.Name+" </identifier>\n"

	if v.Expr != (Expression{}) {
		s += "<symbol> [ </symbol>\n" +
			printExpr(v.Expr) +
			"<symbol> ] </symbol>\n"
	}

	return s
}

func printVarDecs(decs []VarDec) (s string ) {
	for i := range decs {
		v := decs[i]
		s += "<varDec>\n" +
			"<keyword> var </keyword>\n" +
			printVarType(v.Typ) +
			printIdentifiers(v.Identifier)+
			"<symbol> ; </symbol>\n" +
			"</varDec>\n"
	}

	return s
}

func printParamList(params []Param) (s string) {
	for i := range params {
		p := params[i]

		if i > 0 {
			s += "<symbol> , </symbol>\n"
		}
		s += printVarType(p.Typ)
		s += "<identifier> "+p.Name+" </identifier>\n"
	}

	return s
}

func printClassVarDecs(vars []VarDec, file *os.File) {
	for i := range vars {
		v := vars[i]

		_, err := file.WriteString(
			"<classVarDec>\n" +
				"<keyword> "+v.Scope+" </keyword>\n" +
				printVarType(v.Typ)+
				printIdentifiers(v.Identifier)+
				"<symbol> ; </symbol>\n" +
				"</classVarDec>\n")
		if err != nil {
			panic("cant write to file: "+err.Error())
		}
	}
}

func printVarType(typ VarType) string {
	if typ.Custom {
		return "<identifier> "+typ.Name+" </identifier>\n"
	} else {
		return "<keyword> "+typ.Name+" </keyword>\n"
	}
}

func printIdentifiers(ids []string) (s string) {
	for i := range ids {
		if i > 0 {
			s += "<symbol> , </symbol>\n"
		}
		s += "<identifier> "+ids[i]+" </identifier>\n"
	}

	return s
}

type AST struct {
	Classes []Class
}

type Class struct {
	Identifier   string
	ClassVarDecs []VarDec
	FuncDec      []FuncDec
}

type VarDec struct {
	Scope string
	Typ VarType
	Identifier []string
}

type VarType struct {
	Name string
	Custom bool
}

// compile class
func compileClass(tkns *Tokens) (cls *Class, err error) {
	var classClosed bool
	cls = &Class{}

	i := 0
	for tkns.Next() {
		tkn := tkns.Get()
		i++

		switch i {
		case 1:
			if tkn.typ != keyword && tkn.value != class {
				return nil, fmt.Errorf("file should begin with 'class' token keyword")
			}
		case 2:
			if tkn.typ != identifier {
				return nil, fmt.Errorf("after token 'class' should be class identifier, have %s", tkn.typ)
			}
			cls.Identifier = tkn.value
		case 3:
			if tkn.typ != symbol && tkn.value != "{" {
				return nil, fmt.Errorf("after token 'identifier' should be class opener '{', have %s", tkn.value)
			}
		case 4:
			// zero or one classVarDec
			// zero or one subroutineDec
			// ClosingSymbol
			classClosed, err = varOrsubroutineOrClassClosingCase(tkns, tkn, cls)
			if err != nil {
				return nil, err
			}
		case 5:
			// zero or one subroutineDec
			// ClosingSymbol
			classClosed, err = subroutineOrClassClosingCase(tkns, tkn, cls, classClosed)
			if err != nil {
				return nil, err
			}
		case 6:
			if !classClosed && tkn.typ == symbol && tkn.value == "}" {
				classClosed = true
				continue
			}
			fallthrough
		default:
			return nil, fmt.Errorf("unexpected Class closed: %t, have %s", classClosed, tkn.value)
		}
	}

	return cls, nil
}

func subroutineOrClassClosingCase(tkns *Tokens, tkn Tkn, cls *Class, classClosed bool,
	) (clsClosed bool, err error) {
	switch tkn.typ {
	case keyword:
		if inSlice(tkn.value, funcDecl) {
			return false, compileFuncDec(tkns, tkn, cls)
		}
		return false, fmt.Errorf("unexpected value '%s', want subroutineDec* | '}'. Class closed: %t", tkn.value, classClosed)
	case symbol:
		if tkn.value == "}" && !classClosed {
			return true, nil
		}
		fallthrough
	default:
		return false, fmt.Errorf("unexpected value '%s', want subroutineDec* | '}'. Class closed: %t", tkn.value, classClosed)
	}
}

func varOrsubroutineOrClassClosingCase(tkns *Tokens, tkn Tkn, cls *Class,
	) (classClosed bool, err error) {
	switch tkn.typ {
	case keyword:
		switch {
		case inSlice(tkn.value, []string{static, field}):
			err = compileClassVarDecs(tkns, tkn, cls)
		case inSlice(tkn.value, funcDecl):
			err = compileFuncDec(tkns,  tkn, cls)
		default:
			return false, fmt.Errorf("unexpected value '%s', want classVarDec* | subroutineDec* | '}'", tkn.value)
		}
	case symbol:
		if tkn.value == "}" {
			return true, nil
		}
		fallthrough
	default:
		return false, fmt.Errorf("unexpected value '%s', want classVarDec* | subroutineDec* | '}'", tkn.value)
	}
	return false, err
}

// compile ClassVars
func compileClassVarDecs(tkns *Tokens, tkn Tkn, cls *Class) (err error) {
newClassVarDec:
	varDec := VarDec{}
	varDec.Scope = tkn.value
	varDec.Typ, err = getType(tkns, varKW)
	if err != nil {
		return err
	}

	varDec.Identifier, err = getIdentifiers(tkns, ";")
	if err != nil {
		return err
	}

	cls.ClassVarDecs = append(cls.ClassVarDecs, varDec)

	if tkns.Next() {
		tkn = tkns.Get()
		if tkn.typ == keyword && inSlice(tkn.value, []string{static, field}) {
			goto newClassVarDec
		}
		tkns.PushBack()
	}
	return nil
}

// get identifiers
func getIdentifiers(tkns *Tokens, closingSymbol string) (ids []string, err error) {
	tkn := Tkn{}
	i := 0

	for tkns.Next() {
		tkn = tkns.Get()
		i++

		switch {
		case i%2 != 0:
			if tkn.typ != identifier {
				return nil, fmt.Errorf("unexpected type of var: %s – %s, need var name", tkn.typ, tkn.value)
			}
			ids = append(ids, tkn.value)
		default:
			if tkn.typ != symbol {
				return nil, fmt.Errorf("unexpected type of var: %s – %s, need symbol token", tkn.typ, tkn.value)
			}
			switch tkn.value {
			case ",":
				continue
			case closingSymbol: // ";" ")"
				return ids, nil
			}
			return nil, fmt.Errorf("unexpected type of var: %s – %s, need closing symbol token: '%s'", tkn.typ, tkn.value, closingSymbol)
		}
	}
	return nil, fmt.Errorf("unexpected token: %s", tkn.value)
}

// get type
func getType(tkns *Tokens, typ string) (VarType, error) {
	if !tkns.Next() {
		return VarType{}, fmt.Errorf("unexpected end of class var decl")
	}
	tkn := tkns.Get()

	switch tkn.typ {
	case keyword:
		if (typ == function && !inSlice(tkn.value, []string{intKW, char, boolean, void})) ||
			(typ == varKW && !inSlice(tkn.value, []string{intKW, char, boolean})) {
			return VarType{}, fmt.Errorf("unexpected type of var: %s – %s, need %s type", tkn.typ, tkn.value, typ)
		}
		return VarType{tkn.value, false}, nil
	case identifier:
		return VarType{tkn.value, true}, nil
	default:
		return VarType{}, fmt.Errorf("unexpected type of var: %s – %s, need %s type", tkn.typ, tkn.value, typ)
	}
}

type FuncDec struct {
	Typ        string
	ReturnType VarType
	Identifier string
	Params     []Param
	Body       RoutineBody
}

var funcDecl = []string{constructor, function, method}

// compile func
func compileFuncDec(tkns *Tokens, tkn Tkn, cls *Class) (err error) {
newFunc:
	funcDec := FuncDec{}
	funcDec.Typ = tkn.value
	funcDec.ReturnType, err = getType(tkns, function)
	if err != nil {
		return err
	}

	funcDec.Identifier, err = getFuncIdentifier(tkns)
	if err != nil {
		return err
	}

	need, err := needParse(tkns, []string{keyword, identifier}, symbol, ")")
	if err != nil {
		return err
	}
	if need {
		funcDec.Params, err = getFuncParams(tkns)
		if err != nil {
			return err
		}
	}

	funcDec.Body, err = getFuncBody(tkns)
	if err != nil {
		return err
	}

	cls.FuncDec = append(cls.FuncDec, funcDec)

	if tkns.Next() {
		tkn = tkns.Get()
		if tkn.typ == keyword && inSlice(tkn.value, funcDecl) {
			goto newFunc
		}
		tkns.PushBack()
	}
	return nil
}

type Param struct {
	Name string
	Typ VarType
}

func getFuncParams(tkns *Tokens) (params []Param, err error) {
	tkn := Tkn{}
	p := Param{}
	i := 0

	for tkns.Next() {
		tkn = tkns.Get()
		i++

		switch i%3 {
		case 1:
			tkns.PushBack()
			p.Typ, err = getType(tkns, varKW)
			if err != nil {
				return nil, err
			}
		case 2:
			if tkn.typ != identifier {
				return nil, fmt.Errorf("unexpected type of var: %s – %s, need param name", tkn.typ, tkn.value)
			}
			p.Name = tkn.value
			params = append(params, p)
		default:
			if tkn.typ != symbol {
				return nil, fmt.Errorf("unexpected type of var: %s – %s, need symbol", tkn.typ, tkn.value)
			}
			switch tkn.value {
			case ",":
				continue
			case ")":
				return params, nil
			}
			return nil, fmt.Errorf("unexpected value of var: %s – %s, need ', type VarName' or ')'", tkn.typ, tkn.value)
		}
	}
	return nil, fmt.Errorf("unexpected token: %s", tkn.value)
}

func getFuncIdentifier(tkns *Tokens) (id string, err error) {
	i := 0

	tkn := Tkn{}
	for tkns.Next() {
		tkn = tkns.Get()
		i++

		switch {
		case i%2 != 0:
			if tkn.typ != identifier {
				return "", fmt.Errorf("unexpected type of var: %s – %s, need func identifier", tkn.typ, tkn.value)
			}
			id = tkn.value
		default:
			if tkn.typ == symbol && tkn.value == "(" {
				return id, nil
			}
			return "", fmt.Errorf("unexpected token: %s, need '('", tkn.value)
		}
	}

	return "", fmt.Errorf("unexpected token: %s", tkn.value)
}

func needParse(tkns *Tokens, typs []string, elseTyp, elseVal string,
	) (need bool, err error) {
	unexpectedTkn := &Tkn{
		typ:   "TOKEN",
		value: "NEEDED",
	}

	if tkns.Next() {
		tkn := tkns.Get()

		if inSlice(tkn.typ, typs) {
			tkns.PushBack()
			return true, nil
		} else if tkn.typ == elseTyp && tkn.value == elseVal {
			return false, nil
		}
		unexpectedTkn = &tkn
	}
	return false, fmt.Errorf("unexpected token: %s - %s",
		unexpectedTkn.typ, unexpectedTkn.value)
}

type RoutineBody struct {
	VarDecs []VarDec
	Statements []Statement
}

func getFuncBody(tkns *Tokens) (rb RoutineBody, err error) {
	err = needToken(tkns, symbol, "{", function)
	if err != nil {
		return rb, err
	}

	err = compileVarDecs(tkns, &rb)
	if err != nil {
		return rb, err
	}

	stmts, retStmt, err := compileStatements(tkns)
	if err != nil {
		return rb, err
	} else if !retStmt {
		return rb, fmt.Errorf("expected return statement in func body")
	}

	rb.Statements = stmts

	return rb, needToken(tkns, symbol, "}", function)
}

func compileVarDecs(tkns *Tokens, rb *RoutineBody) (err error) {
	if !tkns.Next() {
		return fmt.Errorf("unexpected end of file, expected vars decl, func body, or '}'")
	}

	tkn := tkns.Get()
	if tkn.typ != keyword || tkn.value != varKW {
		tkns.PushBack()
		return nil
	}

newVarDec:
	varDec := VarDec{}
	varDec.Scope = tkn.value
	varDec.Typ, err = getType(tkns, varKW)
	if err != nil {
		return err
	}

	varDec.Identifier, err = getIdentifiers(tkns, ";")
	if err != nil {
		return err
	}

	rb.VarDecs = append(rb.VarDecs, varDec)

	if tkns.Next() {
		tkn = tkns.Get()
		if tkn.typ == keyword && tkn.value == varKW {
			goto newVarDec
		}
		tkns.PushBack()
	}
	return nil
}

type Statement interface{}

type Term struct {
	Typ string
	Value interface{}
}

type Expression struct {
	Term Term
	OP string
	OPTerm *Term
}

type Var struct {
	Name string
	Expr Expression
}

type LetSt struct {
	VarName Var
	Expr Expression
}

type IfSt struct {
	Expr Expression
	Statements []Statement
	ElseSts []Statement
}

type WhileSt struct {
	Expr       Expression
	Statements []Statement
}

type DoSt SubroutineCall

type ReturnSt Expression

type SubroutineCall struct {
	Name string
	ClassOrVarName string
	ExprList []Expression
}

type UnaryOpTerm struct {
	UnaryOp string
	Term Term
}

var statements = []string{let, ifKW, elseKW, while, do, returnKW}

//statements
func compileStatements(tkns *Tokens) (stmts []Statement, returnStmt bool, err error) {
	if !tkns.Next() {
		return nil, false, fmt.Errorf("unexpected end of file, expected func body, or '}'")
	}
	tkn := tkns.Get()
	if tkn.typ != keyword || !inSlice(tkn.value, statements) {
		tkns.PushBack()
		return nil, false,nil
	}

	var stmt Statement

newStatement:
	switch tkn.value {
	case let:
		stmt, err = compileLet(tkns)
	case ifKW:
		stmt, err = compileIf(tkns)
	case while:
		stmt, err = compileWhile(tkns)
	case do:
		stmt, err = compileDo(tkns)
	case returnKW:
		stmt, err = compileReturn(tkns, returnStmt)
		returnStmt = true
	}
	if err != nil {
		return nil, false, err
	}

	stmts = append(stmts, stmt)

	if tkns.Next() {
		tkn = tkns.Get()
		if tkn.typ == keyword && inSlice(tkn.value, statements) {
			goto newStatement
		}
		tkns.PushBack()
	}

	return stmts, returnStmt, nil
}

//do
func compileDo(tkns *Tokens) (doSt DoSt, err error) {
	sc, err := compileSubroutineCall(tkns)
	if err != nil {
		return doSt, err
	}

	err = needToken(tkns, symbol, ";", do)

	return DoSt(sc), err
}

//while
func compileWhile(tkns *Tokens) (whl WhileSt, err error) {
	err = needToken(tkns, symbol, "(", while)
	if err != nil {
		return whl, err
	}

	whl.Expr, err = compileExprs(tkns)

	err = needToken(tkns, symbol, ")", while)
	if err != nil {
		return whl, err
	}

	err = needToken(tkns, symbol, "{", while)
	if err != nil {
		return whl, err
	}

	whl.Statements, _, err = compileStatements(tkns)
	if err != nil {
		return whl, err
	}

	err = needToken(tkns, symbol, "}", while)
	if err != nil {
		return whl, err
	}

	return whl, nil
}

//if
func compileIf(tkns *Tokens) (ifSt IfSt, err error) {
	err = needToken(tkns, symbol, "(", ifKW)
	if err != nil {
		return ifSt, err
	}

	ifSt.Expr, err = compileExprs(tkns)

	err = needToken(tkns, symbol, ")", ifKW)
	if err != nil {
		return ifSt, err
	}

	err = needToken(tkns, symbol, "{", ifKW)
	if err != nil {
		return ifSt, err
	}

	ifSt.Statements, _, err = compileStatements(tkns)
	if err != nil {
		return ifSt, err
	}

	err = needToken(tkns, symbol, "}", ifKW)
	if err != nil {
		return ifSt, err
	}

	ok, err := checkToken(tkns, keyword, "else", ifKW+elseKW)
	if err != nil {
		return ifSt, err
	}

	if ok {
		tkns.Next()
		err = needToken(tkns, symbol, "{", elseKW)
		if err != nil {
			return ifSt, err
		}

		ifSt.ElseSts, _, err = compileStatements(tkns)
		if err != nil {
			return ifSt, err
		}

		err = needToken(tkns, symbol, "}", elseKW)
		if err != nil {
			return ifSt, err
		}
	}

	return ifSt, nil
}

//return
func compileReturn(tkns *Tokens, returnStmt bool) (ret ReturnSt, err error) {
	if returnStmt {
		return ret, fmt.Errorf("dup of return statement")
	}

	ok, err := checkToken(tkns, symbol, ";", returnKW)
	if err != nil {
		return ret, err
	}
	if ok {
		tkns.Next()
		return ret, nil
	}

	exp, err := compileExprs(tkns)
	if err != nil {
		return ret, err
	}

	return ReturnSt(exp), needToken(tkns, symbol, ";", returnKW)
}

//let
func compileLet(tkns *Tokens) (letSt LetSt, err error) {
	letSt.VarName, err = compileVarName(tkns)
	if err != nil {
		return letSt, err
	}

	err = needToken(tkns, symbol, "=", let)
	if err != nil {
		return letSt, err
	}

	letSt.Expr, err = compileExprs(tkns)
	if err != nil {
		return letSt, err
	}

	err = needToken(tkns, symbol, ";", let)
	if err != nil {
		return letSt, err
	}

	return letSt, nil
}

//varName
func compileVarName(tkns *Tokens) (vn Var, err error) {
	if !tkns.Next() {
		return vn, fmt.Errorf("expected var name, have end of file")
	}
	tkn := tkns.Get()
	if tkn.typ != identifier {
		return vn, fmt.Errorf("expected var name, have %s - %s", tkn.typ, tkn.value)
	}

	vn.Name = tkn.value

	if !tkns.Next() {
		return vn, fmt.Errorf("unexpected end of var statement decl")
	}
	tkn = tkns.Get()

	if tkn.value == "[" {
		vn.Expr, err = compileExprs(tkns)
		if err != nil {
			return vn, err
		}

		if !tkns.Next() {
			return vn, fmt.Errorf("unexpected end of var statement")
		}

		tkn = tkns.Get()
		if tkn.value != "]" {
			return vn, fmt.Errorf("unexpected token in var statement: %s, want symbol ']'",
				tkn.value)
		}
	} else {
		tkns.PushBack()
	}

	return vn, nil
}

//expression
func compileExprs(tkns *Tokens) (expr Expression, err error) {
	expr.Term, err = compileTerm(tkns)

	if !tkns.Next() {
		return expr, fmt.Errorf("unexpected end of file, want expression close")
	}

	tkn := tkns.Get()
	if tkn.typ == symbol && isOP(tkn.value) {
		expr.OP = tkn.value

		oPTerm, err := compileTerm(tkns)
		if err != nil {
			return expr, err
		}
		expr.OPTerm = &oPTerm
	} else {
		tkns.PushBack()
	}

	return expr, nil
}

//term
func compileTerm(tkns *Tokens) (term Term, err error) {
	if !tkns.Next() {
		return term, fmt.Errorf("expected term, have end")
	}

	tkn := tkns.Get()
	term.Typ = tkn.typ
	term.Value = tkn.value

	switch tkn.typ {
	case integerConstant, stringConstant:
	case keyword:
		v := tkn.value
		if v != "true" && v != "false" && v != "null" && v != "this" {
			return term, fmt.Errorf("have keyword, but non-acceptable value: %s", v)
		}
	case identifier: //varName, varName[expr], subroutineCall
		term.Value, err = compileTermIdentifier(tkns)
		if err != nil {
			return term, err
		}
	case symbol: // ( expr ), unaryOp ( - , ~ )
		term, err = compileTermSymbol(tkns, tkn.value)
		if err != nil {
			return term, err
		}

	default:
		return term, fmt.Errorf("unexpected value: '%s', expected term", tkn.value)
	}

	return term, nil
}

//term define by symbol:  ( expr ) or unaryOp ( - , ~ )
func compileTermSymbol(tkns *Tokens, tknVal string) (term Term, err error) {
	switch tknVal {
	case "(":
		term.Typ = expression
		term.Value, err = compileExprs(tkns)
		if err != nil {
			return term, err
		}

		err = needToken(tkns, symbol, ")", expression)
		if err != nil {
			return term, err
		}
	case "-", "~":
		//unaryOp
		term2, err := compileTerm(tkns)
		if err != nil {
			return term, err
		}

		term.Typ = unary
		term.Value = UnaryOpTerm{
			UnaryOp: tknVal,
			Term:    term2,
		}
	default:
		return term, fmt.Errorf("unexpected value: %s", tknVal)
	}

	return term, nil
}

//term def by identifier: varName or varName[expr] or subroutineCall
func compileTermIdentifier(tkns *Tokens) (term interface{}, err error) {
	save := tkns.Get()

	if !tkns.Next() {
		return term, fmt.Errorf("unexpected end of file")
	}

	tkn := tkns.Get()
	tkns.PushBack()
	tkns.AddToStart(save)

	switch tkn.value {
	case ".", "(": // subroutineCall
		return compileSubroutineCall(tkns)
	default: // varName or varName[expr]
		return compileVarName(tkns)
	}
}

//subroutineCall
func compileSubroutineCall(tkns *Tokens) (sc SubroutineCall, err error) {
	firstTok, err := needType(tkns, identifier)
	if err != nil {
		return sc, err
	}

	secTok, err := needType(tkns, symbol)
	if err != nil {
		return sc, err
	}

	switch secTok.value {
	case "(":
		sc.Name = firstTok.value
		sc.ExprList, err = compileExprList(tkns)
		if err != nil {
			return sc, err
		}
	case ".":
		tok, err := needType(tkns, identifier)
		if err != nil {
			return sc, err
		}

		err = needToken(tkns, symbol, "(", function)
		if err != nil {
			return sc, err
		}

		sc.ExprList, err = compileExprList(tkns)
		if err != nil {
			return sc, err
		}

		sc.ClassOrVarName = firstTok.value
		sc.Name = tok.value


	default:
		return sc, fmt.Errorf("unexpected token in subroutine call: '%s'", secTok.value)
	}

	err = needToken(tkns, symbol, ")", function)
	if err != nil {
		return sc, err
	}

	return sc, err
}

//expressionList
func compileExprList(tkns *Tokens) (exprs []Expression, err error) {
	ok, err := checkToken(tkns, symbol, ")", expression)
	if err != nil {
		return exprs, err
	}
	if ok {
		return nil, nil
	}

	expr, err := compileExprs(tkns)
	if err != nil {
		return exprs, err
	}

	exprs = append(exprs, expr)

	f := func() bool {
		ok, err = checkToken(tkns, symbol, ",", expression)
		if err != nil {
			return false
		}
		return ok
	}

	for f() {
		tkns.Next()
		expr, err := compileExprs(tkns)
		if err != nil {
			return exprs, err
		}

		exprs = append(exprs, expr)
	}
	if err != nil {
		return exprs, err
	}

	return exprs, nil
}

func needToken(tkns *Tokens, typ, val, stmtTyp string) error {
	if !tkns.Next() {
		return fmt.Errorf("unexpected end of file in %s statement, want %s", stmtTyp, val)
	}
	tkn := tkns.Get()
	if tkn.typ != typ || tkn.value != val {
		return fmt.Errorf("expected '%s' in %s statement, have '%s'", val, stmtTyp, tkn.value)
	}
	return nil
}

func needType(tkns *Tokens, typNeeded string) (tkn Tkn, err error) {
	if !tkns.Next() {
		return tkn, fmt.Errorf("unexpected end of file")
	}

	tkn = tkns.Get()

	if tkn.typ != typNeeded {
		return tkn, fmt.Errorf("expected %s, have %s - %s", typNeeded, tkn.typ, tkn.value)
	}

	return tkn, nil
}

func checkToken(tkns *Tokens, typ, val, stmtTyp string) (bool, error) {
	if !tkns.Next() {
		return false, fmt.Errorf("unexpected end of file near %s statement", stmtTyp)
	}
	tkn := tkns.Get()
	if tkn.typ != typ || tkn.value != val {
		tkns.PushBack()
		return false, nil
	}

	tkns.PushBack()
	return true, nil
}

func isOP(op string) bool {
	switch op {
	case "+", "-", "*", "/", "&amp;", "|", "&lt;", "&gt;", "=":
		return true
	}
	return false
}

// // // // /
// Tokenizer
type Tkn struct {
	typ    string
	value     string
}

type Tokens struct {
	token Tkn
	_next *Tokens
	last  *Tokens
}

func tokenize(r io.Reader) *Tokens {
	sc := bufio.NewScanner(r)
	tkns := &Tokens{}

	var line string
	commentOpened := false

	for sc.Scan() {
		line, commentOpened = filterComments(sc.Text(), commentOpened)
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		splitOnTokens(line, tkns)
	}
	if err := sc.Err(); err != nil {
		panic("can't read file: " + err.Error())
	}

	//tkns.End()
	return tkns
}

const (
	symbol = "symbol"
	keyword = "keyword"
	stringConstant = "stringConstant"
	integerConstant = "integerConstant"
	identifier = "identifier"
	unary = "unary"
	expression = "expression"
)

func splitOnTokens(str string, tkns *Tokens) {
	lexElem := ""

	for i := 0; i < len(str); i++ {
		switch {
		case isSymbol(str[i:], &lexElem, &i):
			tkns.AddToEnd(Tkn{symbol, lexElem})
		case isKeyword(str[i:], &lexElem, &i):
			tkns.AddToEnd(Tkn{keyword, lexElem})
		case isStringConst(str[i:], &lexElem, &i):
			tkns.AddToEnd(Tkn{stringConstant, lexElem})
		case isIntConst(str[i:], &lexElem, &i):
			tkns.AddToEnd(Tkn{integerConstant, lexElem})
		case isIdentifier(str[i:], &lexElem, &i):
			tkns.AddToEnd(Tkn{identifier, lexElem})
		default:
			fmt.Println("Tokenizer: unknown field", str)
			i++
		}

		if i < len(str) {
			str = strings.TrimSpace(str[i:])
			i = -1
		}
	}
}

//symbol
func isSymb(c uint8) bool {
	switch c {
	case '{', '}', '(', ')', '[', ']',
		'.', ',', ';', '+', '-', '*', '/',
		'&', '|', '<', '>', '=', '~':
		return true
	}
	return false
}

func isSymbol(line string, write *string, i *int) bool {
	if isSymb(line[0]) {
		*write = convertForXML(line[:1])
		*i = 1
		return true
	}
	return false
}

func convertForXML(s string) string {
	switch s {
	case "<":
		return "&lt;"
	case ">":
		return "&gt;"
	case `"`:
		return "&quot;"
	case "&":
		return "&amp;"
	default:
		return s
	}
}

//keyword
func isKeyword(line string, write *string, i *int) bool {
	if isKWRightSp(line, write, i) || isKWAdjToSymbol(line, write, i) {
		return true
	}
	return false
}

const (
	trueKW = "true"
	falseKW = "false"
	null = "null"
	this = "this"
	ifKW = "if"
	elseKW = "else"
	while = "while"
	returnKW = "return"
)

var needSymbolOrSpace = []string{
	trueKW, falseKW, null, this,
	ifKW, elseKW, while, returnKW}

func isKWAdjToSymbol(line string, write *string, i *int) bool {
	for _, pattern := range needSymbolOrSpace {
		if strings.HasPrefix(line, pattern) {
			ptrLen := len(pattern)
			lineLen := len(line)

			if lineLen == ptrLen ||
				(lineLen > ptrLen &&
					(unicode.IsSpace(rune(line[ptrLen])) ||
						isSymb(line[ptrLen]))) {
				*write = pattern
				*i = ptrLen
				return true
			}
		}
	}
	return false
}

const (
	class       = "class"
	constructor = "constructor"
	function    = "function"
	method      = "method"
	field       = "field"
	static      = "static"
	varKW       = "var"
	intKW       = "int"
	char        = "char"
	boolean     = "boolean"
	void        = "void"
	let         = "let"
	do          = "do"
)

var needRightSpaceOrLn = []string{
	class, constructor, function,
	method, field, static, varKW,
	intKW, char, boolean, void,
	let, do}

func isKWRightSp(line string, write *string, i *int) bool {
	for _, pattern := range needRightSpaceOrLn {
		if strings.HasPrefix(line, pattern) {
			ptrLen := len(pattern)
			if len(line) == ptrLen || (len(line) > ptrLen &&
				unicode.IsSpace(rune(line[ptrLen]))) {
				*write = pattern
				*i = ptrLen + 1
				return true
			}
		}
	}
	return false
}

//string
func isStringConst(line string, write *string, i *int) bool {
	if strings.HasPrefix(line, `"`) {
		for j := 1; j < len(line); j++ {
			if line[j] == '"' {
				*write = line[1:j]
				*i = j + 1
				return true
			}
		}
	}
	return false
}

//integer
func isIntConst(line string, write *string, i *int) bool {
	if !unicode.IsDigit(rune(line[0])) {
		return false
	}

	for j := 1; j < len(line); j++ {
		c := line[j]
		if isSymb(c) || unicode.IsSpace(rune(c)) {
			*write = line[:j]
			*i = j
			return true
		} else if c < 48 || c > 57 {
			return false
		}
	}

	return false
}

//identifier
func isIdentifier(line string, write *string, i *int) bool {
	if unicode.IsDigit(rune(line[0])) {
		return false
	}

	for j := 1; j < len(line); j++ {
		c := line[j]
		if isSymb(c) || unicode.IsSpace(rune(c)) {
			*write = line[:j]
			*i = j
			return true
		} else if !unicode.IsDigit(rune(c)) &&
			c != '_' && !isAlpha(c) {
			return false
		}
	}
	return false
}

func isAlpha(c uint8) bool {
	switch {
	case (c > 64 && c < 91) || (c > 96 && c < 123):
		return true
	}
	return false
}

func filterComments(rawLine string, cmtOpened bool) (line string, commentOpened bool) {
	if cmtOpened {
		//	/*cmt*/After
		rawLine, cmtOpened = getLineAfterCmt(rawLine)
		if cmtOpened {
			return "", true
		}
	}

	var leftOver, lin string
	line, leftOver, cmtOpened = getLineBeforeCmt(rawLine)

	// Before/*cmt*/After
	// line +leftover....
	for leftOver != "" { // cmtOpened
		//parse leftOver and find after
		lin, cmtOpened = getLineAfterCmt(leftOver)
		if cmtOpened { //no closing symbol
			break
		}

		lin, leftOver, cmtOpened = getLineBeforeCmt(lin)
		line += " " + lin
	}

	return line, cmtOpened
}

func getLineBeforeCmt(rawLine string) (line string, leftOver string, cmtOpened bool) {
	//	Before/*cmt*/
	for i := range rawLine {
		if strings.HasPrefix(rawLine[i:], "/*") {
			return rawLine[:i], rawLine[i+2:], true
		} else if strings.HasPrefix(rawLine[i:], "//") {
			return rawLine[:i], "", false
		}
	}
	return rawLine, "", false
}

func getLineAfterCmt(rawLine string) (line string, cmtOpened bool) {
	//	/*cmt*/After
	for i := range rawLine {
		if strings.HasPrefix(rawLine[i:], "*/") {
			return rawLine[i+2:], false
		}
	}
	return "", true
}

func (t *Tokens) AddToEnd(newTkn Tkn) {
	switch {
	case t._next != nil:
		t.last._next = &Tokens{token: newTkn}
		t.last = t.last._next
	case t._next == nil:
		t._next = &Tokens{token: newTkn}
		t.last = t._next
	}
}

func (t *Tokens) AddToStart(newTkn Tkn) {
	t._next = &Tokens{
		token: newTkn,
		_next: t._next,
	}
	if t.last == nil {
		t.last = t._next
	}
}

func (t *Tokens) Next() bool {
	if t._next != nil {
		*t = Tokens{
			token: t._next.token,
			_next: t._next._next,
			last:  t.last,
		}
		return true
	}
	return false
}

func (t *Tokens) Get() Tkn {
	return t.token
}

//func (t *Tokens) End() {
//	t._next = &Tokens{
//		token: t.token,
//		_next: t._next,
//		last:  t.last,
//	}
//}

func (t *Tokens) PushBack() {
	t._next = &Tokens{
		token: t.token,
		_next: t._next,
		last:  t.last,
	}
}

type compileFiles struct {
	files []string
	dir   string
}

func getFiles(av string) (cf compileFiles) {
	av = strings.TrimRight(av, "/")
	stat, err := os.Stat(av)
	if err != nil {
		panic("can't stat argv: " + err.Error())
	}

	switch typ := stat.Mode(); {
	case typ.IsRegular():
		if !strings.HasSuffix(av, ".jack") {
			panic("unsupported type to compile")
		}
		cf.files = []string{av}
	case typ.IsDir():
		files, err := ioutil.ReadDir(av)
		if err != nil {
			panic("cant read dir: " + err.Error())
		}

		cf.dir = av + "/"
		for _, f := range files {
			if strings.HasSuffix(f.Name(), ".jack") {
				cf.files = append(cf.files, f.Name())
			}
		}
	default:
		panic("unsupported file type to compile")
	}

	return cf
}

// ######

func printTkns(tkns *Tokens, tokenFile *os.File) {
	_, err := tokenFile.WriteString("<tokens>\n")
	if err != nil {
		panic("cant write to file: "+err.Error())
	}

	for tkns.Next() {
		tk := tkns.Get()
		_, err = tokenFile.WriteString(
			"<"+tk.typ+"> "+tk.value+" </"+tk.typ+">\n")
		if err != nil {
			panic("cant write to file: "+err.Error())
		}
	}

	_, err = tokenFile.WriteString("</tokens>\n")
	if err != nil {
		panic("cant write to file: "+err.Error())
	}
}

func inSlice(needle string, hay []string) bool {
	for i := range hay {
		if needle == hay[i] {
			return true
		}
	}
	return false
}

func printTknsLn(tkns *Tokens, wFile *os.File) {
	for tkns.Next() {
		fmt.Println(tkns.Get())
	}
}

func testTokens() {
	t := &Tokens{}
	for i := 0; i < 10; i++ {
		t.AddToEnd(Tkn{
			typ:   "a" + strconv.Itoa(i),
			value: strconv.Itoa(i),
		})
	}

	t.Next()
	fmt.Println(t.Get())
	t.AddToEnd(Tkn{
		typ:   "extra",
		value: "10",
	})

	cop := *t

	for t.Next() {
		fmt.Println(t.Get())
	}
	fmt.Println()

	c := &cop
	c.AddToEnd(Tkn{
		typ:   "extra",
		value: "11",
	})
	c.AddToStart(Tkn{"extraFirst", "-0"})

	for c.Next() {
		fmt.Println(c.Get())
	}

	return
}

func testComment() {
	//line := `assdad//aksjdk//\sad//*asda*/payload/*againComment*/AgainPayload/*comm*/`
	line := `// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/10/Square/SquareGame.jack

// (same as projects/09/Square/SquareGame.jack)

/**
 * Implements the Square Dance game.
 * This simple game allows the user to move a black square around
 * the screen, and change the square's size during the movement.
 * When the game starts, a square of 30 by 30 pixels is shown at the
 * top-left corner of the screen. The user controls the square as follows.
 * The 4 arrow keys are used to move the square up, down, left, and right.
 * The 'z' and 'x' keys are used, respectively, to decrement and increment
 * the square's size. The 'q' key is used to quit the game.
 */

class SquareGame {
   field Square square; // the square of this game
   field int direction; // the square's current direction: 
                        // 0=none, 1=up, 2=down, 3=left, 4=right

   /** Constructs a new Square Game. */
   constructor SquareGame new() {
      // Creates a 30 by 30 pixels square and positions it at the top-left
      // of the screen.
      let square = Square.new(0, 0, 30);
      let direction = 0;  // initial state is no movement
      return this;
   }

   /** Disposes this game. */
   method void dispose() {
      do square.dispose();
      do Memory.deAlloc(this);
      return;
   }

   /** Moves the square in the current direction. */
   method void moveSquare() {
      if (direction = 1) { do square.moveUp(); }
      if (direction = 2) { do square.moveDown(); }
      if (direction = 3) { do square.moveLeft(); }
      if (direction = 4) { do square.moveRight(); }
      do Sys.wait(5);  // delays the next movement
      return;
   }

   /** Runs the game: handles the user's inputs and moves the square accordingly */
   method void run() {
      var char key;  // the key currently pressed by the user
      var boolean exit;
      let exit = false;
      
      while (~exit) {
         // waits for a key to be pressed
         while (key = 0) {
            let key = Keyboard.keyPressed();
            do moveSquare();
         }
         if (key = 81)  { let exit = true; }     // q key
         if (key = 90)  { do square.decSize(); } // z key
         if (key = 88)  { do square.incSize(); } // x key
         if (key = 131) { let direction = 1; }   // up arrow
         if (key = 133) { let direction = 2; }   // down arrow
         if (key = 130) { let direction = 3; }   // left arrow
         if (key = 132) { let direction = 4; }   // right arrow

         // waits for the key to be released
         while (~(key = 0)) {
            let key = Keyboard.keyPressed();
            do moveSquare();
         }
     } // while
     return;
   }
}



`
	r := strings.NewReader(line)

	compile(r, nil, nil)
}