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

type className string
type funcName string
type funcType string
type varName string
type clsFuncs map[funcName] funcType

type funcs struct {
	cName className
	fName funcName
}

func main() {
	flsName := getFiles(os.Args[1])

	classMap := make(map[className]clsFuncs)
	funcsNeeded := make(map[funcs]struct{}, len(flsName.files))
	typesNeeded := make(map[string]struct{})

	for _, fName := range flsName.files {
		fName = flsName.dir + fName
		rFile, err := os.Open(fName)
		if err != nil {
			panic("can't open file to read: " + err.Error())
		}
		defer rFile.Close()

		fName = strings.TrimSuffix(fName, ".jack")

		vmFile, err := os.Create(fName + ".vm")
		if err != nil {

			panic("can't create file to write: " + err.Error())
		}
		defer vmFile.Close()

		clsName, clsFuncs, err := compile(rFile, vmFile, funcsNeeded, typesNeeded)
		if err != nil {
			fmt.Println("Compile error:", err)
			os.Exit(1)
		}

		classMap[clsName] = clsFuncs
	}

	if err := checkFuncs(classMap, funcsNeeded, typesNeeded); err != nil {
		fmt.Println(err)
	}
}

func checkFuncs(classMap map[className]clsFuncs, funcsN map[funcs]struct{}, typesN map[string]struct{}) error {
	//if os pass todo
	return nil
}

func compile(rFile io.Reader, astFile *os.File,
	funcsNeeded map[funcs]struct{}, typesNeeded map[string]struct{}) (
	clsName className, clsFuncs clsFuncs, err error)  {

	tkns := tokenize(rFile)
	cpTkns := *tkns

	class, err := makeAST(&cpTkns)
	if err != nil {
		return clsName, clsFuncs, err
	}

	cc := classCompile{
		class:       class,
		clsST:       map[varName]varS{},
		funcST:      nil,
		funcsNeeded: funcsNeeded,
		typesNeeded: typesNeeded,
	}

	clsFuncs = generateVM(&cc, astFile)

	return className(class.Identifier), clsFuncs, err
}

type varS struct {
	//name string
	typ string
	scope string
	count int
}

type classCompile struct {
	class *Class

	clsST map[varName]varS
	funcST map[varName]varS

	funcsNeeded map[funcs]struct{}
	typesNeeded map[string]struct{}
}

func generateVM(cc *classCompile, astFile *os.File) (clsFuncs clsFuncs) {

	cc.clsST = getClassVarDecs(cc, cc.class.ClassVarDecs)

	clsFuncs = printClassFuncs(cc, astFile)

	return clsFuncs
}

func printClassFuncs(cc *classCompile, file *os.File) (clsFunc clsFuncs) {

	clsFunc = clsFuncs{}

	for i := range cc.class.FuncDec {
		f := cc.class.FuncDec[i]
		cc.funcST = map[varName]varS{}

		if f.ReturnType.Custom && f.ReturnType.Name != cc.class.Identifier {
			cc.typesNeeded[f.ReturnType.Name] = struct{}{}
		}

		clsFunc[funcName(f.Identifier)] = funcType(f.Typ)

		initFuncParams(cc, f.Params, f.Typ)
		lcl := initVarDecs(cc, f.Body.VarDecs)

		_, err := file.WriteString(
			"function " + cc.class.Identifier + "." + f.Identifier + " " + lcl + "\n" +
				popPointerCallingFunc(cc, f.Typ) +
				printStatements(cc, f.Body.Statements),
		)

		if err != nil {
			panic("cant write to file: " + err.Error())
		}
	}

	return clsFunc
}

func popPointerCallingFunc(cc *classCompile, typ string) string {
	switch typ {
	case constructor:
		flds := 0
		for _, vs := range cc.clsST {
			if vs.scope == field {
				flds++
			}
		}

		return "push constant " + strconv.Itoa(flds) + "\n" +
			"call Memory.alloc 1\n" +
			"pop pointer 0\n"

	case method:
		return "push argument 0\n" +
				"pop pointer 0\n"
	}

	return ""
}

func printStatements(cc *classCompile, stmts []Statement) (s string) {

	for i := range stmts {
		stmt := stmts[i]

		switch stmt.(type) {
		case LetSt:
			s += printLet(cc, stmt.(LetSt))
		case IfSt:
			s += printIf(cc, stmt.(IfSt))
		case WhileSt:
			s += printWhile(cc, stmt.(WhileSt))
		case DoSt:
			s += printDo(cc, stmt.(DoSt))
		case ReturnSt:
			s += printRet(cc, stmt.(ReturnSt))
		default:
			panic("undefined statement")
		}
	}

	return s
}

func printRet(cc *classCompile, stmt ReturnSt) (s string) {
	if stmt != (ReturnSt{}) {
		s += printExpr(cc, Expression(stmt))
	} else {
		s += "push constant 0\n"
	}
	return s + "return\n"
}

func printDo(cc *classCompile, stmt DoSt) (s string) {
	s = printFuncCall(cc, SubroutineCall(stmt))
	s += "pop temp 0\n"
	return s
}

func printFuncCall(cc *classCompile, sc SubroutineCall) (s string) {
	call := ""
	pushedArgc := 0

	if sc.ClassOrVarName != "" {
		//need class name when calling in same class
		classN := getClassName(cc, sc.ClassOrVarName)
		if classN == "" {
			classN = sc.ClassOrVarName
		}

		call = "call " + classN + "." + sc.Name + " "
		s, pushedArgc = callMethod(cc, sc.ClassOrVarName)

		cc.funcsNeeded[ funcs{className(sc.ClassOrVarName),funcName(sc.Name)} ] = struct{}{}
	} else {
		s, pushedArgc = pushIfMethod(cc, sc.Name)
		call = "call " + cc.class.Identifier + "." + sc.Name + " "

		cc.funcsNeeded[funcs{className(cc.class.Identifier), funcName(sc.Name)} ] = struct{}{}
	}


	for i := range sc.ExprList {
		s += printExpr(cc, sc.ExprList[i])
	}

	pushedArgc += len(sc.ExprList)

	return s + call + strconv.Itoa(pushedArgc) + "\n"
}

func pushIfMethod(cc *classCompile, name string) (string, int) {
	for i := range cc.class.FuncDec {
		f := cc.class.FuncDec[i]

		if f.Identifier == name {
			if f.Typ == method {
				return writePush("pointer", 0), 1
			}
			break
		}
	}
	return "", 0
}

func getClassName(cc *classCompile, name string) (clsName string) {
	if v, ok := cc.funcST[varName(name)]; ok {
		clsName = v.typ
	} else if v, ok := cc.clsST[varName(name)]; ok {
		clsName = v.typ
	}

	return clsName
}

func callMethod(cc *classCompile, vn string) (s string, c int) {
	if v, ok := cc.funcST[varName(vn)]; ok {
		s = writePush(getScope(v.scope), v.count)
		c = 1
	} else if v, ok := cc.clsST[varName(vn)]; ok {
		s = writePush(getScope(v.scope), v.count)
		c = 1
	}

	return s, c
}

func writePop(scope string, n int) string {
	return "pop " + scope + " " + strconv.Itoa(n) + "\n"
}

func writePush(scope string, n int) string {
	return "push " + scope + " " + strconv.Itoa(n) + "\n"
}

func getScope(scope string) string {
	switch scope {
	case field: return this
	case varKW: return "local"
	case static, argument: return scope
	default:
		panic("WHAT IS THAT SCOPE?: "+scope)
	}
}

var (
	labelIf = 0
	labelWhile = 0
)

func printWhile(cc *classCompile, stmt WhileSt) (s string) {
	endWhile := "end.WHILE." + strconv.Itoa(labelWhile) + "\n"
	start := "WHILE." + strconv.Itoa(labelWhile) + "\n"
	labelWhile++

	s = "label " + start +
		printExpr(cc, stmt.Expr) +
		"not\n" +
		"if-goto " + endWhile +
		printStatements(cc, stmt.Statements) +
		"goto " + start +
		"label " + endWhile

	return s
}

func printIf(cc *classCompile, stmt IfSt) (s string) {
	elseSt := "else.IF." + strconv.Itoa(labelIf) + "\n"
	done := "done.IF." + strconv.Itoa(labelIf) + "\n"
	labelIf++

	s = printExpr(cc, stmt.Expr) +
		"not\n" +
		"if-goto " + elseSt +
		printStatements(cc, stmt.Statements) +
		"goto " + done +
		"label " + elseSt

	if stmt.ElseSts != nil {
		s += printStatements(cc, stmt.ElseSts)
	}

	s += "label " + done

	return s
}

func printLet(cc *classCompile, stmt LetSt) (s string) {
	s = printExpr(cc, stmt.Expr)
	//		s += writePop("temp", 0)
	pushArr, isArr := printVar(cc, stmt.VarName)
	//s += ss

	if isArr {
		s += writePop("temp", 0) +
			pushArr +
			writePop("pointer", 1) +
			writePush("temp", 0) +
			writePop("that", 0)
	} else {
		v := getVar(cc, stmt.VarName.Name)
		s += writePop(getScope(v.scope), v.count)
	}

	return s
}

func getVar(cc *classCompile, vn string) varS {
	if v, ok := cc.funcST[varName(vn)]; ok {
		return v
	} else if v, ok := cc.clsST[varName(vn)]; ok {
		return v
	} else {
		panic("let statement: var doesn't exist")
	}
}

func printExpr(cc *classCompile, expr Expression) (s string) {
	s = printTerm(cc, expr.Term)

	if expr.OP != "" {
		s += printTerm(cc, *expr.OPTerm) +
			printArifmethic(expr.OP)
	}

	return s
}

func printArifmethic(op string) (s string) {
	switch op {
	case "+": s = "add"
	case "-": s = "sub"
	case "*": s = "call Math.multiply 2"
	case "/": s = "call Math.divide 2"
	case "&amp;": s = "and"
	case "|": s = "or"
	case "&lt;": s = "lt"
	case "&gt;": s = "gt"
	case "=": s = "eq"
	default:
		panic("WHAAAAAT?! WHO IS THAT UNDEFINED OPERATOR::>>> "+s)
	}

	return s + "\n"
}

func printTerm(cc *classCompile, term Term) (s string) {

	switch term.Typ {
	case integerConstant:
		v := term.Value.(string)
		n, _ := strconv.Atoi(v)
		s = writePush("constant", n)
		if n, _ := strconv.Atoi(term.Value.(string)); n < 0 {
			s += "neg\n"
		}
	case stringConstant:
		s = printString(term.Value.(string))
	case keyword:
		//"true" && v != "false" && v != "null" && v != "this" {
		switch val := term.Value.(string); val {
		case trueKW:
			s = writePush("constant", 0) + "not\n"
		case falseKW, null:
			s = writePush("constant", 0)
		case this:
			s = writePush("pointer", 0)
		}
	case identifier:
		switch term.Value.(type) {
		case SubroutineCall:
			s = printFuncCall(cc, term.Value.(SubroutineCall))
		case Var:
			s, _ = printVar2(cc, term.Value.(Var))
		}
	case expression:
		s = printExpr(cc, term.Value.(Expression))
	case unary:
		uo := term.Value.(UnaryOpTerm)
		s = printTerm(cc, uo.Term)
		if uo.UnaryOp == "-" {
			s += "neg\n"
		} else {
			s += "not\n"
		}
	default:
		panic("undefined term")
	}

	return s
}

func printString(str string) (s string) {
	s = writePush("constant", len(str)) +
		"call String.new 1\n"

	for i := range str {
		s += writePush("constant", int(str[i])) +
			"call String.appendChar 2\n"
	}

	return s
}

func printVar(cc *classCompile, vr Var) (s string, isArr bool) {
	v := getVar(cc, vr.Name)
	s = writePush(getScope(v.scope), v.count)

	if vr.Expr != (Expression{}) {
		s += printExpr(cc, vr.Expr) +
			"add\n"
		isArr = true
	}

	return s, isArr
}

func printVar2(cc *classCompile, vr Var) (s string, isArr bool) {
	v := getVar(cc, vr.Name)
	s = writePush(getScope(v.scope), v.count)

	if vr.Expr != (Expression{}) {
		s += printExpr(cc, vr.Expr) +
			"add\n" +
			writePop("pointer", 1) +
			writePush("that", 0)
		isArr = true
	}

	return s, isArr
}

func initVarDecs(cc *classCompile, decs []VarDec) string {
	lcl := 0

	for i := range decs {
		v := decs[i]

		if v.Typ.Custom && v.Typ.Name != cc.class.Identifier {
			cc.typesNeeded[v.Typ.Name] = struct{}{}
		}

		for j := range v.Identifier {
			cc.funcST[varName(v.Identifier[j])] = varS{
				//name:  "",
				typ:   v.Typ.Name,
				scope: v.Scope,
				count: lcl,
			}
			lcl++
		}
	}

	return strconv.Itoa(lcl)
}

func initFuncParams(cc *classCompile, params []Param, typ string) {

	sc := map[string]int{}

	if typ == method {
		cc.funcST[this] = varS{
			typ:   cc.class.Identifier,
			scope: argument,
			count: 0,
		}
		sc[argument]++
	}

	for i := range params {
		p := params[i]

		cc.funcST[varName(p.Name)] = varS{
			typ:   p.Typ.Name,
			scope: argument,
			count: sc[argument],
		}

		if p.Typ.Custom && p.Typ.Name != cc.class.Identifier {
			cc.typesNeeded[p.Typ.Name] = struct{}{}
		}

		sc[argument]++
	}
}

func getClassVarDecs(cc *classCompile, varsd []VarDec) (
	st map[varName]varS) {

	scopeCounter := map[string]int{}
	st = map[varName]varS{}

	for i := range varsd {
		v := varsd[i]

		if v.Typ.Custom && v.Typ.Name != cc.class.Identifier {
			cc.typesNeeded[v.Typ.Name] = struct{}{}
		}

		for j := range v.Identifier {
			st[varName(v.Identifier[j])] = varS{
				//name:  "",
				typ:   v.Typ.Name,
				scope: v.Scope,
				count: scopeCounter[v.Scope],
			}
			scopeCounter[v.Scope]++
		}
	}

	return st
}


// ######
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
func makeAST(tkns *Tokens) (cls *Class, err error) {
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

	stmts, _, err := compileStatements(tkns)
	if err != nil {
		return rb, err
	}
	//} else if !retStmt {
	//	return rb, fmt.Errorf("expected return statement in func body")
	//}

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

	rett, err := compileExprs(tkns)
	if err != nil {
		return ret, err
	}

	return ReturnSt(rett), needToken(tkns, symbol, ";", returnKW)
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
	argument 	= "argument"
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

func inSlice(needle string, hay []string) bool {
	for i := range hay {
		if needle == hay[i] {
			return true
		}
	}
	return false
}
