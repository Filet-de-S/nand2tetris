package main

import (
	"bufio"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

var rawFileName = ""
var funcName = ""

func main() {
	filesToRead, fileToWrite := _init()
	defer func() {
		fileToWrite.Close()
		for _, readF := range filesToRead {
			readF.Close()
		}
	}()

	err := translate(filesToRead, fileToWrite)
	if err != nil {
		panic("err during translate: " + err.Error())
	}
}

func translate(filesToRead []*os.File, w *os.File) error {
	if len(filesToRead) > 1 {
		initCall, _ := callFunc([]string{"", "Sys.init", "0"})
		_, err := w.WriteString(`@256
D=A
@SP
M=D
` + initCall)
		if err != nil {
			return fmt.Errorf("write: %v", err)
		}
	}

	for _, f := range filesToRead {
		scan := bufio.NewScanner(f)
		rawFileName = strings.TrimRight(f.Name(), ".vm")
		splt := strings.Split(rawFileName, "/")
		rawFileName = splt[len(splt)-1]

		for scan.Scan() {
			if len(scan.Text()) == 0 || scan.Text()[0] == '/' {
				continue
			}

			if strings.Contains(scan.Text(), "function") {
				funcName = strings.Fields(scan.Text())[1]
			}

			asmCode, err := parse(scan.Text())
			if err != nil {
				return fmt.Errorf("parse: %v", err)
			}

			_, err = w.WriteString("// " + scan.Text() + "\n" +
				asmCode)
			if err != nil {
				return fmt.Errorf("write: %v", err)
			}
		}
		if err := scan.Err(); err != nil {
			return fmt.Errorf("read: %v", err)
		}

	}
	return nil
}

func parse(vm string) (string, error) {
	cmd := strings.Fields(vm)

	switch cmd[0] {
	case "push", "pop":
		return memoryAccess(cmd)
	case "add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not":
		return arithmeticLogical(cmd[0])
	case "label", "goto", "if-goto":
		return labelCmd(cmd)
	case "function":
		return functionDecl(cmd)
	case "return":
		return returnCmd()
	case "call":
		return callFunc(cmd)
	default:
		return "", fmt.Errorf("unknown cmd: %v", cmd[0])
	}
}

var calleeIdx = 0
func callFunc(cmd_ []string) (string, error) {
	callee := cmd_[1]
	argc := cmd_[2]

	calleeIdx++
	return fmt.Sprintf(
		`@AFTER_CALL::%s::RET_ADDR.%d
D=A
@SP
M=M+1
A=M-1
M=D
@LCL
D=M
@SP
M=M+1
A=M-1
M=D
@ARG
D=M
@SP
M=M+1
A=M-1
M=D
@THIS
D=M
@SP
M=M+1
A=M-1
M=D
@THAT
D=M
@SP
M=M+1
A=M-1
M=D
@5
D=A
@SP
D=M-D
@%s
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@%s
0;JMP
(AFTER_CALL::%s::RET_ADDR.%d)
`,
callee, calleeIdx, argc, callee, callee, calleeIdx),
nil
}

func returnCmd() (string, error) {
	//r13 ret
	//r14 frame
	return `@LCL
D=M
@5
D=D-A
@R14
AM=D
D=M
@R13
M=D
@SP
A=M-1
D=M
@ARG
A=M
M=D
@ARG
D=M+1
@SP
M=D
@R14
AM=M+1
D=M
@LCL
M=D
@R14
AM=M+1
D=M
@ARG
M=D
@R14
AM=M+1
D=M
@THIS
M=D
@R14
AM=M+1
D=M
@THAT
M=D
@R13
A=M
0;JMP
`, nil

}

func functionDecl(cmd []string) (string, error) {
	funcName := cmd[1]
	lclArgs := cmd[2]
	lclArgsNum, err := strconv.Atoi(lclArgs)
	if err != nil {
		return "", fmt.Errorf("must be num, have %s. Err: %s", lclArgs, err)
	}

	funcDecl := fmt.Sprintf("(%s)\n", funcName)

	for i := 0; i < lclArgsNum; i++ {
		funcDecl += `@SP
M=M+1
A=M-1
M=0
`
	}
	return funcDecl, nil
}

var lblIdx = 0
func labelCmd(vm []string) (string, error) {
	cmd := vm[0]
	label := funcName + "$" + vm[1]
	lblIdx++

	switch cmd {
	case "label":
		return fmt.Sprintf("(%s)\n", label), nil
	case "goto":
		return fmt.Sprintf("@%s\n0;JMP\n", label), nil
	case "if-goto":
		return fmt.Sprintf(
			`@SP
AM=M-1
D=M
@FALSE.%d
D;JEQ
@%s
0;JMP
(FALSE.%d)
`, lblIdx, label, lblIdx), nil
	}
	
	return "", nil
}

func memoryAccess(vm []string) (string, error) {
	if len(vm) < 3 {
		return "", fmt.Errorf("wrong cmd format: %v", strings.Join(vm, " "))
	}
	cmd := vm[0]
	if cmd != "push" && cmd != "pop" {
		return "", fmt.Errorf("unknown cmd: %s", cmd)
	}
	segment := vm[1]
	idx := vm[2]
	idxNum, err := strconv.Atoi(idx)
	if err != nil {
		return "", errors.New("idx must be num: "+err.Error())
	}

	switch segment {
	case "constant":
		return pushConstant(idx), nil
	case "local", "argument", "this", "that":
		if cmd == "push" {
			return pushBaseAddr(idx, getSegment(segment)), nil
		}
		return popBaseAddr(idx, getSegment(segment)), nil
	case "pointer":
		if idxNum != 0 && idxNum != 1 {
			return "", fmt.Errorf("pointer must be 0 or 1, have %s", idx)
		}
		thisThat := ""
		if idxNum == 0 {
			thisThat = "THIS"
		} else {
			thisThat = "THAT"
		}
		if cmd == "push" {
			return pushPointerOrStatic(thisThat), nil
		}
		return popPointerOrStatic(thisThat), nil
	case "static":
		if cmd == "push" {
			return pushPointerOrStatic(rawFileName + "." + idx), nil
		}
		return popPointerOrStatic(rawFileName + "." + idx), nil
	case "temp":
		if cmd == "push" {
			return pushTemp(idx), nil
		}
		return popTemp(idx), nil
	default:
		return "", fmt.Errorf("bad mem segment: %s", segment)
	}
}

func pushConstant(idx string) string {
	return fmt.Sprintf(
		`@%s
D=A
@SP
M=M+1
A=M-1
M=D
`, idx)
}

func pushBaseAddr(idx, segment string) string {
	return fmt.Sprintf(
		`@%s
D=A
@%s
A=M+D
D=M
@SP
M=M+1
A=M-1
M=D
`, idx, segment)
}
func popBaseAddr(idx string, segment string) string {
	return fmt.Sprintf(
		`@%s
D=A
@%s
D=M+D
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
`, idx, segment)
}

func pushPointerOrStatic(addr string) string {
	return fmt.Sprintf(
		`@%s
D=M
@SP
M=M+1
A=M-1
M=D
`, addr)
}
func popPointerOrStatic(addr string) string {
	return fmt.Sprintf(
		`@SP
AM=M-1
D=M
@%s
M=D
`, addr)
}

func pushTemp(idx string) string {
	return fmt.Sprintf(
		`@5
D=A
@%s
A=A+D
D=M
@SP
M=M+1
A=M-1
M=D
`, idx)
}
func popTemp(idx string) string {
	return fmt.Sprintf(
		`@5
D=A
@%s
D=A+D
@R13
M=D
@SP
AM=M-1
D=M
@R13
A=M
M=D
`, idx)
}

func arithmeticLogical(cmd string) (string, error) {
	switch cmd {
	case "add", "sub", "and", "or":
		return arithmetical(cmd), nil
	case "neg", "not":
		return negNot(cmd), nil
	case "eq", "gt", "lt":
		return logical(cmd), nil
	default:
		return "", fmt.Errorf("bad arithmetical/logical cmd: %s", cmd)
	}
}

var end = 0
func logical(cmd string) string {
	jmp := ""
	switch cmd {
	case "eq":
		jmp = "JEQ"
	case "gt":
		jmp = "JGT"
	case "lt":
		jmp = "JLT"
	}
	end++
	return fmt.Sprintf(
		`@SP
AM=M-1
D=M
@SP
A=M-1
D=M-D
M=-1
@END.%d
D;%s
@SP
A=M-1
M=0
(END.%d)
`, end, jmp, end)
}

func negNot(cmd string) string {
	sign := ""
	switch cmd {
	case "neg":
		sign = "-"
	case "not":
		sign = "!"
	}
	return fmt.Sprintf(
		`@SP
A=M-1
M=%sM
`, sign)
}

func arithmetical(cmd string) string {
	sign := ""
	switch cmd {
	case "add":
		sign = "+"
	case "sub":
		sign = "-"
	case "and":
		sign = "&"
	case "or":
		sign = "|"
	}
	return fmt.Sprintf(
		`@SP
AM=M-1
D=M
@SP
A=M-1
M=M%sD
`, sign)
}

func getSegment(s string) string {
	switch s {
	case "local":
		return "LCL"
	case "argument":
		return "ARG"
	case "this":
		return "THIS"
	case "that":
		return "THAT"
	}
	return ""
}

func _init() (filesToRead []*os.File, fileToWrite *os.File) {
	if len(os.Args) != 2 {
		panic("invalid params: need 1")
	}

	arg := os.Args[1]
	filesToOpen, fileToWrite := filesToRW(arg, fileToWrite)

	for _, file := range filesToOpen {
		toRead, err := os.Open(file)
		if err != nil {
			fileToWrite.Close()
			for _, f := range filesToRead {
				f.Close()
			}
			panic("cant open file: " + err.Error())
		}

		filesToRead = append(filesToRead, toRead)
	}

	return filesToRead, fileToWrite
}

func filesToRW(arg string, fileToWrite *os.File) ([]string, *os.File) {
	arg = strings.TrimRight(arg, "/")
	fi, err := os.Stat(arg)
	if err != nil {
		panic("cant take stat from file: " + err.Error())
	}

	filesToOpen := []string{}
	fileToWriteName := ""

	switch typ := fi.Mode(); {
	case typ.IsDir():
		dirName := arg+"/"
		files, err := ioutil.ReadDir(arg)
		if err != nil {
			panic("cant read dir: " + err.Error())
		}

		for _, f := range files {
			if strings.HasSuffix(f.Name(), ".vm") {
				filesToOpen = append(filesToOpen, dirName + f.Name())
			}
		}

		splt := strings.Split(arg, "/")
		fileToWriteName = dirName+splt[len(splt)-1]
	case typ.IsRegular():
		if !strings.HasSuffix(arg, ".vm") {
			panic("wrong file extension. want fileName.vm")
		}
		filesToOpen = append(filesToOpen, arg)

		fileToWriteName = arg[:len(arg)-3]
	}

	fileToWrite, err = os.Create(fileToWriteName + ".asm")
	if err != nil {
		panic("cant create file: " + err.Error())
	}
	return filesToOpen, fileToWrite
}
