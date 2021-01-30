package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var rawFileName = ""

func main() {
	fileToRead, fileToWrite := _init()
	defer fileToRead.Close()
	defer fileToWrite.Close()

	err := translate(fileToRead, fileToWrite)
	if err != nil {
		panic("err during translate: " + err.Error())
	}
}

func translate(r *os.File, w *os.File) error {
	sc := bufio.NewScanner(r)

	for sc.Scan() {
		if len(sc.Text()) == 0 || sc.Text()[0] == '/' {
			continue
		}

		asmCode, err := parse(sc.Text())
		if err != nil {
			return fmt.Errorf("parse: %v", err)
		}

		_, err = w.WriteString("// "+ sc.Text() + "\n" + asmCode)
		if err != nil {
			return fmt.Errorf("write: %v", err)
		}
	}
	if err := sc.Err(); err != nil {
		return fmt.Errorf("read: %v", err)
	}

	return nil
}

func parse(vm string) (string, error) {
	cmd := strings.Split(vm, " ")

	switch cmd[0] {
	case "push", "pop":
		return memoryAccess(cmd)
	case "add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not":
		if len(cmd) > 1 {
			return "", fmt.Errorf("bad format: Arithmetical "+
				"Logical Cmd must have no args, have %v", len(cmd)-1)
		}
		return arithmeticLogical(cmd[0])
	// case "" // prog flow
	// case "" // func call
	default:
		return "", fmt.Errorf("unknown cmd: %v", cmd[0])
	}
}

func memoryAccess(vm []string) (string, error) {
	if len(vm) != 3 {
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
		return "", errors.New("idx must be num")
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
			return pushPointerOrStatic(rawFileName + idx), nil
		}
		return popPointerOrStatic(rawFileName + idx), nil
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
M=M-1
A=M
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
M=M-1
A=M
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
M=M-1
A=M
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
M=M-1
A=M
D=M
@SP
A=M-1
D=M-D
@TRUE.%d
D;%s
@SP
A=M-1
M=0
@END.%d
0;JMP
(TRUE.%d)
@SP
A=M-1
M=-1
(END.%d)
`, end, jmp, end, end, end)
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
M=M-1
A=M
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

func _init() (*os.File, *os.File) {
	if len(os.Args) != 2 {
		panic("invalid params: need 1")
	}

	f := os.Args[1]
	fileExt := strings.Split(f, ".")
	if fileExt[len(fileExt)-1] != "vm" {
		panic("wrong vm code format: " + fileExt[len(fileExt)-1])
	}

	fileToRead, err := os.Open(f)
	if err != nil {
		panic("cant open file: " + err.Error())
	}

	rawFileName = f[:len(f)-2]
	fileToWrite, err := os.Create(rawFileName + "asm")
	if err != nil {
		fileToRead.Close()
		panic("cant create file: " + err.Error())
	}
	excludeSlash := strings.Split(rawFileName, "/")
	rawFileName = excludeSlash[len(excludeSlash)-1]

	return fileToRead, fileToWrite
}
