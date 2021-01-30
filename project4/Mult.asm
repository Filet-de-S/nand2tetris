// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.

// if (R0 || R1) == 0 {
//  R2 = 0; gotoEND
// }

// 3 * 4
// for R1 != 0 {
//  R2 += R0
//  R1--
// }

// ps: not optimized on least multiplier

    @R2
    M=0

    @R0
    D=M
    @END
    D;JEQ

    @R1
    D=M
    @END
    D;JEQ

    @counter
    M=D

(LOOP)
    @R0
    D=M
    @R2
    M=M+D

    @counter
    M=M-1
    D=M
    @LOOP
    D;JGT

(END)
    @END
    0;JMP