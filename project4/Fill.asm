// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.
// while KBD != 0 {
//  screenBLACK script
// } else check KBD STATUS


(CHECK)
    @SCREEN
    D=A
    @pixels
    M=D
    
    @KBD
    D=M
    @WHITE
    D;JEQ

    @BLACK
    0;JMP

(WHITE)
    @pixels
    AD=M
    M=0
    @pixels
    M=M+1
    D=M

    @KBD // check for address overflow on KBD register
    D=A-D
    @WHITE
    D;JNE
    
    @CHECK
    0;JMP

(BLACK)
    @pixels
    AD=M
    M=-1
    @pixels
    M=M+1
    D=M

    @KBD // check for address overflow on KBD register
    D=A-D
    @BLACK
    D;JNE

    @CHECK
    0;JMP