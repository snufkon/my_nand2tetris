// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.


        // set 0000000000000000 to white
        @0
        D=A
        @white
        M=D

        // set 1111111111111111 to black
        @white
        D=!M
        @black
        M=D        

        // set end address
        @8192
        D=A
        @SCREEN
        D=D+A
        @end_address
        M=D
        
(LOOP)
        // set init address
        @SCREEN
        D=A
        @address
        M=D

        // D = keycode
        @KBD
        D=M

        // if keyboard is being pressed then jump to PAINT_BLACK
        //@BLACK
        @PAINT_BLACK
        D;JNE

        // else jump to PAINT_WHITE
        @PAINT_WHITE
        0;JMP


(PAINT_BLACK)
        // set color to black
        @black
        D=M
        @color
        M=D

        @PAINT
        0;JMP


(PAINT_WHITE)
        // set color to white
        @white
        D=M
        @color
        M=D

        @PAINT
        0;JMP


(PAINT)
        @end_address
        D=M

        @address
        D=D-M

        @LOOP
        D;JEQ

        // paint
        @color
        D=M
        @address
        A=M
        M=D

        @address
        M=M+1

        @PAINT
        0;JMP