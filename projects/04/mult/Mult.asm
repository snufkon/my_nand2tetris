// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)


        // i = R1
        @R1
        D=M
        @i
        M=D

        // sum = 0
        @sum
        M=0  
(LOOP)
        // if i == 0 then jump to END
        @i
        D=M
        @END
        D;JEQ

        // sum = sum + R0
        @R0
        D=M
        @sum
        M=M+D

        // i = i - 1
        @i
        M=M-1

        // jump to LOOP
        @LOOP
        0;JMP
        
(END)
        // R2 = sum
        @sum
        D=M
        @R2
        M=D

        // exit program
        @END
        0;JMP
