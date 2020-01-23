*********************************************************************
*                                                                   *
*      The copyright to the computer program herein is  the         *
*      property of ABB TRANSFORMERS , Sweden.  The  program         *
*      may be used or copied only with the written  permis-         *
*      sion of ABB TRANSFORMERS  or in accordance with  the         *
*      terms of agreement under which the program has  been         *
*      supplied.                                                    *
*                                                                   *
*      In no event shall ABB TRANSFORMERS   be  liable  for         *
*      incidental or consequential damages arising from use         *
*      of this program.                                             *
*                                                                   *
*********************************************************************

************************************************************************
*
*      Function CHI
*      ------------
*
C...   Title:  Writing INTEGER data in a string.
*
*      Written: 91-11-12 by B-G Bladh      , SETFO/TS
*      Revised: ....................       , ........
************************************************************************
*
       FUNCTION CHI(I)
*
        CHARACTER  CUT*12, CHI*12, FORMAT*10
        INTEGER    I, N, M
        REAL       X
*
C...Make FORMAT ...
*
        X = REAL(ABS(I))
        X = MAX( X, 2.)
        X = LOG10(X)
        N = INT(X) + 1
       IF (I .LT. 0) N = N + 1
        M = 12 - N
        WRITE(FORMAT, 80) N, M
 80     FORMAT('(I',I1,',',I2,'X)')

        WRITE(CUT, FORMAT) I
*
        CHI = CUT
*
        RETURN
        END
