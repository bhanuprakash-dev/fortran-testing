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
*      Function CHR
*      ------------
*
C...   Title:  Writing float data in a string.
*
*      Written: 91-11-12 by B-G Bladh      , SETFO/TS
*      Revised: ....................       , ........
************************************************************************
*
       FUNCTION CHR(PAR)
*
        CHARACTER  CUT*12, CHR*12, FORMAT*10
        REAL       X,  PAR, APAR
        INTEGER    NHEL, NDEC, NTOT, M, DEC
*
C... Make FORMAT ...Truncate to 3 decimals
*
        APAR= ABS(PAR)
        DEC = NINT( (1.E-5 + APAR - REAL(INT(APAR))) * 1000.)
*
C... Chose number of decimals
*
        NDEC = 3
        IF ( (DEC - (DEC/10)*10).EQ. 0) THEN
           NDEC = 2
           IF ( (DEC - (DEC/100)*100).EQ.0) THEN
              NDEC = 1
                 IF( DEC .EQ. 0 ) THEN
                 NDEC = 0
              ENDIF
           ENDIF
        ENDIF
*
        NHEL = INT (LOG 10 (MAX( APAR, 2.))) + 1
        IF ((PAR+0.0005) .LT. 0.) NHEL = NHEL + 1
*
C... Limit number of decimals, if space is insufficient
*
        NDEC = MIN(NDEC, (11-NHEL) )
        NTOT = NHEL + 1 + NDEC
        NTOT = MAX(NTOT,3)
        M = 12 - NTOT
*
        WRITE(FORMAT, 88) NTOT, NDEC, M
 88     FORMAT('(F',I2,'.',I1,',',I1,'X)')
        WRITE(CUT, FORMAT ) PAR
*
        CHR = CUT
*
        RETURN
        END
