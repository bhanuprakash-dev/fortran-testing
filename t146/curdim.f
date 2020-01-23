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
*      Subroutine CURDIM
*      -----------------
*
C...   Title:  Calculation of the total relative No. of turns
C...           for each winding = ZWINDW( > 0.)
C...           Also POSIT is modified for main windings with
C...           extra tappings ( Type 4 )
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 86-10-03 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE CURDIM
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0801(1)    ,  FRACT  (1)    ),
     &  (XZ0821(1)    ,  FRMPOS (1)    ),
     &  (XZ0831(1)    ,  FRPPOS (1)    ),
     &  (XZ0841(1)    ,  FRZPOS (1)    ),
     &  (XZ0891(1)    ,  KCODE  (1)    )
       EQUIVALENCE
     &  (XZ1171(1)    ,  ZTALMW (1)    ),
     &  (XZ1181(1)    ,  ZTALRW (1)    ),
     &  (XZ1201(1)    ,  ZWINDW (1)    ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1656(1,1)  ,  POSIT  (1,1)  )
       EQUIVALENCE
     &  (XZ3004(1)    ,  FRXPOS (1)    )
*
       REAL      FRACT(9),FRZPOS(9),FRPPOS(9),FRMPOS(9),FRXPOS(6),
     &           POSIT(9,4),ZTALMW(9),ZTALRW(9),ZWINDW(9),X1,X2
*
       INTEGER   KCODE(9),IWDG,NWILI
*
CC*SEBL End of the declarations block.
*
C... Calculate No. of turns per winding
*
       DO 299 IWDG=1,NWILI
*
C... For each winding type
*
C,,, Main winding with no variation of turns
*
       IF (KCODE(IWDG).EQ.1) THEN
*
          ZWINDW(IWDG)=ABS(ZTALMW(IWDG)*FRACT(IWDG))
*
C... Regulating winding
*
       ELSE IF (KCODE(IWDG).EQ.2.OR.KCODE(IWDG).EQ.3) THEN
          ZWINDW(IWDG)=ABS(ZTALRW(IWDG))
*
C... Tapped windings
C... The next statements will convert "POSITION" to be valid for
C... the whole winding No. "I" instead of only for the tapping part
*
       ELSE IF (KCODE(IWDG).EQ.4) THEN
          X1=ZTALMW(IWDG)*FRACT(IWDG)
          X2=ZTALRW(IWDG)
          POSIT(IWDG,1)=(X1+FRZPOS(IWDG)*X2)/(X1+X2)
          POSIT(IWDG,2)=(X1+FRPPOS(IWDG)*X2)/(X1+X2)
          POSIT(IWDG,3)=(X1+FRMPOS(IWDG)*X2)/(X1+X2)
          POSIT(IWDG,4)=(X1+FRXPOS(IWDG)*X2)/(X1+X2)
          ZWINDW(IWDG)=ABS(X1+X2)
       END IF
 
  299  CONTINUE
*

       RETURN
       END
