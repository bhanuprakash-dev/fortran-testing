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
*      Subroutine FRFUNK
*      -----------------
*
C...   Title:  Assigns free variables from the object-function
*
*      Written: 87-09-30 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE FRFUNK(XA,ACOND,BBCURD,BBDLI,BBFLU,BBHLI,
     &                   BLIMB,DCORE,HLIMB,NWILI)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL       XA(*),ACOND(9),BLIMB,DCORE,HLIMB
*
       INTEGER    IWDG,NWILI
*
       LOGICAL    BBHLI,BBDLI,BBFLU,BBCURD(9)
*
CC*SEBL End of the declarations block.
*
C... Adjust XA(1)=DCORE, XA(2)=HLIMB and XA(3)=BLIMB as required.
*
CC*SBBL Start of the adjustment block.
*
       IF (BBDLI) XA(1)=DCORE
       IF (BBHLI) XA(2)=HLIMB
       IF (BBFLU) XA(3)=BLIMB
*
CC*SEBL End of the adjustment block.
*
C... Adjust XA(3+IWDG)=ACOND for each winding
*
       DO 399 IWDG=1,NWILI
       IF (BBCURD(IWDG)) XA(3+IWDG)=ACOND(IWDG)
  399  CONTINUE
*
       RETURN
       END
