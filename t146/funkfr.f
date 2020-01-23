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
*      Subroutine FUNKFR
*      -----------------
*
C...   Title:  Assigns free variables for the object-function
*
*      Written: 85-11-14 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE FUNKFR(XA,ACOND,BBCURD,BBDLI,BBFLU,BBHLI,
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
C... Adjust DCORE, HLIMB and BLIMB as required.
*
CC*SBBL Start of the adjustment block.
*
       print*,'inside funkfr'
       IF (BBDLI) DCORE=ABS(XA(1))
       IF (BBHLI) HLIMB=ABS(XA(2))
       write(*,*) 'file funkfr line 50', HLIMB
       IF (BBFLU) BLIMB=ABS(XA(3))
*
CC*SEBL End of the adjustment block.
*
C... BLTOPM = Maximum allowed flux density for HI-B & over-magnetisation
C... With this function  BLIMB < BLTOPM.
C...
C... Allocate values to ACOND for each winding
*
       DO 399 IWDG=1,NWILI
       IF (BBCURD(IWDG)) ACOND(IWDG)=ABS(XA(3+IWDG))
  399  CONTINUE
*
       print*,'coming out of funkfr'
       RETURN
       END
