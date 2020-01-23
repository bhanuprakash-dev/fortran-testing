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
*      Subroutine LIMCHK
*      -----------------
*
C...   Title:  Check whether 99% and/or 100% limits exceeded.
C...           Called from CONSTR.
C...           Critical parameters are saved in SAVE1.
C...           NCONST stores the No. of critical parameters.
C...           Printout of SAVE1 via PAGE01
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE LIMCHK(VALUE,TEXT1,TEXT2,NCONST,SAVE1)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL      VALUE
*
       INTEGER   NCONST
*
       DIMENSION SAVE1(50)
       CHARACTER SAVE1*7,TEXT1*7,TEXT2*7
*
CC*SEBL End of the declarations block.
*
C... Check which limit has been exceeded.
*
C,,, > 1.001
*
       IF (VALUE.GT.1.001.AND.TEXT2.NE.'       ') THEN
          NCONST=NCONST+1
          SAVE1(NCONST)=TEXT2
*
C... > 0.990
*
       ELSE IF (VALUE.GT.0.990.AND.TEXT1.NE.'       ') THEN
          NCONST=NCONST+1
          SAVE1(NCONST)=TEXT1
*
C... < 0.000
*
       ELSE IF (VALUE.LT.0..AND.TEXT1.NE.'       ') THEN
*
C... Total up
*
C,,, > 0.001
*
          IF(ABS(VALUE).GT.1.E-3) THEN
             NCONST=NCONST+1
             SAVE1(NCONST)=TEXT1
          END IF
       END IF
       RETURN
       END
