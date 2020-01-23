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
*      Subroutine DIMSCC
*      -----------------
*
C...   Title:  Dimensioning short-circuit cases
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 83-04-27 by P.L-V.     , KYT
*      Revised: 86-04-03 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE DIMSCC(BBVR,NG,SLINE,BBSCI,BBSCJ,BBSCK,BBSCL)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL      SLINE(4)
*
       INTEGER   MAX,NG
*
       LOGICAL   BBVR(4),BBSCI(4),BBSCJ(4),BBSCK(4),BBSCL(4),BB1,BB2
*
CC*SEBL End of the declarations block.
*
C... Set BBSCI,BBSCJ,BBSCK,BBSCL
*
CC*SBBL Start of the Setting block.
*
       BBSCI(1)=.TRUE.
       BBSCI(2)=BBVR(1)
       BBSCI(3)=BBVR(1)
       BBSCI(4)=BBVR(1)
*
       BBSCJ(1)=.TRUE.
       BBSCJ(2)=BBVR(2)
       BBSCJ(3)=BBVR(2)
       BBSCJ(4)=BBVR(2)
*
       BBSCK(1)=.TRUE.
       BBSCK(2)=BBVR(3)
       BBSCK(3)=BBVR(3)
       BBSCK(4)=BBVR(3)
*
       BBSCL(1)=.TRUE.
       BBSCL(2)=.TRUE.
       BBSCL(3)=.TRUE.
       BBSCL(4)=.FALSE.
*
CC*SEBL End of the Setting block.
*
C... Set BBSCL(MAX)
*
C,,, If NG<=2
*
       IF(NG.LE.2) THEN
          BBSCL(3)=.FALSE.
          MAX=1
          BB1=.FALSE.
          IF (SLINE(1).GE.0.) BB1=.TRUE.
          BB2=.FALSE.
          IF (SLINE(2).GE.0.) BB2=.TRUE.
          IF(BB1.AND..NOT.BB2) MAX=2
          IF(BB1.AND.BB2.AND.SLINE(1).LT.SLINE(2)) MAX=2
          BBSCL(MAX)=.FALSE.
       END IF
*
       RETURN
       END
