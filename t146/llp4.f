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
*      Subroutine LLP4
*      ---------------
*
C...   Title: Performs calculations over columns
C...          to determine the pivot element
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE LLP4(E,NA,GMIN,IND,IM,JMIN,L,LCOL,M,TD,THMIN,IMIN)
*
C... Local vectors:  THMIN(LCOL),IMIN(LCOL)
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       DIMENSION E(NA,*),THMIN(*),IMIN(*),IND(*)
*
CC*SEBL End of the declarations block.
*
C... GMIN is set equal to a large number for initialisation purposes
*
CC*SBBL Start of the GMIN block.
*
       GMIN=1E6
       JMIN=0
       JJ=M+1
       LL=L-1
*
CC*SEBL End of the GMIN block.
*
C... Calculate
*
       DO 20 L1=1,LL
       II=IND(L1)
       IMIN(II)=0
       THMIN(II)=1E6
*
C... Calculate
*
       DO 10 I=2,JJ
*
C... Calculate
*
C,,, If E(I,II) >= TD
*
       IF(E(I,II).GE.TD) THEN
*
C... Calculate
*
C,,, If E(I,LCOL) >= -TD
*
          IF(E(I,LCOL).GE.(-TD)) THEN
             THETA=E(I,LCOL)/E(I,II)
*
C... Calculate
*
C,,, If THETA < THMIN(II)
*
             IF(THETA.LT.THMIN(II)) THEN
                THMIN(II)=THETA
                IMIN(II)=I
             END IF
          END IF
       END IF
   10  CONTINUE
*
       GAMMA=THMIN(II)*E(1,II)
       IF(THMIN(II).EQ.1E6)GAMMA=1E8
*
C... Calculate
*
C,,, If GAMMA < GMIN
*
       IF(GAMMA.LT.GMIN) THEN
          GMIN=GAMMA
          JMIN=IND(L1)
       END IF
   20  CONTINUE
*
       IF(JMIN.GT.0)IM=IMIN(JMIN)
*
       RETURN
       END
