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
*      Subroutine LLP2
*      ---------------
*
C...   Title:  Results
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE LLP2(CHK,DUAL,E,NA,LCOL,M,N,RES,X)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       DIMENSION X(*),E(NA,*)
*
       INTEGER RES,CHK(*)
*
       LOGICAL DUAL
*
CC*SEBL End of the declarations block.
*
C... Initialise X(*)
*
       DO 10 I=1,N
   10  X(I)=0
*
       JJ=N+1
       JK=N+M
*
C... Set X(*) to 0
*
C,,, If DUAL
*
       print *,'llp2',X(JJ)
       IF(DUAL) THEN
*
C... Set X(*) to 0
*
          DO 15 I=JJ,JK
   15     X(I)=0.0
       END IF
       JJ=M+1
*
C... Set X(*)
*
       DO 20 I=2,JJ
       II=CHK(I)
       IF(CHK(I).GT.N) CHK(I)=0
       IF(CHK(I).GT.0) X(II)=E(I,LCOL)
   20  CONTINUE
*
C... Set X(*)
*
C,,, If DUAL
*
       IF(DUAL) THEN
          JJ=LCOL-1
          II=N+1
*
C... Set X(*)
*
          DO 40 I=II,JJ
   40     X(I)=E(1,I)
       END IF
*
C... If RES=1 the optimum has been found
*
CC*SBBL Start of the RES1 block
*
       RES=1
*
CC*SEBL End of the RES1 block
*      
       write(*,*) "CHK",(CHK(i),i=1,9)
       write(*,*) "XDELTA LLP2",(X(i),i=1,9)
       write(*,*) "LCOL",LCOL
       write(*,*) "DUAL",DUAL
       RETURN
       END
