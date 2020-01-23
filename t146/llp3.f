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
*      Subroutine LLP3
*      ---------------
*
C...   Title:  Performs the usual tableau transformations
C...           in a linear programming problem,
C...           (IM,JMIN) being the pivotal element
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE LLP3(CHK,E,NA,LCOL,M,RES,IM,JMIN)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       DIMENSION E(NA,*)
*
       INTEGER RES,CHK(*)
*
CC*SEBL End of the declarations block.
*
C... If RES=2 there is no solution
C... because the row index for the pivot element is zero
*
C,,, IM = 0
*
       write(*,*) "LLP3 CHK=",(CHK(i),i=1,9)
       IF(IM.EQ.0) THEN
          RES=2
*
C... If RES=3 there is no solution
C... because the column index for the pivot element is zero
*
C... JMIN = 0
*
   10  ELSE IF (JMIN.EQ.0) THEN
          RES=3
*
C... Neither
*
       ELSE
   20     DUMMY=E(IM,JMIN)
*
C... Set E(*)
*
          DO 30 J=1,LCOL
   30     E(IM,J)=E(IM,J)/DUMMY
*
          II=M+1
*
C... Set E(*)
*
          DO 40 I=1,II
*
C... Set E(*)
*
C,,, If I <> IM
*
          IF(I.NE.IM) THEN
*
C... Set E(*)
*
C,,, If E(I,JMIN) <> 0
*
             IF(E(I,JMIN).NE.0.0) THEN
                DUMMY=E(I,JMIN)
*
C... Set E(*)
*
                DO 35 J=1,LCOL
   35           E(I,J)=E(I,J)-E(IM,J)*DUMMY
             END IF
          END IF
   40     CONTINUE
*
          CHK(IM)=JMIN
*
       END IF
*
   50  write(*,*) "LLP3 CHK=",(CHK(i),i=1,9)
       RETURN
       END
