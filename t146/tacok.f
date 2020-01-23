*********************************************************************
*                                                                   *
*      The copyright to the computer program herein is  the         *
*      property of ABB  TRANSFORMERS, Sweden.  The  program         *
*      may be used or copied only with the written  permis-         *
*      sion of ABB  TRANSFORMERS or in accordance with  the         *
*      terms of agreement under which the program has  been         *
*      supplied.                                                    *
*                                                                   *
*      In no event shall ABB  TRANSFORMERS  be  liable  for         *
*      incidental or consequential damages arising from use         *
*      of this program.                                             *
*                                                                   *
*********************************************************************

************************************************************************
*
*      Subroutine TACOK
*      -----------------
*
*  0.  Written: 82-12-27 by S. Larson      , XXXX
*      Revised: 86-12-09 by J. Akesson     , Z1KDC
*      Revised: 91-11-27 by Ron Bell       , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the yoke section
C...               for the TAC Core
*
************************************************************************
*
       SUBROUTINE TACOK(IN,DCOR,BSTEP,N,X1,Y1,XG,UT,OK)
*
       REAL      BSTEP,DCOR,TSTEG,UT
*
       REAL      OK(N),X1(0:N),Y1(0:N),XG(N)
*
       INTEGER   I,I1,IN,IX1,M,N,N1
*
C... TACOK
*
C... Dimensioning of the yoke section
*
C... First width
*
       IF (DCOR.GE.300.) THEN
          OK(1)=X1(2)+40.
       ELSE
          OK(1)=X1(2)+20.
       END IF
*
C... Second width to N-1 width
*
       IF (DCOR.GE.300.) THEN
          DO 240 I=2,N-1
             OK(I)=X1(I)+40.
  240     CONTINUE
       ELSE
          DO 260 I=2,N-1
             OK(I)=X1(I)+20.
  260     CONTINUE
       END IF
*
C... Nth width
*
       I=N
       OK(I)=X1(I)+20.
*
C... Stepping against yoke
*
       DO 310 I=1,N
          XG(I)=0.
  310  CONTINUE
*
C... First Step
*
       M=N
       IX1=1
       CALL AVTRAL(M,IX1,XG,Y1,70.,14.)
*
C... Second Step
*
       IX1=1
       CALL AVTRAL(M,IX1,XG,Y1,40.,70.)
*
C... Outer Stepping
*
C... Cut off packets which are sticking up. Max 2.*BSTEP may be cut off.
C... Begin with Packet N.
*
       N1=N-1
       DO 499 I=1,N1
          I1=N+1-I
          TSTEG=0.
  410     IF (XG(I1)+OK(I1).GE.XG(I1-1)+OK(I1-1)) GOTO 499
             TSTEG=TSTEG+BSTEP
             IF (TSTEG.GT.(2.*BSTEP)) GOTO 499
                OK(I1-1)=OK(I1-1)-BSTEP
             GOTO 410
  499  CONTINUE
*
C... Filling up remaining notches
*
       N1=N-1
       DO 599 I=1,N1
  510     IF (XG(I+1)+OK(I+1).LT.XG(I)+OK(I)) THEN
             OK(I+1)=OK(I+1)+BSTEP
             GOTO 510
          END IF
  599  CONTINUE
*
       RETURN
       END
