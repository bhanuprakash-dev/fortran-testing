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
*      Subroutine LLP6
*      ---------------
*
C...   Title:  Applied only to equality constraints, if any
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE LLP6(CHK,E,NA,GMIN,IM,IND,JMIN,K,L,LCOL,M,N,
     &                 P,RES,TD,IMIN,THMIN)
*
       DIMENSION E(NA,*),IND(*),IMIN(*),THMIN(*)
*
       INTEGER P,R,RES,CHK(*),FIRST
*
C... Local vectors:
C... IMIN(LCOL),THMIN(LCOL)
*
CC*SBBL
*
CC*SOFF
*
       DO 50 R=1,P
*
C... GMIN is set equal to a large number for initialisation purposes
*
       GMIN=1E6
       print *,GMIN,'GMIN'
       L=1
       JMIN=0
       FIRST=1
       DO 10 J=1,N
       THMIN(J)=1E6
       IF(E(1,J).GE.(-TD))GOTO 10
       IND(L)=J
       L=L+1
   10  CONTINUE
*
C... L1
*
   12  IF(L.NE.1)GOTO 20
*
       DO 15 J=1,N
   15  IND(J)=J
       L=N+1
   20  LJ=L-1
       DO 40 K=1,LJ
       II=IND(K)
       LL=M-P+2
       JJ=M+1
       DO 30 I=LL,JJ
       IF(CHK(I).NE.0)GOTO 30
       IF(E(I,II).LE.TD)GOTO 30
       THETA=E(I,LCOL)/E(I,II)
       IF(THETA.GE.THMIN(II))GOTO 30
       THMIN(II)=THETA
       IMIN(II)=I
   30  CONTINUE
*
       GAMMA=THMIN(II)*E(1,II)
       IF(THMIN(II).EQ.1E6)GAMMA=1E8
       IF(GAMMA.GE.GMIN)GOTO 40
       GMIN=GAMMA
       JMIN=IND(K)
   40  CONTINUE
*
       IF(JMIN.NE.0)GOTO 45
       IF(FIRST.NE.1)GOTO 42
       FIRST=0
       L=1
       GOTO 12
   42  IM=0
       GOTO 50
*
   45  IM=IMIN(JMIN)
       print *,"LLP3 from LLP6"
       CALL LLP3 (CHK,E,NA,LCOL,M,RES,IM,JMIN)
       IF(RES.GT.0)GOTO 60
   50  CONTINUE
*
CC*SON
*
CC*SEBL
*
   60  RETURN
       END
