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
*      Subroutine LLP1
*      ---------------
*
C...   Title:  Auxiliary optimisation routine
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE LLP1(M,N,P,E,NA,TD,RES,X,IND,IMIN,THMIN,
     &                 CHK,IND1,DELMAX,JMAX)
*
       DIMENSION E(NA,*),X(*),IND(*),IMIN(*),THMIN(*),
     & IND1(*),DELMAX(*),JMAX(*)
c       write(*,*) "E=",(E(I,J),I=1,20,J=1,20)
C... DIMENSION:
C... X(N+M),IND(LCOL),IMIN(LCOL),THMIN(LCOL),
C... CHK(M+1),IND1(M+1),DELMAX(M+1),JMAX(M+1)
*
       INTEGER CHK(*),P,RES,M,N,NA
*
       LOGICAL DUAL
*
C... DUAL is a LOGICAL which should be set to .TRUE.
C... if you need to know the values of the calculated
C... DUAL variables and .FALSE. otherwise
*
CC*SBBL
*
CC*SOFF
*
       print *,(IND(i),i=1,10)
       DUAL=.TRUE.
       LCOL=M+N-P+1
       write(*,*)"M=",M
       write(*,*)"N=",N
       write(*,*)"P=",P
       write(*,*)"LCOL LLP1",LCOL
       II=M+1
       KK=M-P+2
       LL=LCOL-1
       RES=0
       IF(KK.GT.II)GOTO 12
       DO 10 I=KK,II
       IF(E(I,LCOL).GE.0.0)GOTO 10
       DO  5 J=1,N
    5  E(I,J)=-E(I,J)
       E(I,LCOL)=-E(I,LCOL)
   10  CONTINUE
   12  CONTINUE
       DO 20 I=2,II
       CHK(I)=0
   20  CONTINUE
       write(*,*) "LLP1 CHK=",(CHK(i),i=1,9)
*
C... If there are any equality constraints in the problem,
C... the program first goes to PHASE1(OPT115),
C... otherwise it goes directly to RCS(30)
*
       write(*,*) "LLP6 call"
       IF (P.NE.0) CALL LLP6 (CHK,E,NA,GMIN,IM,IND,JMIN,K,L,
     &                        LCOL,M,N,P,RES,TD,IMIN,THMIN)
*
       IF(RES.GT.0)GOTO 120
*
C... RCS
*
   30  L=1
       K=1
       DO 40 J=1,LL
       IF(E(1,J).GE.(-TD))GOTO 40
       write(*,*)"E(1,J)",E(1,J)
       write(*,*)"J=",J
       IND(L)=J
       L=L+1
*
C... IND(L) keeps track of the columns in which E(1,J) is negative
*
   40  CONTINUE
*
       II=M+1
       DO 50 I=2,II
       IF(E(I,LCOL).GE.(-TD))GOTO 50
       IND1(K)=I
       K=K+1
       write(*,*) "I=",I
*      
C... IND1(L) keeps track of the rows in which E(1,LCOL) is negative
*
   50  CONTINUE
*
       IF(L.NE.1)GOTO 80
       IF(K.NE.1)GOTO 60
       write(*,*) "LLP2 call"
       CALL LLP2 (CHK,DUAL,E,NA,LCOL,M,N,RES,X)
       GOTO 120
*
   60  IF(K.NE.2)GOTO 130
*
       DO 70 J=1,LL
       II=IND1(1)
       IF(E(II,J).LT.0.0)GOTO 130
   70  CONTINUE
       RES=4
*
C... If RES=4 then the primal problem has no feasible solutions
C... DUAL objective function is unbounded
*
       GOTO 120
*
   80  IF(L.NE.2)GOTO 95
*
       IF(K.NE.1)GOTO 150
       II=M+1
       DO 90 I=2,II
       JJ=IND(1)
       IF(E(I,JJ).GT.0.0)GOTO 140
   90  CONTINUE
       RES=5
*
C... If RES=5 then the primal objective function is unbounded
C... DUAL problem has no feasible solutions
*
       GOTO 120
*
   95  IF(K.EQ.1)GOTO 140
       GOTO 150
*
C... R PROPHI ROWTRANS(IMAX,JM) GOTO RCS
*
  130  write(*,*) "LLP5 call"
       CALL LLP5 (E,NA,IMAX,IND1,JM,K,LCOL,PHIMAX,TD,DELMAX,JMAX)
       print *,"LLP3 from LLP1"
       CALL LLP3 (CHK,E,NA,LCOL,M,RES,IMAX,JM)
       IF(RES.GT.0)GOTO 120
       GOTO 30
*
C... C  PROGRAMMA ROWTRANS(IM,JMIN) GOTO RCS
*
  140  write(*,*) "LLP4 call"
       CALL LLP4 (E,NA,GMIN,IND,IM,JMIN,L,LCOL,M,TD,THMIN,IMIN)
       print *,"LLP3 from LLP1"
       CALL LLP3 (CHK,E,NA,LCOL,M,RES,IM,JMIN)
       IF(RES.GT.0)GOTO 120
       GOTO 30
*
C... S  PROGRAMMA PROPHI
*
  150  write(*,*) "LLP4 call"
       CALL LLP4 (E,NA,GMIN,IND,IM,JMIN,L,LCOL,M,TD,THMIN,IMIN)
       write(*,*) "LLP5 call"
       CALL LLP5 (E,NA,IMAX,IND1,JM,K,LCOL,PHIMAX,TD,DELMAX,JMAX)
       IF(GMIN.NE.1E6)GOTO 100
       print *,"LLP3 from LLP1"
       CALL LLP3 (CHK,E,NA,LCOL,M,RES,IMAX,JM)
       IF(RES.GT.0)GOTO 120
       GOTO 30
*
  100  write(*,*) "PHIMAX=",PHIMAX
       IF(PHIMAX.NE.(-1E6))GOTO 110
       print *,"LLP3 from LLP1"
       CALL LLP3 (CHK,E,NA,LCOL,M,RES,IM,JMIN)
       IF(RES.GT.0)GOTO 120
       GOTO 30
*
  110 IF(ABS(PHIMAX).GT.ABS(GMIN))GOTO 111
      print *,"LLP3 from LLP1"
      CALL LLP3 (CHK,E,NA,LCOL,M,RES,IM,JMIN)
      IF(RES.GT.0)GOTO 120
      GOTO 30
*
  111 print *,"LLP3 from LLP1"
      CALL LLP3 (CHK,E,NA,LCOL,M,RES,IMAX,JM)
      IF(RES.LE.0)GOTO 30
*
CC*SON
*
CC*SEBL
*
C...  Last
       
  
  120  write(*,*) "LLP1"
       write(*,*) "X",(X(i),i=1,10)
       write(*,*) "IND",(IND(i),i=1,10)
       write(*,*) "IMIN",(IMIN(i),i=1,10)
       write(*,*) "THMIN",(THMIN(i),i=1,10)
       write(*,*) "CHK",(CHK(i),i=1,10)
       write(*,*) "IND1",(IND1(i),i=1,10)
       write(*,*) "DELMAX",(DELMAX(i),i=1,10)
       write(*,*) "JMAX",(JMAX(i),i=1,10)
       RETURN
*
C... M      = The No. of constraints of which the last
C...          P are equality constraints
C... N      = The No. of variables
C... E      = A matrix with (M+1) rows and (M+N-P+1) columns
C... NA     = The size of first index with declaration of
C...          matrix E in the calling program
C... TD     = A small positive number which is used to test
C...          whether a number is non-negative (.GT.-TD)
C... X      = A vector such that :
C...          X(1,N) contains the optimal values of the variables and
C...          X(N+1,N+M) (only if DUAL is .TRUE.) contains the optimal
C...                  values of the DUAL variables corresponding to the
C...                  LQ-constraints
C... RES    = 1 If an optimum has been reached
C...          2 If there is no solution because the row index
C...            for the pivot element is zero
C...          3 If there is no solution because the column index
C...            for the pivot element is zero
C...          4 IF the primal problem has no feasible solutions,
C...            DUAL objective function is unbounded.
C...          5 If the primal objective function is unbounded
C...            DUAL problem has no feasible solutions.
*
       END
