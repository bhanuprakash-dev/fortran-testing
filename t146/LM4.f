CLM4        NEWLIMITS
C
      SUBROUTINE LM4 (ALFA,AM,BETA,B,GAMMA,LIMIT,N,P,NP,SCALAR,SIGNAL,
     &                STEPS,START,XF,X0,XDELTA,MFIRST,STPLIM,ILP,NOR,
     &                SIGMA,SIGLIM,ALFA1)
C
      DIMENSION AM(1),LIMIT(1),P(NP,1),SIGNAL(NP,2),
     &          XF(1),X0(1),XDELTA(1)
      INTEGER   SIGNAL,STEPS,STPLIM,STEP2
      REAL      MFIRST
      LOGICAL   START,LIMIT
C
C
      STEP2=0

      IF(.NOT.START)GOTO 30
      START=.FALSE.
C
      DO 20 NN=1,N
      NN1=NN+1
      LIMIT(NN)=.TRUE.
      SIGNAL(NN,1)=1
      SIGNAL(NN,2)=1
C
      DO 10 I=1,N
      P(NN1,I)=0.0
   10 CONTINUE
C
      NN2=NN-1
      IF(NN.EQ.1)P(2,1)=1
      IF(NN.NE.1)P(NN1,NN)=(-1)**(NN2)
      AM(NN)=MFIRST
      XF(NN)=X0(NN)
   20 CONTINUE
C
      B=1.0
      STEPS=1
      STEP2=0
      GOTO 150
C
C
   30 IF(STEPS.LT.STPLIM)GOTO 70
      IF(STEP2.GE.NOR)GOTO 70
      STEP2=STEP2+1
      STEPS=1
      SCALAR=0.0
      DO 40 NN=1,N
      SCALAR=SCALAR+ (X0(NN)-XF(NN))**2
   40 CONTINUE
C
      IF(SCALAR.GE.(GAMMA**2))GOTO 50
      B=B+1.0
      GOTO 80
C
   50 DO 60 NN=1,N
      P(1,NN)=X0(NN)-XF(NN)
      XF(NN)=X0(NN)
      LIMIT(NN)=.TRUE.
      SIGNAL(NN,1)=1
      SIGNAL(NN,2)=1
   60 CONTINUE
C
      AM(1)=SQRT(SCALAR)*BETA/B
      B=1.0
C
C
C.... ORTONORMALATION
      CALL LM5 (N,P,NP,SIGMA,ALFA1)
      GOTO 150
C
C
   70 STEPS=STEPS+1
C
   80 DO 140 I=1,N
      SCALAR=0.0
      I1=I+1
      DO 120 NN=1,N
      NN2=NN*2
      NN21=NN2-1
      SCALAR=SCALAR+P(I1,NN)*(XDELTA(NN21)-XDELTA(NN2))
  120 CONTINUE
C
      IF(STEPS.LE.2.AND.B.LE.1.1.OR.ILP.EQ.0) GOTO 130
      IF(SCALAR.LE.0.0)GOTO 90
      IF(.NOT.LIMIT(I))GOTO 90
C
C
C.... AGAIN
      CALL LM6 (N,ALFA,AM,SCALAR,SIGNAL,NP,SIGLIM,1,I)
      SIGNAL(I,2)=1
      GOTO 130
C
   90 IF(LIMIT(I))GOTO 110
      IF(SCALAR.GE.0.0)GOTO 110
C
C.... AGAIN
  100 CALL LM6 (N,ALFA,AM,SCALAR,SIGNAL,NP,SIGLIM,2,I)
      SIGNAL(I,1)=1
      GOTO 130
  110 AM(I)=AM(I)/ALFA
      SIGNAL(I,1)=1
      SIGNAL(I,2)=1
C
  130 LIMIT(I)=.TRUE.
      IF(SCALAR.LE.0.0)LIMIT(I)=.FALSE.
  140 CONTINUE
C
  150 DO 1111 i=1,6
      write(*,*) "P=",(P(i,j),j=1,7)
 1111 CONTINUE
      RETURN
      END
