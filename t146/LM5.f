CLM5        ORTONORMALISATION
C
      SUBROUTINE LM5 (N,P,NP,SIGMA,ALFA)
C
C     THIS PROCEDURE IS BASED ON THE ARTICLE BY POWEL IN
C     COMPUTER JOURNAL VOL.19 P.J02
C
      DIMENSION P(NP,1),SIGMA(1),ALFA(1)
      INTEGER   T,TT
C
C
      NN=N+1
C
      DO 20 I=1,NN
      ALFA(I)=0
C
      DO 10 J=1,N
      ALFA(I)=ALFA(I)+P(1,J)*P(I,J)
   10 CONTINUE
   20 CONTINUE
C
      ALFA(1)=SQRT(ALFA(1))
C
      NM=N+2
      DO 30 JT=1,N
      T=NM-JT
      IF(ABS(ALFA(T)).GT.(ALFA(1)*1E-7))GOTO 40
   30 CONTINUE
C     START
   40 S=0.0
C
      DO 50 I=1,N
      SIGMA(I)=0.0
   50 CONTINUE
C     HERE
   60 IF(T.LE.2)GOTO 90
      S=S+ALFA(T)**2
C
      DO 70 J=1,N
      SIGMA(J)=SIGMA(J)+ALFA(T)*P(T,J)
   70 CONTINUE
C
      TT=T-1
      DO 80 J=1,N
      P(T,J)=(S*P(TT,J)-ALFA(TT)*SIGMA(J))/SQRT(S*(S+ALFA(TT)**2))
   80 CONTINUE
C
      T=TT
      GOTO 60
C     READY
   90 DO 100 J=1,N
      P(2,J)=P(1,J)/ALFA(1)
  100 CONTINUE
      RETURN
      END
