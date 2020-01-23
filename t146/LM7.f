CLM7        PREPARATION
C
      SUBROUTINE LM7 (AM,E,NE,F,G,NG,G0,K,LCOLE,ME,N,P,NP,PE,
     &                AMAX,AMMAX,EPSI,RES)
C
      DIMENSION AM(1),E(NE,1),F(1),G(NG,1),G0(1),P(NP,1),AMAX(1)
      INTEGER   PE,RES
C
C
      write(*,*) "LM7 START"
      JJ=2*N+1
      DO 10 J=JJ,LCOLE
      E(1,J)=0.0
   10 CONTINUE
C
      ME1=ME+1
      LC1=LCOLE-1
      DO 30 I=2,ME1
      DO 20 J=JJ,LC1
      E(I,J)=0.0
   20 CONTINUE
   30 CONTINUE
C
      ME2=ME1-PE
      DO 40 I=2,ME2
      IN=2*N-1+I
      E(I,IN)=1.0
   40 CONTINUE
C
      DO 50 J=1,N
      J2=2*J
      J21=J2-1
      write(*,*) "F(J)",F(J)
      E(1,J21)=F(J)
      E(1,J2)=-F(J)
   50 CONTINUE
C
      DO 70 I=1,N
      I1=I+1
      I2=2*I
      I21=I2+1
      DO 60 J=1,N
      J2=2*J
      J21=J2-1
      write(*,*) "P(I1,J)",P(I1,J)
      E(I2,J21)=P(I1,J)
      E(I21,J2)=P(I1,J)
      E(I2,J2)=-P(I1,J)
      E(I21,J21)=-P(I1,J)
   60 CONTINUE
   70 CONTINUE

      IF(RES.NE.1)GOTO 78
      DO 75 NN=1,N
      write(*,*) "AMAX(NN)",AMAX(NN)
      write(*,*) "AM(NN)",AM(NN)
      write(*,*) "EPSI",EPSI
      AMAX(NN)=AMAX1(AMAX(NN),AM(NN)*0.5,EPSI*4.0)
      SLMX=1.E8
      I1=NN+1
      DO 73 J=1,N
   73 SLMX=AMIN1(SLMX,AMAX(NN)/(ABS(P(I1,J)+1.E-10)))
   75 AM(NN)=AMIN1(AM(NN),SLMX)
   78 CONTINUE
C
      DO 80 I=1,N
      I2=2*I
      I21=I2+1
      E(I2,LCOLE)=AM(I)
      E(I21,LCOLE)=AM(I)
   80 CONTINUE
C
      DO 100 I=1,K
      N21=2*N+1+I
      DO 90 J=1,N
      J2=2*J
      J21=J2-1
      E(N21,J21)=G(I,J)
      E(N21,J2)=-G(I,J)
   90 CONTINUE
      E(N21,LCOLE)=-G0(I)
  100 CONTINUE
      RETURN
      END
