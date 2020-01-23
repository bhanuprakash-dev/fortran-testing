CLM9A        CALCULATION
C
      SUBROUTINE LM9A (X0,F,G,N,NF,X,FUNK,INDEX,XREL,GREL,G3,K1,
     &                 NVAR,IUT3,IRET1)
C
      DIMENSION X0(1),X(1),G(1),INDEX(1),XREL(1),GREL(1),G3(1)
C
      EXTERNAL FUNK
C
C
      IRET1=0
      DO 10 NN=1,N
      IND=INDEX(NN)
      X(IND)=X0(NN)*XREL(IND)
   10 CONTINUE
C
      NF=NF+1
C
      write(*,*) "X=",(X(I),I=1,6)
      write(*,*) "X0=",(X0(I),I=1,6)
      CALL FUNK(X,F,G3,*200)
      write(*,*) "F=",F
      write(*,*) "G3=",(G3(i),i=1,22)
      II=0
      DO 30 I=1,K1
      IF(GREL(I).LE.0.)GOTO 30
      II=II+1
      G(II)=G3(I)/GREL(I)
   30 CONTINUE
      DO 40 I=1,K1
      IF(GREL(I).GE.0.) GOTO 40
      II=II+1
      G(II)=G3(I)/ABS(GREL(I))
   40 CONTINUE
C.... UTSKRIFT VID VARJE FUNKTIONSANROP
      IF(IUT3.EQ.0)RETURN
      WRITE(6,120) NF,F
      WRITE(6,130) (X(I),I=1,NVAR)
      WRITE(6,140) (G3(I),I=1,K1)
  120 FORMAT(1X,'NF  =',I4,2X,'F=',1PE15.8)
  130 FORMAT(1X,'X   =',6X,1P,4E14.5)
  140 FORMAT(1X,'G   =',6X,1P,4E14.5)
      RETURN
  200 IRET1=1
C.... GER HOPP TILL LM1 SATSNR. 145
      RETURN
      END
