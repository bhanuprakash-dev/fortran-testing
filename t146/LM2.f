CLM2        CONTROL
C
      SUBROUTINE LM2 (CONV,EPSI,FEPSI,F0,LASTF,LASTNF,LASTNX,LASTX,
     &                N,NCYK,START,XDELTA,X0,INDEX,LASTG,K,G0,X,G3,K1)
C
      DIMENSION X0(1),LASTF(5),LASTX(5,1),XDELTA(1),INDEX(1),
     &          LASTG(5,1),G0(1),X(1),G3(1)
      INTEGER   CONV
      REAL      LASTF,LASTX,LASTG
      LOGICAL   START
C
      DATA NNX/3/,NNF/3/,N2/1/
C
C	
	print *,'LM2'
      write(*,*) "START=",START
      IF(.NOT.START)GOTO 20
      CONV=0
      LASTNF=0
      LASTNX=0
      DO 10 I=1,5
      LASTF(I)=1E20
      DO 10 J=1,N
      IND=INDEX(J)
      LASTX(I,IND)=0
   10 CONTINUE
C
      GOTO 110
C
   20 DO 30 I=1,N
      I2=2*I
      I21=I2-1
C      write(*,*) "XDELTA(7)",XDELTA(7)
      write(*,*) "LASTNX=",LASTNX
      write(*,*) "XDELTA(I21)=",XDELTA(I21)
      write(*,*) "XDELTA(I2)=",XDELTA(I2)
      write(*,*) "EPSI=",EPSI
      IF(ABS(XDELTA(I21)-XDELTA(I2)).LE.EPSI)GOTO 30
      LASTNX=0
      GOTO 60
   30 CONTINUE
C
      LASTNX=LASTNX+1
      write(*,*) "NNX=",NNX
      write(*,*) "LASTNX=",LASTNX
      IF(LASTNX.LT.NNX)GOTO 60
      CONV=1
      write(*,*) "LINE 41 CONV=",CONV
c      write(*,*) "NNX=",NNX
c      write(*,*) "LASTNX=",LASTNX
C
      GOTO 80
C
   60 IF(ABS(F0-LASTF(N2)).LE.FEPSI)GOTO 70
      LASTNF=0
      GOTO 80
C
   70 LASTNF=LASTNF+1
c	print *,LASTNF,NNF,'lastnfnnf'
      IF(LASTNF.LT.NNF)GOTO 80
      CONV=2
C
      
   80 DO 90 I=1,4
      J=6-I
      JJ=J-1
      LASTF(J)=LASTF(JJ)
      DO 90 NN=1,N
      IND=INDEX(NN)
      LASTX(J,IND)=LASTX(JJ,IND)
   90 CONTINUE
C
c	print *,CONV,'CONV'
      LASTF(1)=F0
C
      DO 100 NN=1,N
      IND=INDEX(NN)
      LASTX(1,IND)=X(IND)
  100 CONTINUE

      DO 120 I=1,4
      DO 120 NN=1,K1
  120 LASTG(6-I,NN)=LASTG(5-I,NN)
      DO 130 NN=1,K1
  130 LASTG(1,NN)=G3(NN)
C
  110 write(*,*) " LM2 CONV=",CONV
      RETURN
      END
