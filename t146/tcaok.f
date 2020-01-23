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
*      Subroutine TCAOK
*      -----------------
*
*  0.  Written: XX-XX-XX by S-E Jansson      , ZKB
*      Revised: 86-12-09 by Jan Akesson      , Z1KDC
*      Revised: 91-11-27 by Ron Bell         , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the Yoke-Section for the TCA Core
*
************************************************************************
*
       SUBROUTINE TCAOK(TYP,N,OLAPP,BSTEP,DCOR,TWSUPC,AF,B1,TP,FINDR,
     &                  B3,RK1,BBPRT1,BBPRT2)
*
       COMMON /CTCAUT/ HB,CB
*
       REAL      TWSUPC,RLNU,BSTEP,A,AF,ANL,AVST,CB,DCOR,FINDR,
     &           HB,OKM,OLAPP,RL,TYP,Y,Y1
*
       REAL      B1(0:N),TP(0:N),TS(100),B3(N),RK1(N)
*
       INTEGER   I,I1,I2,IX,J,M,N,N1,N2,NL
*
       LOGICAL   BBPRT1,BBPRT2

       include'consts.h'
*
C... Preliminary layout
       WRITE(*,*) 'TCAOK.F N = ', N
*       
 100   CONTINUE
*      
       WRITE(*,*) 'TCAOK.F B1 = ', B1
       DO 299 I=1,N
          B3(I)=B1(I)+OLAPP
          WRITE(*,*) '----------------'
          WRITE(*,*) 'TCAOK.F B3(I) = ', B3(I)
          WRITE(*,*) 'TCAOK.F B1(I) = ', B1(I)
          WRITE(*,*) 'TCAOK.F OLAPP = ', OLAPP
          WRITE(*,*) '----------------'
 299   CONTINUE
*
       BBPRT1=.FALSE.
       BBPRT2=.FALSE.
       TS(1)=TP(1)
*
       DO 399 I=2,N
          TS(I)=TS(I-1)+TP(I)
 399   CONTINUE
*
       DO 499 I=1,N
          RK1(I)=0.
 499   CONTINUE
*
       IF (DCOR.GT.780.) GOTO 502
          RLNU=35.
          GOTO 599
 502   IF (DCOR.GT.900.) GOTO 503
          RLNU=45.
          GOTO 599
 503   IF (DCOR.GT.1200.) GOTO 504
          RLNU=55.
          GOTO 599
 504      RLNU=65.
 599   CONTINUE
*
       I=1
*
 600   IF (I.GT.N) GOTO 699
          I1=I
          RK1(I)=TWSUPC+58.+8.
          IF (TS(I).GE.RLNU) GOTO 702
             I=I+1
             GOTO 799
 702         I=N+1
 799      CONTINUE
          GOTO 600
 699   CONTINUE
*
       ANL=FINDR*TWSUPC
       I=I1+1
*
 800   IF (I.GT.N-1) GOTO 899
          I2=I
          RK1(I)=TWSUPC+8.
          IF (TS(I)-TS(I1).GE.ANL) GOTO 902
             I=I+1
             GOTO 999
 902         I=N
 999      CONTINUE
          GOTO 800
 899   CONTINUE
*
       A=(29.+TWSUPC+8.)/TAN20-100.
       J=0
*
 1000  IF (A.LT.TS(I2)+18..OR.J.EQ.1) GOTO 1099
          IF (I2.EQ.N-1) GOTO 1102
             I2=I2+1
             RK1(I2)=TWSUPC+8.
             GOTO 1199
 1102        BBPRT2=.TRUE.
             J=1
 1199     CONTINUE
          GOTO 1000
 1099  CONTINUE
*
       N1=N-1
       I=1
*
 1200  IF (I.GT.N1) GOTO 1299
          IF (RK1(I+1)+B3(I+1).GE.RK1(I)+B3(I)) GOTO 3602
             B3(I+1)=B3(I+1)+BSTEP
             GOTO 3699
 3602     I=I+1
 3699     CONTINUE
          GOTO 1200
 1299  CONTINUE
*
       IF (TYP.GT.3..OR.DCOR.LT.680.) GOTO 1302
*
       OKM=B3(N)-(AINT(0.08*DCOR/BSTEP)*BSTEP)
       I=1
 1400  IF (I.GT.N) GOTO 1499
       IF (RK1(I)+B3(I).LE.OKM) GOTO 1502
       IX=I
       I=N+1
       GOTO 1599
 1502  I=I+1
 1599  CONTINUE
       GOTO 1400
 1499  CONTINUE
*
       DO 1699 I=IX,N
          B3(I)= AINT((OKM-RK1(I))/BSTEP)*BSTEP
 1699  CONTINUE
       GOTO 1399
 1302  CONTINUE
 1399  CONTINUE
*
       IF (DCOR.GT.800.) GOTO 1702
          NL=3
       GOTO 1799
 1702  IF (DCOR.GT.900.) GOTO 1703
          NL=4
       GOTO 1799
 1703     NL=5
 1799  CONTINUE
*
       IF (TYP.LE.3.) GOTO 1802
 1900  IF (DCOR.GT.800.) GOTO 1902
          NL=2
          GOTO 1999
 1902  IF (DCOR.GT.1200.) GOTO 1903
          NL= INT((DCOR-0.001)*0.01)-5
          GOTO 1999
 1903     NL=7
 1999  CONTINUE
       GOTO 1899
 1802  CONTINUE
 1899  CONTINUE
*
       DO 2099 I=1,NL-1
          B3(I)= AINT((B3(NL)+RK1(NL)-RK1(I))/BSTEP)*BSTEP
 2099  CONTINUE
*
C... B)
*
       Y1=35.
       M=1
 2100  IF (M.GT.2) GOTO 2199
*
       AVST=35./COS20
       I=2
 2200  IF (I.GT.N.OR.TS(I).GT.50.) GOTO 2299
       RL=RK1(1)+B3(1)+Y1/COS20
       Y=TAN20*TS(I-1)+RL
       IF (RK1(I)+B3(I).LE.Y-AVST) GOTO 2302
       Y1=Y1+1.
       GOTO 2399
 2302  N1=I
       I=I+1
 2399  CONTINUE
       GOTO 2200
 2299  CONTINUE
*
       AVST=20./COS20
       I=1
 2400  IF (I.GT.N) GOTO 2499
       IF (RK1(I)+B3(I).LT.B3(N)) GOTO 2502
       N2=I
       I=N+1
       GOTO 2599
 2502  I=I+1
 2599  CONTINUE
       GOTO 2400
 2499  CONTINUE
       I=N1
*
 2600  IF (I.GT.N2) GOTO 2699
       RL=RK1(1)+B3(1)+Y1/COS20
       Y=TAN20*TS(I-1)+RL
       IF (RK1(I)+B3(I).LE.Y-AVST) GOTO 2702
       Y1=Y1+1
       GOTO 2799
 2702  I=I+1
 2799  CONTINUE
       GOTO 2600
 2699  CONTINUE
*
       HB=B3(N)+15.-RL
       CB=HB/TAN20
*
       IF (M.EQ.1)GOTO 2802
       M=M+1
       GOTO 2899
*
C... C)
*
 2802  I=2
 2900  IF (I.GT.N2) GOTO 2999
       IF (TS(I-1).LE.CB+20.) GOTO 3002
       B3(I)= AINT((B3(N)+RK1(N)-RK1(I))/BSTEP)*BSTEP
       I=I+1
       GOTO 3099
 3002  I=I+1
 3099  CONTINUE
       GOTO 2900
 2999  CONTINUE
*
C... D)
*
       J=1
 3100  IF (J.GT.N2) GOTO 3199
       I=N2-J+1
       IF (I.LE.NL+1) GOTO 3202
 3300  IF (B3(I)+RK1(I)-B3(I-1)-RK1(I-1).LE.16.) GOTO 3399
       B3(I-1)=B3(I-1)+BSTEP
       GOTO 3300
 3399  CONTINUE
       J=J+1
       GOTO 3299
 3202  J=N2+1
 3299  CONTINUE
       GOTO 3100
 3199  CONTINUE
*
       M=M+1
 2899  CONTINUE
       GOTO 2100
 2199  CONTINUE
*
       IF (AF.NE.0.) THEN
       DO 3599 I=1,N
       B3(I)=B3(I)+AF
 3599  CONTINUE
       END IF
*
 199   RETURN
       END
