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
*      Subroutine CORE2
*      ----------------
*
*  0.  Written: XX-XX-XX by S-E Jansson       , XXXX
*      Revised: 86-12-09 by J. Akesson        , Z1KDC
*      Revised: 91-11-27 by Ron Bell          , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the Core Section
*                   for core types EY,DY and TY
*
************************************************************************
*
       SUBROUTINE CORE2(COM,SLZ,X1Z,B2Z,OKZ,XGZ,RK2Z,T1Z,YZ)
*
       COMMON /TVING/ T2Z
       REAL           T2Z(100)
*
       REAL                          WMARG,TMARG
       LOGICAL         BBPRT1,BBPRT2
       COMMON /CTCAOK/ BBPRT1,BBPRT2,WMARG,TMARG
*
       REAL      RKVCM,RKVCM1,RKVCM2,RKYLY,RNY,TWSUPC,RKORDA,
     &           BSTEP,ACCTR,FTJBR,H1,H2,TSUM,HEELS,TY,SLA,TX,TANSL,
     &           EL,T,SLI,DY,SL1,SL2,SLASK,SLD,X,WDM,RKO1,RKO2,SLS,YB,
     &           FYFAKT,RKB1,RKB2,B2MAX,SLE,SLU,OKL,OKF,TIT,DA,UTURN0,
     &           BLEAK,X1MAX,X1MIN,FINDR,DYL,ETA,ETAL,TCD,TCORST,
     &           TYP,PLIMB,TASEC1,TASEC2,AF,PLIMBY,TJBR,PMIN,DUMMY,
     &           YTA1,YTA2,OLAPP,POE,P,TNCORS,DB,X1MIX,X1MAX1,BT,H,HS,
     &           HLIMB,DCOR,DCORN,RCORN
*
       REAL      ZU(20),COM(110),SLZ(110),X1Z(0:110),B2Z(110),OKZ(110),
     &           XGZ(110),T1Z(0:110),YZ(110),
     &           Y1(0:100),Y2(100),YG(0:100),RK2Z(110),
     &           Z1(100),ZY(0:100)
*
       INTEGER   NUM,IHO,JV,IOH,IHB,IBFLPL,ITFLPL,NCDLTB,M,NB,
     &           KSERIE,KTEST,ISLUT,K,N1,N2,ITYP,ISLS,
     &           ISLU,IVIKT,ITXTY,ISLE,IKO2,NFLPL,ILACK,N,IPMIN,INDATA,
     &           LCD,IADON,I,ITEST,J,KOD,NCD,J1
*
       INTEGER   IND(5), CDUCT(0:20),
     &           IOK(100),IXG(100),IX1(0:100),IZX(0:100)
*
       LOGICAL   BBSLIM

       include'consts.h'
*
       DATA
     & CDUCT/21*0/,IOK/100*0/,IX1/101*0/,IXG/100*0/,IZX/101*0/,
     & Y1/101*0./,Y2/100*0./,YG/101*0./,ZU/20*0./,Z1/100*0./,ZY/101*0./,
     & TASEC1/0./,TASEC2/0./
*
       INDATA  = COM(1)
       TYP     = COM(2)
       DCOR    = COM(3)
       PLIMB   = COM(4)
       PLIMBY  = COM(5)
       HLIMB   = COM(6)
       KSERIE  = COM(7)
       LCD     = COM(8)
       AF      = COM(9)
       DCORN   = COM(11)
       X1MIN   = COM(12)
       X1MAX   = COM(13)
       FINDR   = COM(16)
       TWSUPC  = COM(22)
       DYL     = COM(23)
       IADON   = COM(24)
       BLEAK   = COM(25)
       BSTEP   = ANINT(COM(33))
       IPMIN   = COM(34)
       NFLPL   = COM(36)
       IBFLPL  = COM(37)
       ITFLPL  = COM(38)
       TCD     = COM(41)
       TCORST  = COM(42)
       ETA     = COM(43)
       ILACK   = COM(15)+0.1
       IF (COM(14).LT.0.01) COM(14) = COM(43)
       ETAL    = COM(14)
       N       = COM(53)
       ACCTR   = COM(95)
       KOD     = COM(97)
*
       TNCORS=TCORST*ETA
*
C... Constants
*
       OLAPP=20.
       POE=2.
       P=4.2
       IF (INDATA.EQ.3) GOTO 1370
*
       CALL DNET(KSERIE,KOD,HLIMB,ACCTR,DCOR,DCORN,TASEC1,TASEC2)
       BT=NFLPL/2*IBFLPL+NFLPL/2*5-5
       H=0.5*DCORN-0.5*SQRT(DCORN*DCORN-BT*BT)
       HS=H+FLOAT(ITFLPL)+3.
       IF (KOD.EQ.1 )HS=HS+2.
*
       IF (KSERIE.NE.4) THEN
*
C... Calculate the net- and the gross-diameter
*
       DB=DCOR-AINT(1.3333E-3*DCOR+3.5)
*
C... Minimum and maximum width of limb-plates
*
       RKORDA=SQRT(4.*((0.5*DCORN)**2-(0.5*DCORN-HS)**2))
       X1MIN=10.*AINT((0.5*RKORDA-0.5*TCD)*0.1+1.)
       X1MIN=AMAX1(X1MIN,100.)
       X1MIX=10.*AINT((0.5*BT+10.)*0.1+1.)
       IF (NFLPL.EQ.4)X1MIN=AMAX1(X1MIN,X1MIX)
       X1MAX1=0.5*(DCOR-30.-TCD)+0.1
       IF (DCOR.GE.700.) X1MAX1=0.5*(DCOR-35.-TCD)+0.1
       X1MAX=10.*AINT(X1MAX1*0.1)
*
C... Calculating number of cooling ducts
*
       LCD=NCDLTB(TYP,DCOR)
       END IF
       NCD=LCD
*
C... Maximum number of packets
*
       N=INT((X1MAX-X1MIN))/NINT(BSTEP)+1
*
C... Laying out the limb-section
*
       RCORN=0.5*DCORN
       Z1(N)=X1MAX+0.5*TCD
       Y1(N)=SQRT(RCORN*RCORN-Z1(N)*Z1(N))
       TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,X1MAX)
       Y1(N)=AINT(Y1(N)/TJBR)*TJBR
       Y2(N)=Y1(N)
       Z1(N)=Z1(N)-0.5*TCD
       IX1(N)=INT(Z1(N)+0.1)
       J1=N-1
       DO 300 J=1,J1
       I=N-J
       Z1(I)=Z1(I+1)-BSTEP+0.5*TCD
       Y2(I)=SQRT(RCORN*RCORN-Z1(I)*Z1(I))
       IF (Y2(1).GT.RCORN-HS.AND.I.EQ.1)Y2(I)=RCORN-HS
       TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,Z1(I)-0.5*TCD)
       Y2(I)=AINT(Y2(I)/TJBR)*TJBR
       Y1(I)=Y2(I)-Y2(I+1)
       Z1(I)=Z1(I)-0.5*TCD
       IX1(I)=INT(Z1(I)+0.1)
 300   CONTINUE
*
C... Minimum thickness of packets
*
       IX1(0)=0
       IZX(0)=0
       Y1(0)=0.
       ZY(0)=0.
       ITEST=0
       YTA1=0.
       YTA2=0.
*
C... From J=1 to J=N
*
 350   DO 355 I=1,N
       IZX(I)=IX1(I)
 355   ZY(I)=Y1(I)
       N1=N
       J=1
 370   TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,FLOAT(IZX(J)))
       PMIN=FLOAT(IPMIN)*TJBR-0.1
       IF (ZY(J).LT.PMIN) THEN
       ZY(J)=ZY(J)+ZY(J+1)
       N1=N1-1
       K=J+1
       I=K
 375   ZY(I)=ZY(I+1)
       IZX(I)=IZX(I+1)
       I=I+1
       IF (I.LE.N1) GOTO 375
*
       GOTO 370
       END IF
       IF (J.NE.N1) THEN
       J=J+1
       GOTO 370
       END IF
       IF (ITEST.NE.1) THEN
       DO 391 I=1,N1
 391   YTA1=YTA1+FLOAT(IZX(I))*ZY(I)
       ELSE
       DO 396 I=1,N1
       IX1(I)=IZX(I)
 396   Y1(I)=ZY(I)
       N=N1
       GOTO 500
       END IF
*
C... From J=N up to J=1
*
 450   DO 455 I=1,N
       IZX(I)=IX1(I)
 455   ZY(I)=Y1(I)
       N2=N
       J=N2
 470   TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,FLOAT(IZX(J)))
       PMIN=FLOAT(IPMIN)*TJBR-0.1
       IF (ZY(J).GE.PMIN) GOTO 480
       IF (J.NE.1) ZY(J-1)=ZY(J)+ZY(J-1)
       N2=N2-1
       K=J
       DO 475 I=K,N2
       ZY(I)=ZY(I+1)
 475   IZX(I)=IZX(I+1)
       GOTO 470
 480   IF (J.EQ.1) GOTO 490
       J=J-1
       GOTO 470
 490   IF (ITEST.EQ.1) GOTO 495
       DO 491 I=1,N2
 491   YTA2=YTA2+FLOAT(IZX(I))*ZY(I)
       GOTO 497
 495   DO 496 I=1,N2
       IX1(I)=IZX(I)
 496   Y1(I)=ZY(I)
       N=N2
       GOTO 500
 497   ITEST=1
       DO 498 I=1,N
       IZX(I)=IX1(I)
 498   ZY(I)=Y1(I)
       IF (ABS(YTA1-YTA2).LE.YTA1*0.001) GOTO 499
       IF (YTA1-YTA2)450,350,350
 499   IF (N1-N2)350,350,450
 500   X1MAX=IX1(N)
       X1MIN=IX1(1)
       Y2(N)=Y1(N)
       J1=N-1
       DO 501 J=1,J1
       I=N-J
 501   Y2(I)=Y2(I+1)+Y1(I)
       NB=N
*
C... Placing the cooling ducts
*
       SL2=0.
       SL1=100.
       DY=TCORST
       SLI=0.
       JV=0
       DO 690 I=1,10
       CDUCT(I)=0
 690   ZU(I)=0.
       IF (LCD.EQ.0) GOTO 900
       DO 700 I=1,N
 700   SLI=SLI+FLOAT(IX1(I))*Y1(I)
       I=1
       RKYLY=2.*Y2(1)+X1MAX-FLOAT(LCD)*P

 710   IF (I.LE.(LCD+1)/2) THEN
       IF (LCD-2*(LCD/2).NE.0) THEN
       M=INT(FLOAT(N)*((2.*FLOAT(I)/FLOAT(LCD+1))**0.25)+0.5)
       JV=1
       ELSE
       M=INT(FLOAT(N)*((2.*FLOAT(I)/FLOAT(LCD))**(1./3.))+0.5)
       END IF
       SLI=SLI-P*FLOAT(IX1(M))
       RKYLY=RKYLY+2.*FLOAT(IX1(M))
       I=I+1
       GOTO 710
       END IF
       IF (JV.NE.0) THEN
       SLI=SLI+0.5*P*FLOAT(IX1(N))
       RKYLY=RKYLY-FLOAT(IX1(N))
       END IF
       RNY=SLI*ETA*DENCST*100.*POE/RKYLY
       IF (JV.EQ.1) THEN
       LCD=(LCD-1)/2
       CDUCT(0)=N+1
       NUM=0
       IHO=0
       Y2(N+1)=0.5*P
       Y1(N)=Y1(N)-Y2(N+1)
       ELSE
       LCD=LCD/2
       END IF
 760   RKYLY=FLOAT(IX1(N))
       IF (JV.EQ.1)RKYLY=2.*RKYLY
       I=N
       SLI=0.
       SLASK=ETA*DENCST*100.*POE
       NUM=1
 770   CONTINUE
       X=(RNY*RKYLY-SLASK*SLI)/(SLASK*FLOAT(IX1(I))-2.*RNY)
 780   IF (X.GT.Y1(I).OR.NUM.GT.LCD) THEN
       RKYLY=RKYLY+2.*Y1(I)
       SLI=SLI+FLOAT(IX1(I))*Y1(I)
       IF (I.EQ.CDUCT(NUM-1)) THEN
       RKYLY=RKYLY-2.*(ZU(NUM-1)+P)
       SLI=SLI-FLOAT(IX1(I))*(ZU(NUM-1)+P)
       END IF
       I=I-1
       IF (I.EQ.0) GOTO 850
       GOTO 770
       END IF
       IF (I.NE.CDUCT(NUM-1)) GOTO 800
       IF (Y1(I)-X-ZU(NUM-1)-P  .GE.0.) GOTO 795
       X=Y1(I)+10.
       GOTO 780
 795   X=X+ZU(NUM)+P
 800   IF (Y1(I).GE.3.*P) GOTO 805
       GOTO 820
 805   IF (X.LE.P) GOTO 810
       IF (X.GT.Y1(I)-2.*P) GOTO 815
       ZU(NUM)=DY*AINT(X/DY)
       GOTO 830
 810   ZU(NUM)=P
       GOTO 830
 815   ZU(NUM)=Y1(I)-2.*P
       GOTO 830
 820   IF (Y1(I).GE.2.*P) GOTO 821
       GOTO 830
 821   IF (X.LE.1.2) GOTO 822
       IF (X.GT.Y1(I)-6.) GOTO 823
       ZU(NUM)=DY*AINT(X/DY)
 822   ZU(NUM)=1.2
       GOTO 840
 823   ZU(NUM)=Y1(I)-6.0
       GOTO 840
 830   IF (Y1(I).LT.2.*P) THEN
       I=I-1
       ZU(NUM)=DY
       END IF
 840   CDUCT(NUM)=I
       RKYLY=2.*FLOAT(IX1(I))
       SLI=0.
       NUM=NUM+1
       GOTO 770
 850   CONTINUE
       WDM=SLASK*SLI/RKYLY
       SLD=RNY-WDM
       IF (ABS(SLD).GT.0.050*RNY) THEN
       IF (RNY.GE.WDM.AND.SL1.GE.RNY)SL1=RNY
       IF (RNY.LT.WDM.AND.SL2.LT.RNY)SL2=RNY
       IF (SL1.GE.100..OR.SL2.LE.0.) THEN
       RNY=RNY-SLD/(FLOAT(LCD)+1.)
       ELSE
       RNY=(SL1+SL2)*0.5
       END IF
       IF (ABS(SL1-SL2).LT.0.11) THEN
       WDM=(SL1+SL2)*0.5
       ELSE
       NUM=0
       IHO=IHO+1
       IF (IHO.GT.10) GOTO 1999
       GOTO 760
       END IF
       END IF
 900   CONTINUE
*
C... Calculating the yoke
*
C... TCA-Yoke
*
       IF (KSERIE.EQ.4) THEN
          KTEST=0
          DO 903 I=1,N
             X1Z(I)=IX1(I)
 903      CONTINUE
 904      CALL TCAOK(TYP,N,OLAPP,BSTEP,DCOR,TWSUPC,AF,X1Z,Y1,
     &            FINDR,OKZ,XGZ,BBPRT1,BBPRT2)
*
          DO 905 I=1,N
             IOK(I)=OKZ(I)
             IXG(I)=XGZ(I)
 905      CONTINUE
*
          BBSLIM=.TRUE.
*
          IF (BLEAK.GE.1.E-6) THEN
             T=IXG(1)
             J=1
             DO 907 I=1,N
                IF (IXG(I).LE.T-1.E-5) THEN
                   IND(J)=I
                   J=J+1
                END IF
                T=IXG(I)
 907         CONTINUE
             TSUM=0.
             DO 908 I=1,IND(1)-1
                TSUM=TSUM+Y1(I)
 908         CONTINUE
             HEELS=TSUM
             H1=IOK(IND(1))
             H2=IOK(IND(2))
             DUMMY=TWSUPC
             CALL WDGSUP(FINDR,IADON,KTEST,DYL,DCOR,
     &             HEELS,H1,H2,BLEAK,TWSUPC,BBSLIM,TANSL,WMARG,TMARG)
*
             IF (DUMMY.NE.TWSUPC) GOTO 904
             IF (KTEST.EQ.1) THEN
                FINDR=FINDR+0.1
                GOTO 904
             END IF
          END IF
       ELSE
          COM(53)=N
          CALL OKDIM(COM,IX1,Y1,Y2,IXG,IOK)
          EL     = COM(96)
       END IF
*
       IOH=IOK(N)
*
C... Calculating the number of plates per packet SL,
C...               Width of plates in outer-limb B2,
C...                                The distance K2,
C...   The thickness of packets                  T1
C...                                         and T2
C...                                     and sum Y
*
       RKVCM=0.
       RKVCM1=0.
       RKVCM2=0.
       TX=0.
       TY=0.
       SLA=0.
       SLE=0.
       SLU=0.
       IHO=0
       DO 1200 I=1,N
          YZ(I)=0.
 1200  CONTINUE
*
C... Packet No. 1
*
       TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,FLOAT(IX1(1)))
       SLZ(1)=AINT(Y1(1)/TJBR+0.499)
       X1Z(1)=IX1(1)
       B2Z(1)=IX1(1)+AF
       OKZ(1)=IOK(1)
       XGZ(1)=IXG(1)
       RK2Z(1)=(IX1(N)-B2Z(1))/2
       T1Z(1)=SLZ(1)*TJBR
       T2Z(1)=AINT(T1Z(1)*9.85+0.5)
       T2Z(1)=T2Z(1)*0.1
       YZ(1)=T1Z(1)
       I=2
       J=2
       ISLUT=0
       NUM=LCD
       YG(0)=0.
 1210  IF (LCD.EQ.0) GOTO 1300
       IF (I.EQ.CDUCT(NUM)) THEN
          IHO=IHO+2
          IF (IHO.EQ.2) THEN
             YG(IHO-1)=Y1(I)-P  -ZU(NUM)
             YG(IHO)=ZU(NUM)
          ELSE
             YG(IHO-3)=Y1(I)-P  -ZU(NUM)
             YG(IHO-1)=ZU(NUM)-P  -YG(IHO-2)
          END IF
          NUM=NUM-1
          GOTO 1210
       END IF
       IF (IHO.LE.0) GOTO 1300
 1260  TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,FLOAT(IX1(I)))
       SLZ(J)=AINT(YG(IHO-1)/TJBR+0.499)
       X1Z(J)=IX1(I)
       B2Z(J)=IX1(I)+AFX
       OKZ(J)=IOK(I)
       XGZ(J)=IXG(I)
       RK2Z(J)=(IX1(N)-IX1(I))/2
       T1Z(J)=SLZ(J)*TJBR
       T2Z(J)=AINT(T1Z(J)*9.85+0.5)
       T2Z(J)=T2Z(J)*0.1
       YZ(J)=YZ(J-1)+T1Z(J)
       IHO=IHO-2
 1275  J=J+1
       SLZ(J)=1
       X1Z(J)=IX1(I)
       B2Z(J)=IX1(I)+AF
       WRITE(*,*) 'SIZE OF OKZ', SIZE(OKZ)
       WRITE(*,*) 'OKZ :', OKZ

       OKZ(J)=IOK(I)
       XGZ(J)=IXG(I)
       RK2Z(J)=(IX1(N)-IX1(I))/2
       T1Z(J)=P
       IF (ISLUT.EQ.1)T1Z(J)=0.5*P
       T2Z(J)=AINT(T1Z(J)*9.85+0.5)
       T2Z(J)=T2Z(J)*0.1
       YZ(J)=YZ(J-1)+T1Z(J)
       IF (ISLUT.EQ.1) GOTO 1360
       J=J+1
       IF (IHO.NE.0) GOTO 1260
       TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,FLOAT(IX1(I)))
       SLZ(J)=AINT(YG(2)/TJBR+0.499)
       X1Z(J)=IX1(I)
       B2Z(J)=IX1(I)+AF
       OKZ(J)=IOK(I)
       XGZ(J)=IXG(I)
       RK2Z(J)=(IX1(N)-IX1(I))/2
       T1Z(J)=SLZ(J)*TJBR
       T2Z(J)=AINT(T1Z(J)*9.85+0.5)
       T2Z(J)=T2Z(J)*0.1
       YZ(J)=YZ(J-1)+T1Z(J)
       IHO=0
       IF (I.GE.N) GOTO 1350
       I=I+1
       J=J+1
       GOTO 1210
 1300  TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,FLOAT(IX1(I)))
       SLZ(J)=AINT(Y1(I)/TJBR+0.499)
       X1Z(J)=IX1(I)
       B2Z(J)=IX1(I)+AF
       OKZ(J)=IOK(I)
       XGZ(J)=IXG(I)
       RK2Z(J)=(IX1(N)-B2Z(J))/2
       T1Z(J)=SLZ(J)*TJBR
       T2Z(J)=AINT(T1Z(J)*9.85+0.5)
       T2Z(J)=T2Z(J)*0.1
       YZ(J)=YZ(J-1)+T1Z(J)
       IF (I.LT.N) THEN
       I=I+1
       J=J+1
       GOTO 1210
       END IF
 1350  CONTINUE
       IF (JV.NE.0) THEN
          ISLUT=1
          GOTO 1275
       END IF
 1360  CONTINUE
*
C... New N
*
       N=J
       GOTO 1390
*
C... Steps of outer-limb, thickness of packet, number of
C...     cooling ducts, yoke height, X1MAX for the core
*
 1370  CONTINUE
*
C... The first packet
*
       RK2Z(1)=(X1Z(N)-B2Z(1))/2
       T1Z(1)=SLZ(1)*TCORST
       YZ(1)=T1Z(1)
       DO 1375 I=2,N
       RK2Z(I)=(X1Z(N)-B2Z(I))/2
       IF (SLZ(I).EQ.1)LCD=LCD+1
       T1Z(I)=SLZ(I)*TCORST
       IF (SLZ(I).EQ.1)T1Z(I)=P
 1375  YZ(I)=YZ(I-1)+T1Z(I)
       IF (SLZ(N).LE.2) THEN
          T1Z(N)=P*0.5
          YZ(N)=YZ(N)-P*0.5
       END IF
       IF (LCD.NE.0) THEN
          IF (LCD/2*2.NE.LCD) THEN
             LCD=LCD*2-1
             IF (SLZ(N).GE.2)LCD=LCD+1
          ELSE
             LCD=LCD*2
             IF (SLZ(N).EQ.1)LCD=LCD-1
          END IF
       END IF
       IOH=OKZ(N)
       X1MAX=X1Z(N)
       NCD=LCD
       NB=N-NCD
       X1MIN  = COM(49)
       DCORN  = COM(51)
       DB     = COM(52)
*
C... Space for supporting plate
*
       EL=0.
       DO 1385 I=1,N
       IF (XGZ(I).LE.TWSUPC) GOTO 1386
       EL=EL+T1Z(I)
 1385  CONTINUE
 1386  CONTINUE
*
C... Length of yoke
*
 1390  B2MAX=B2Z(N)
       ITYP=TYP
       IF (ITYP.EQ.7) THEN
          OKL=2.*PLIMBY+B2MAX
       ELSE IF (ITYP.EQ.8) THEN
          OKL=PLIMB+2.*PLIMBY+B2MAX
       ELSE IF (ITYP.EQ.9.OR.
     &          ITYP.EQ.10) THEN
          OKL=2.*(PLIMB+PLIMBY)+B2MAX
       END IF
*
C... Calculating turn voltage area, mass, yoke re-inforcement
*     
       DO 1400 I=1,N
          RKVCM1=SLZ(I)*X1Z(I)+RKVCM1
          RKVCM2=SLZ(I)*B2Z(I)+RKVCM2
          RKVCM=SLZ(I)*OKZ(I)+RKVCM
          TX=TX+SLZ(I)*X1Z(I)*XGZ(I)
          TY=TY+SLZ(I)*B2Z(I)*XGZ(I)
          SLA=SLA+SLZ(I)*X1Z(I)*OKZ(I)
          SLE=SLE+SLZ(I)*B2Z(I)*OKZ(I)
          SLU=SLU+SLZ(I)*OKZ(I)*RK2Z(I)
 1400  CONTINUE
*
       DA=TNCORS*8.*DENCST*1.E-2
       RKVCM1=RKVCM1*TNCORS*0.04
       UTURN0=RKVCM1*2.22*0.01
       TIT=TNCORS*0.02
       RKVCM2=RKVCM2*TIT
       RKVCM=RKVCM*TIT
       OKF=2.*RKVCM/RKVCM1
       IHO=TYP-6
       IF (IHO.GT.3)IHO=3
       TX=TX*DA*FLOAT(IHO)
       RKB1=RKVCM1*FLOAT(IHO)*DENCST
       TY=TY*DA
       RKB2=RKVCM2*2.*DENCST
       SLS=RKB1+RKB2
       ITXTY=INT(TX+TY+0.5)
       SLA=SLA*FLOAT(IHO)*DA
       SLE=SLA+SLE*DA
       ISLE=INT(SLE+0.5)
       IHB=(TYP-6)*2
       IF (ABS(TYP-10.).LT.0.001) IHB=3
       RKO1=RKVCM*2.*DENCST
       RKO2=-SLU*DA-FLOAT(IHB)*RKVCM*TCD*DENCST-SLE
       IKO2=INT(RKO2+0.5)
*
C... Contact width of the winding support
*
       DO 1600 I=1,N
          IF (XGZ(I).LE.1) THEN
             YB=YZ(N)-YZ(I-1)
             GOTO 1610
          END IF
 1600  CONTINUE
 1610  CONTINUE
       FYFAKT=400.*RKVCM1/(PI*(DCOR*DCOR))
       ISLS=INT(HLIMB*SLS+FLOAT(ITXTY)+0.5)
       ISLU=INT(OKL*RKO1+RKO2+0.5)
       IVIKT=ISLS+ISLU+ISLE
       ISLS=ISLS+ISLE/2
       ISLU=ISLU+ISLE/2
       COM(16) = FINDR
       COM(22) = TWSUPC
       COM(26) = TANSL
       COM(44) = UTURN0
       COM(45) = FYFAKT
       COM(46) = RKVCM1+0.5
       COM(47) = OKF
       COM(48) = NB
       COM(49) = X1MIN
       COM(50) = X1MAX
       COM(51) = DCORN
       COM(52) = DB
       COM(53) = N
*
C... HLIMB*SLS+ITXTY,  OKL*RKO1+IKO2
*
       COM(55) = SLS
       COM(56) = ITXTY
       COM(57) = RKO1
       COM(58) = IKO2
       COM(71) = TASEC1
       COM(72) = TASEC2
       COM(76) = 2.*RKVCM2/RKVCM1
       COM(77) = AINT(HLIMB*RKB1+TX+0.5)
       COM(78) = AINT(HLIMB*RKB2+TY+0.5)
       COM(79) = RKB1
       COM(80) = RKB2
       COM(81) = TX
       COM(82) = TY
       COM(83) = IVIKT
       COM(87) = NCD
       COM(88) = ISLS
       COM(89) = ISLU
       COM(90) = ISLE
       COM(91) = IOH
       COM(92) = 2.*YZ(N)
       COM(93) = OKL
       COM(94) = YB
       COM(96) = EL
*
 1999  RETURN
       END
