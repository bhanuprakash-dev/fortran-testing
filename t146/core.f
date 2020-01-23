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
*      Subroutine CORE
*      ---------------
*
*  0.  Written: XX-XX-XX by S-E Jansson        , ZKB
*      Revised: 86-12-09 by J. Akesson         , Z1KDC
*      Revised: 91-11-27 by Ron Bell           , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the core-section for core types D and T.
*
************************************************************************
*
       SUBROUTINE CORE(COM,RNP,B1,B3,K1,TP,TS)
       

*
       COMMON /CTAAOK/ BBTAAE
*
       REAL                          WMARG,TMARG
       LOGICAL         BBPRT1,BBPRT2
       COMMON /CTCAOK/ BBPRT1,BBPRT2,WMARG,TMARG
*
       INTEGER   IN,IX1,KTEST,L1,LNCD,NB,IEND,N2,
     &           ILACK,KSERIE,NCD,IPMIN,ITEST,I,J,K,L,M,N,N1,IADON,
     &           J1,IPKT,JPKT,NBEN
*
       INTEGER   IV,IANT,IHO,IND(5),CDUCT(0:10),NPL(100)
*
       REAL      RNY,RKYLY,BSTEP,POE,P,OLAPP,TISOL,SL1,YTA1,DUMMY,
     &           FTJBR,UT,SLD,TSUM,HEELS,T,SL2,X1MAX,X1MIN,TWSUPC,
     &           X,WDM,AUX,DY,SLI,H1,GB,G1,OKF,FYFAKT,GBEN,GOK,G2,GO,
     &           GHORN,TANSL,H2,S1,S2,UTURN0,ACORE,AOK,AF,TT,BLADN,
     &           B1MAX,B1MIN,OHRED,TH,TB,DCORN,TYP,PLIMB,ETA,TCORST,
     &           ETAL,YTA2,TVAX,PMIN,BLEAK,TNCORS,FINDR,DYL,RCORN,
     &           TJBR,HLIMB,DCOR
*
       REAL      ZU(0:10),
     &           COM(50),RNP(100),B1(0:100),B3(100),K1(100),
     &           TP(0:100),TS(100),X1(0:100),
     &           Y1(0:100),Y2(100),YG(0:100),
     &           OK(100),XG(100),ZX(0:100),ZY(0:100)
       LOGICAL BBTAAE,BBSLIM

       include'consts.h'
*
*
       POE=2.
       P=4.2
       OLAPP=20.
       TISOL=2.
       write(*,*)"CORE.F"
*
C... Internal designations and constants
*
       TYP    = COM(1)
       DCOR   = COM(2)
       DCORN  = COM(3)
       PLIMB  = COM(5)
       HLIMB  = COM(6)
       BSTEP  = ANINT(COM(7))
       IPMIN  = COM(8)
       TCORST = COM(9)
       ETA    = COM(10)
       NCD    = NINT(COM(11))
       BLADN  = COM(12)
       B1MIN  = COM(13)
       B1MAX  = COM(14)
       AF     = COM(18)
       TT     = COM(19)
       TWSUPC = COM(33)
       TH     = COM(34)
       TB     = COM(35)
       KSERIE = NINT(COM(36))
       OHRED  = COM(37)
       ILACK  = NINT(COM(38))
       IF (COM(40).EQ.0.) COM(40)= COM(10)
       ETAL   = COM(40)
       FINDR  = COM(41)
       DYL    = COM(42)
       IADON  = COM(43)
       BLEAK  = COM(44)
*
       TNCORS=TCORST*ETA
*
C... Maximum number of widths
*
       N=INT(B1MAX-B1MIN)/NINT(BSTEP)+1
*
C... Laying out the limb-section
*
       RCORN=0.5*DCORN
*
C... Laying out packet No. 'N'
*
       X1(N)=0.5*B1MAX
       Y2(N)=SQRT(RCORN*RCORN-X1(N)*X1(N))
       TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,B1MAX)
       Y2(N)=(BLADN*TJBR)*AINT(Y2(N)/(BLADN*TJBR))
       Y1(N)=Y2(N)
*
C... Laying out packet No. 'N-1' DOWN TO PACKET NO. 1
*
       J1=N-1
*
       DO 20 JPKT=1,J1
          IPKT=N-JPKT
          X1(IPKT)=X1(IPKT+1)-(0.5*BSTEP)
          Y2(IPKT)=SQRT(RCORN*RCORN-X1(IPKT)*X1(IPKT))
          TVAX=2.*X1(IPKT)
          TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,TVAX)
          Y2(IPKT)=BLADN*TJBR*AINT(Y2(IPKT)/(BLADN*TJBR))
          Y1(IPKT)=Y2(IPKT)-Y2(IPKT+1)
   20  CONTINUE
*
C... Minimum thickness of packet
*
       X1(0)=0
       ZX(0)=0
       Y1(0)=0.
       ZY(0)=0.
       ITEST=0
       YTA1=0.
       YTA2=0.
*
C... From J=1 up to J=N
*
       M=1
  200  IF (M.EQ.3) GOTO 299
*
       GOTO (301,302),M
  301  CONTINUE
       DO 499 I=1,N
          ZX(I)=X1(I)
          ZY(I)=Y1(I)
  499  CONTINUE
       N1=N
       J=1
       L=0
  500  IF (L.EQ.1) GOTO 599
          TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,2.*ZX(J))
          PMIN=TJBR*FLOAT(IPMIN)-0.1
          IF (ZY(J).GE.PMIN) GOTO 602
          ZY(J)=ZY(J)+ZY(J+1)
          N1=N1-1
          K=J+1
          DO 799 I=K,N1
             ZY(I)=ZY(I+1)
             ZX(I)=ZX(I+1)
  799     CONTINUE
          GOTO 699
  602     IF (J.EQ.N1) THEN
             L=1
          ELSE
             J=J+1
          END IF
  699     CONTINUE
          
          GOTO 500
  599  CONTINUE
       IF (ITEST.EQ.1) THEN
          DO 1099 I=1,N1
             X1(I)=ZX(I)
             Y1(I)=ZY(I)
 1099     CONTINUE
          
          N=N1
          M=3
       ELSE
          DO 999 I=1,N1
             YTA1=YTA1+ZX(I)*ZY(I)
  999     CONTINUE
          M=2
       END IF
       GOTO 399
*
  302  CONTINUE
       DO 1199 I=1,N
          ZX(I)=X1(I)
          ZY(I)=Y1(I)
 1199  CONTINUE
       N2=N
       J=N2
       L=0
 1200  IF (L.EQ.1) GOTO 1299
          TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,2.*ZX(J))
          PMIN=TJBR*FLOAT(IPMIN)-0.1
          IF (ZY(J).GE.PMIN) GOTO 1302
          IF (J.NE.1) THEN
          ZY(J-1)=ZY(J)+ZY(J-1)
          END IF
          N2=N2-1
          K=J
          DO 1599 I=K,N2
             ZY(I)=ZY(I+1)
             ZX(I)=ZX(I+1)
 1599     CONTINUE
          GOTO 1399
 1302     IF (J.EQ.1) THEN
             L=1
          ELSE
             J=J-1
          END IF
 1399     CONTINUE
          GOTO 1200
 1299  CONTINUE
*
 1600  IF (ITEST.EQ.1) GOTO 1602
       DO 1799 I=1,N2
       YTA2=YTA2+ZX(I)*ZY(I)
 1799  CONTINUE
       ITEST=1
       DO 1899 I=1,N
          ZX(I)=X1(I)
          ZY(I)=Y1(I)
 1899  CONTINUE
       IF (ABS(YTA1-YTA2).LE.YTA1*0.001) GOTO 1902
       IF (YTA1-YTA2.GE.0.) THEN
          M=1
       ELSE
          M=2
       END IF
       GOTO 1999
 1902  CONTINUE
       IF (N1-N2.LE.0) THEN
          M=1
       ELSE
          M=2
       END IF
 1999  CONTINUE
       GOTO 1699
 1602  CONTINUE
       DO 2299 I=1,N2
          X1(I)=ZX(I)
          Y1(I)=ZY(I)
 2299  CONTINUE
       N=N2
       M=3
 1699  CONTINUE
  399  CONTINUE
       GOTO 200
  299  CONTINUE
*
       X1MAX=X1(N)
       X1MIN=X1(1)
       Y2(N)=Y1(N)
       J1=N-1
       DO 2399 J=1,J1
          I=N-J
          Y2(I)=Y2(I+1)+Y1(I)
 2399  CONTINUE
*
       IV=0
       L=0
       LNCD=0
*
       IF (NCD.EQ.0) THEN
          L=7
       ELSE
          L=1
       END IF
*
 2500  IF (L.EQ.8) GOTO 2599
*
       GOTO (2601,2602,2603,2604,2605,2606,2607),L
*
 2601  LNCD=NCD
       SL2=0.
       SL1=100.
       DY=TCORST*BLADN
       SLI=0.
       DO 2799 I=0,10
          CDUCT(I)=0
          ZU(I)=0.
          
 2799  CONTINUE
*
       DO 2899 I=1,N
          SLI=SLI+X1(I)*Y1(I)
 2899  CONTINUE
*
       I=1
       RKYLY=2.*Y2(1)+X1MAX-FLOAT(LNCD)*P
 2900  IF (I.GT.(LNCD+1)/2) GOTO 2999
       IF (LNCD-2*(LNCD/2).EQ.0) THEN
          M=INT(FLOAT(N)*((2.*FLOAT(I)/FLOAT(LNCD))**(1./3.))+0.5)
       ELSE
          M=INT(FLOAT(N)*((2.*FLOAT(I)/FLOAT(LNCD+1))**0.25)+0.5)
          IV=1
       END IF
       SLI=SLI-P*X1(M)
       RKYLY=RKYLY+2.*X1(M)
       I=I+1
       GOTO 2900
 2999  CONTINUE
*
       IF (IV.NE.0) THEN
          SLI=SLI+0.5*P*X1(N)
          RKYLY=RKYLY-X1(N)
       END IF
*
       RNY=SLI*ETA*DENCST*100.*POE/RKYLY
*
       IF (IV.NE.1) THEN
          LNCD=LNCD/2
       ELSE
          LNCD=(LNCD-1)/2
          CDUCT(0)=N+1
          IANT=0
          IHO=0
          Y2(N+1)=0.5*P
          Y1(N)=Y1(N)-Y2(N+1)
       END IF
       L=2
       GOTO 2699
*
 2602  RKYLY=X1(N)
       IF (IV.EQ.1) RKYLY=2.*RKYLY
       I=N
       SLI=0.
       AUX=ETA*DENCST*100.*POE
       IANT=1
       L=3
       GOTO 2699
*
 2603  X=(RNY*RKYLY-AUX*SLI)/(AUX*X1(I)-2.*RNY)
       L=4
       GOTO 2699
*
 2604  CONTINUE
       IF (X.GT.Y1(I).OR.IANT.GT.LNCD) GOTO 3302
       IF (I.NE.CDUCT(IANT-1)) GOTO 3402
       IF (Y1(I)-X-ZU(IANT-1)-P.GE.0.) GOTO 3502
       X=Y1(I)+10.
       GOTO 3599
 3502  X=X+ZU(IANT)+P
       L=5
 3599  CONTINUE
       GOTO 3499
 3402  L=5
 3499  CONTINUE
       GOTO 3399
 3302  RKYLY=RKYLY+2.*Y1(I)
       SLI=SLI+X1(I)*Y1(I)
       IF (I.EQ.CDUCT(IANT-1)) THEN
          RKYLY=RKYLY-2.*(ZU(IANT-1)+P)
          SLI=SLI-X1(I)*(ZU(IANT-1)+P)
       END IF
       I=I-1
       IF (I.EQ.0) THEN
          L=6
       ELSE
          L=3
       END IF
 3399  CONTINUE
       GOTO 2699
*
 2605  CONTINUE
 3800  IF (Y1(I).LT.3.*P) GOTO 3802
 3801  CONTINUE
       IF (X.LE.P) THEN
          ZU(IANT)=P
       ELSE
          IF (X.GT.Y1(I)-2.*P) THEN
             ZU(IANT)=Y1(I)-2.*P
          ELSE
             ZU(IANT)=DY*AINT(X/DY)
          END IF
       END IF
       GOTO 3899
 3802  CONTINUE
       IF (Y1(I).LT.2.*P) THEN
          I=I-1
          ZU(IANT)=DY
       ELSE
          IF (X.LE.1.2) THEN
             ZU(IANT)=1.2
          ELSE
             IF (X.GT.Y1(I)-6.) THEN
                ZU(IANT)=Y1(I)-6.0
             ELSE
                ZU(IANT)=1.2
             END IF
          END IF
       END IF
 3899  CONTINUE
       CDUCT(IANT)=I
       RKYLY=2.*X1(I)
       SLI=0.
       IANT=IANT+1
       L=3
       GOTO 2699
*
 2606  WDM=AUX*SLI/RKYLY
       SLD=RNY-WDM
       IF (ABS(SLD).LE.0.050*RNY) THEN
          L=7
       ELSE
          IF (RNY.GE.WDM.AND.SL1.GE.RNY) SL1=RNY
          IF (RNY.LT.WDM.AND.SL2.LT.RNY) SL2=RNY
          IF (SL1.LT.100..AND.SL2.GT.0.) THEN
             RNY=(SL1+SL2)*0.5
          ELSE
             RNY=RNY-SLD/(FLOAT(LNCD)+1.)
          END IF
*
          IF (ABS(SL1-SL2).LT.0.11) THEN
             WDM=(SL1+SL2)*0.5
             L=7
          ELSE
             IANT=0
             IHO=IHO+1
             IF (IHO.GT.10) THEN
                L=8
             ELSE
                L=2
             END IF
          END IF
       END IF
       GOTO 2699
*
 2607  NCD=LNCD
*
       DO 4899 I=1,N
          X1(I)=2.*X1(I)
 4899  CONTINUE
*
       IF ( (KSERIE.EQ.2.AND.DCOR.GE.340.)
     &  .OR.(KSERIE.EQ.4)) GOTO 4902
       IF ( KSERIE.EQ.6) GOTO 4904
*
C... TAA-yoke
*
       OK(1)=X1(2)+OLAPP
*
       DO 5099 I=2,N
          OK(I)=X1(I)+OLAPP
 5099  CONTINUE
*
       DO 5199 I=1,N
          XG(I)=0.
 5199  CONTINUE
*
       IF (.NOT.BBTAAE) THEN
          XG(3)=OK(4)-OK(3)
          XG(2)=OK(4)-OK(2)
          XG(1)=XG(2)
*
C... Extended TAA
*
       ELSE
          M=N
          IX1=1
          CALL AVTRAL(M,IX1,XG,Y1,70.,14.)
          IX1=1
          CALL AVTRAL(M,IX1,XG,Y1,40.,70.)
*
          N1=N-1
          I=1
 5400     IF (I.GT.N1) GOTO 5499
          IF (XG(I+1)+OK(I+1).GE.XG(I)+OK(I)) THEN
             I=I+1
          ELSE
             OK(I+1)=OK(I+1)+BSTEP
          END IF
          GOTO 5400
 5499     CONTINUE
       END IF
*
C... TAA-modified
*
       IF (KSERIE.EQ.3) CALL LTSOK(OK,FLOAT(N))
       GOTO 4999
*
C... TBA-yoke
*
 4902  IF (KSERIE.EQ.4) GOTO 4903
       CALL TBAOK(N,BSTEP,X1,OLAPP,DCOR,TT,Y1,TWSUPC,TH,
     &            TB,OK,XG,OHRED)
       GOTO 4999
*
C... TAC-yoke
*
 4904  CALL TACOK(IN,DCOR,BSTEP,N,X1,Y1,XG,UT,OK)
       GOTO 4999
*
C... TCA-yoke
*
 4903  KTEST=0
       L1=0
 5700  IF (L1.EQ.1) GOTO 5799
 5710  CALL TCAOK(TYP,N,OLAPP,BSTEP,DCOR,TWSUPC,AF,X1,
     &            Y1,FINDR,OK,XG,BBPRT1,BBPRT2)
       BBSLIM=.FALSE.
 5800  IF (BLEAK.LT.1.E-6) GOTO 5802
 5801  T=XG(1)
       J=1
 5900  DO 5999 I=1,N
          IF (XG(I).LE.T-1.E-5) THEN
             IND(J)=I
             J=J+1
          END IF
*
       T=XG(I)
 5999  CONTINUE
       TSUM=0.
       DO 6199 I=1,IND(1)-1
          TSUM=TSUM+Y1(I)
 6199  CONTINUE
       HEELS=TSUM
       H1=OK(IND(1))
       H2=OK(IND(2))
       DUMMY=TWSUPC
       CALL WDGSUP(FINDR,IADON,KTEST,DYL,DCOR,
     &            HEELS,H1,H2,BLEAK,TWSUPC,BBSLIM,TANSL,WMARG,TMARG)
       IF (DUMMY.NE.TWSUPC) GOTO 5710
*
       IF (KTEST.NE.1) THEN
          L1=1
       ELSE
          FINDR=FINDR+0.1
       END IF
       GOTO 5899
 5802  L1=1
 5899  CONTINUE
       GOTO 5700
 5799  CONTINUE
 4999  CONTINUE
*
       NB=N
       IHO=0
       DO 6399 I=1,N
          TS(I)=0.
 6399  CONTINUE
       IEND=0
       YG(0)=0.
*
       TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,X1(1))
       NPL(1)=Y1(1)/TJBR+0.1
       B1(1)=X1(1)
       B3(1)=OK(1)
       K1(1)=XG(1)
       TP(1)=FLOAT(NPL(1))*TJBR
       TS(1)=TP(1)
*
       I=2
       J=2
       IANT=NCD
       L1=1
*
 6400  IF (L1.EQ.6) GOTO 6499
*
       GOTO (6501,6502,6503,6504,6505),L1
 6501  CONTINUE
       IF (NCD.EQ.0) THEN
          L1=4
       ELSE
       write(*,*)"IANT=",IANT
       IF (I.NE.CDUCT(IANT)) THEN
*
          IF (IHO.GT.0) THEN
             L1=2
          ELSE
             L1=4
          END IF
*
       ELSE
       IHO=IHO+2
*
       IF (IHO.NE.2) THEN
       YG(IHO-3)=Y1(I)-P-ZU(IANT)
       YG(IHO-1)=ZU(IANT)-P-YG(IHO-2)
       write(*,*)"IHO=",IHO
       IANT=IANT-1
       ELSE
       YG(IHO-1)=Y1(I)-P-ZU(IANT)
       YG(IHO)=ZU(IANT)
       write(*,*)"IHO1=",IHO
       IANT=IANT-1
       END IF
*
       END IF
       END IF
       GOTO 6599
*
 6502  TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,X1(I))
       NPL(J)=YG(IHO-1)/TJBR+0.1
       B1(J)=X1(I)
       B3(J)=OK(I)
       K1(J)=XG(I)
       TP(J)=FLOAT(NPL(J))*TJ BR
       TS(J)=TS(J-1)+TP(J)
       IHO=IHO-2
       L1=3
       GOTO 6599
*
 6503  J=J+1
       NPL(J)=1
       B1(J)=X1(I)
       B3(J)=OK(I)
       K1(J)=XG(I)
       IF (IEND.EQ.1) GOTO 7002
       TP(J)=P
       TS(J)=TS(J-1)+TP(J)
       J=J+1
*
       IF (IHO.NE.0) THEN
       L1=2
       ELSE
       TJBR=FTJBR(TNCORS,ETA,ETAL,
     &            ILACK,TYP,DCOR,X1(I))
       NPL(J)=YG(2)/TJBR+0.1
       B1(J)=X1(I)
       B3(J)=OK(I)
       K1(J)=XG(I)
       TP(J)=FLOAT(NPL(J))*TJBR
       TS(J)=TS(J-1)+TP(J)
       IHO=0
*
       IF (I.GE.N) THEN
       L1=5
       ELSE
       I=I+1
       J=J+1
       L1=1
       END IF
*
       END IF
*
       GOTO 7099
 7002  TP(J)=0.5*P
       TS(J)=TS(J-1)+TP(J)
       L1=6
 7099  CONTINUE
       GOTO 6599
*
 6504  TJBR=FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,X1(I))
       NPL(J)=Y1(I)/TJBR+0.1
       B1(J)=X1(I)
       B3(J)=OK(I)
       K1(J)=XG(I)
       TP(J)=FLOAT(NPL(J))*TJBR
       TS(J)=TS(J-1)+TP(J)
*
       IF (I.GE.N) THEN
       L1=5
       ELSE
       I=I+1
       J=J+1
       L1=1
       END IF
       GOTO 6599
*
 6505  CONTINUE
*
       IF (IV.EQ.0) THEN
       L1=6
       ELSE
       IEND=1
       L1=3
       END IF
*
 6599  CONTINUE
       GOTO 6400
 6499  CONTINUE
*
       N=J
       ACORE=0.
       AOK=0.
       S1=0.
       S2=0.
       DO 7599 I=1,N
       ACORE=ACORE+FLOAT(NPL(I))*B1(I)
       AOK= AOK+FLOAT(NPL(I))*B3(I)
       S1=S1+FLOAT(NPL(I))*B1(I)*K1(I)
       S2=S2+FLOAT(NPL(I))*B1(I)*B3(I)
 7599  CONTINUE
       ACORE=ACORE*TNCORS*2.E-2
       AOK=AOK*TNCORS*2.E-2
       UTURN0=ACORE*2.22144E-2
       OKF=AOK/ACORE
       FYFAKT=4.*ACORE/(PI*(DCOR*DCOR))*100.
*
C... GBEN=HLIMB*GB+G1, GOK=PLIMB*GO-G2, GHORN=GHORN
*
       NBEN=2
       IF (TYP.EQ.3.) NBEN=3
       GB=ACORE*FLOAT(NBEN)*DENCST
       G1=S1*TNCORS*DENCST*FLOAT(NBEN)*4.E-2
       GO=AOK*FLOAT(NBEN-1)*2.*DENCST
       GHORN=S2*TNCORS*DENCST*FLOAT(NBEN)*4.E-2
       G2=-0.667*GHORN
       IF (TYP.EQ.1.) G2=-0.5000*GHORN
       GBEN=HLIMB*GB+G1
       GOK=PLIMB*GO+G2
*
       COM(15)=GBEN
       COM(16)=GOK
       COM(17)=GHORN
       COM(20)=UTURN0
       COM(21)=FYFAKT
       COM(22)=ACORE
       COM(23)=OKF
       COM(24)=GB
       COM(25)=G1
       COM(26)=GO
       COM(27)=G2
       COM(28)=B3(N)
       COM(29)=2.*Y2(1)
       COM(30)=N
       COM(31)=NB
       COM(32)=AOK
       COM(33)=TWSUPC
       COM(41)=FINDR
       COM(45)=TANSL
*
       DO 7699 I=1,N
       RNP(I)=NPL(I)
 7699  CONTINUE
*
       L=8
 2699  CONTINUE
       GOTO 2500
 2599  CONTINUE
*     
 199   RETURN
       END
