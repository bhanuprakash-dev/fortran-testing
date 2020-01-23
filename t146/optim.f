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
*      Subroutine OPTIM
*      ----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Layout for the TA1 Core
C...               All references to 20. changed to BSTEP
C...               BSTEP = Width of Step Increase (Input Value)
*
************************************************************************
*
       SUBROUTINE OPTIM(DCORN,F1,TCORST,RNK,PLIMB,HLIMB,FHD,ETA,
     &                  BSTEP,BPLMAX,BBLARM,ALARM)
*
       COMMON /ROPTIM/ ARE,REST,TK,RLK1,BSMAX,BSMIN,
     &                 RMCORE,RMLIMB,RMCORN,RMYOKE,
     &                 RNS1,SW,TPKT
       REAL            ARE,REST,TK,RLK1,BSMAX,BSMIN,
     &                 RMCORE,RMLIMB,RMCORN,RMYOKE,
     &                 RNS1(100),SW(100),TPKT(100)
*
       COMMON /IOPTIM/ NPKTS
       INTEGER         NPKTS
*
       REAL      DCORN,S1,TCORST,B1MIN,RI,F1,H1,A,A1,AUX,RNK,
     &           PLIMB,HLIMB,STP,FHD,AUXS,TACC,AUXST,
     &           ARE1,ARE2,ARE3,B1MAX,V1,V2,RMLTPK,RMINPK,
     &           RDN,BSTEP,BPLMAX,BNOTCH,HNOTCH,ETA
*
       REAL      DIST(3),PRC(4),RDNNP(21,2),
     &           F(100),H(100,100),BPLPKT(100),TPHT(100)
*
       INTEGER   I,J,M,N1,LOC,NL,LSH,KPKT,MPKT,
     &           ICLO(3),IXINDX(100),IWINDX(100,100)
*
       LOGICAL   BBLARM
*
       CHARACTER ALARM*50

       include'consts.h'
*
C... Each Packet shall have a multiple of RMLTPK sheets
C... Each packet shall have at least RMINPK sheets
*
       RMLTPK=12.
       RMINPK=36.
*
       RDNNP( 1,1)=333.
       RDNNP( 1,2)= 5.
       RDNNP( 2,1)=384.
       RDNNP( 2,2)= 6.
       RDNNP( 3,1)=428.
       RDNNP( 3,2)= 7.
       RDNNP( 4,1)=474.
       RDNNP( 4,2)= 8.
       RDNNP( 5,1)=508.
       RDNNP( 5,2)= 9.
       RDNNP( 6,1)=541.
       RDNNP( 6,2)=10.
       RDNNP( 7,1)=572.
       RDNNP( 7,2)=11.
       RDNNP( 8,1)=600.
       RDNNP( 8,2)=12.
       RDNNP( 9,1)=621.
       RDNNP( 9,2)=13.
       RDNNP(10,1)=646.
       RDNNP(10,2)=14.
       RDNNP(11,1)=672.
       RDNNP(11,2)=15.
       RDNNP(12,1)=698.
       RDNNP(12,2)=16.
       RDNNP(13,1)=723.
       RDNNP(13,2)=17.
       RDNNP(14,1)=749.
       RDNNP(14,2)=18.
       RDNNP(15,1)=775.
       RDNNP(15,2)=19.
       RDNNP(16,1)=800.
       RDNNP(16,2)=20.
       RDNNP(17,1)=825.
       RDNNP(17,2)=21.
       RDNNP(18,1)=850.
       RDNNP(18,2)=22.
       RDNNP(19,1)=875.
       RDNNP(19,2)=23.
       RDNNP(20,1)=940.
       RDNNP(20,2)=24.
       RDNNP(21,1)=2000.
       RDNNP(21,2)=25.
*
       PRC(1)=0.31
       PRC(2)=0.24
       PRC(3)=0.20
       PRC(4)=0.17
*
C... BBLARM is used to transmit a warning to the designer via the panel
*
       BBLARM=.FALSE.
       ALARM='                                                  '
*
C... HNOTCH and BNOTCH define the dimensions of the
C... step in the surface of the yoke against the winding
*
       HNOTCH=40.
       BNOTCH=65.
*
       DO 20 I=1,100
          IXINDX(I)=0
          DO 30 J=1,100
             H(I,J)=0.
             IWINDX(I,J)=0
   30     CONTINUE
   20  CONTINUE
*
       DO 5 I=1,100
          BPLPKT(I)=0.
    5  CONTINUE
*
       DO 6 I=1,3
          ICLO(I)=0
    6  CONTINUE
*
       DO 7 I=1,21
          IF (DCORN.LE.RDNNP(I,1)) THEN
             NPKTS=IFIX(RDNNP(I,2))
             GOTO 8
          END IF
    7  CONTINUE
    8  CONTINUE
*
       RDN=2.*SQRT((DCORN/2.)**2-(0.1*DCORN)**2)
       B1MAX=IFIX(RDN/BSTEP)*BSTEP
       IF (B1MAX.GT.BPLMAX) B1MAX=BPLMAX
       S1=SQRT(DCORN**2-F1**2)
       B1MIN=IFIX(S1/BSTEP)*BSTEP
       N1=0
       DO 10 RI=B1MIN,B1MAX,BSTEP
          N1=N1+1
          BPLPKT(N1)=RI
   10  CONTINUE
*
       IF (N1.LT.NPKTS) THEN
          BBLARM=.TRUE.
          ALARM='Check the Maximum Width of Core Plate Available.'
          GOTO 199
       END IF
       M=N1
*
  140  F(1)=F1/2.
       DO 40 I=2,M
          F(I)=SQRT(DCORN**2-BPLPKT(I)**2)/2.
   40  CONTINUE
       DO 50 I=2,(M-NPKTS+2)
          H(NPKTS,I)=0.
          DO 60 J=1,(I-1)
             H1=BPLPKT(J)*(F(J)-F(I))
             IF (H1.GT.H(NPKTS,I)) THEN
                H(NPKTS,I)=H1
                IWINDX(NPKTS,I)=J
          END IF
   60     CONTINUE
   50  CONTINUE
       DO 70 KPKT=(NPKTS-1),2,-1
          DO 80 I=(2+NPKTS-KPKT),(M-KPKT+2)
             H(KPKT,I)=0.
             DO 90 J=(NPKTS-KPKT+1),(I-1)
                H1=BPLPKT(J)*(F(J)-F(I))+H(KPKT+1,J)
                IF (H1.GT.H(KPKT,I)) THEN
                   H(KPKT,I)=H1
                   IWINDX(KPKT,I)=J
                END IF
   90        CONTINUE
   80     CONTINUE
   70  CONTINUE
       A=0.
       DO 100 J=NPKTS,M
          A1=2*(BPLPKT(J)*F(J)+H(2,J))
          IF (A1.GT.A) THEN
             A=A1
             IXINDX(1)=J
          END IF
  100  CONTINUE
       DO 110 KPKT=2,NPKTS
          IXINDX(KPKT)=IWINDX(KPKT,IXINDX(KPKT-1))
  110  CONTINUE
       KPKT=1
       TPKT(KPKT)=F(IXINDX(KPKT))+0.009
       RNS1(KPKT)=TPKT(KPKT)/TCORST
       DO 130 KPKT=2,NPKTS
          TPKT(KPKT)=F(IXINDX(KPKT))-F(IXINDX(KPKT-1))
          RNS1(KPKT)=TPKT(KPKT)/TCORST
  130  CONTINUE
*
C... Locate the Cooling Ducts
*
       LOC=0
       DIST(1)=0.
       DIST(2)=0.
       DIST(3)=0.
       IF (RNK.GT.1.) THEN
          DIST(1)=F1*PRC(NINT(RNK)-1)
          IF (RNK.GT.3.)DIST(2)=2.*DIST(1)
       END IF
       AUX=0.
       I=1
       DO 200 KPKT=NPKTS,1,-1
          AUX=AUX+TPKT(KPKT)
       IF (DIST(I).LE.AUX.AND.DIST(I).NE.0.) THEN
          IF (((AUX-DIST(I))/TPKT(KPKT)).LT.0.2) THEN
             TPKT(KPKT)=TPKT(KPKT)-4.
             LOC=LOC+1
             ICLO(LOC)=KPKT
          ELSE
             TPKT(KPKT+1)=TPKT(KPKT+1)-4.
             LOC=LOC+1
             ICLO(LOC)=KPKT+1
          END IF
          I=I+1
       END IF
  200  CONTINUE
       IF (RNK.EQ.1.OR.
     &     RNK.EQ.3.OR.
     &     RNK.EQ.5) THEN
       TPKT(1)=TPKT(1)-2.
       LOC=LOC+1
       ICLO(LOC)=1
       END IF
*
C... Make the packets a multiple of RMLTPK
*
       ARE1=0.
       ARE2=0.
       REST=0.
       DO 180 KPKT=1,NPKTS
          ARE1=ARE1+2.*(TPKT(KPKT)*BPLPKT(IXINDX(KPKT)))*ETA
          RNS1(KPKT)=IFIX((TPKT(KPKT)+REST)/(TCORST*RMLTPK))*RMLTPK
          REST=(TPKT(KPKT)+REST)-(RNS1(KPKT)*TCORST)
          TPKT(KPKT)=RNS1(KPKT)*TCORST
          ARE2=ARE2+2.*(TPKT(KPKT)*BPLPKT(IXINDX(KPKT)))*ETA
  180  CONTINUE
*
C... Make the minimum packet with RMINPK sheets
*
       REST=REST*2.
       ARE3=0.
       DO 190 KPKT=NPKTS,1,-1
          IF (RNS1(KPKT).LT.RMINPK) THEN
             RNS1(KPKT-1)=RNS1(KPKT-1)-(RMINPK-RNS1(KPKT))
             RNS1(KPKT)=RMINPK
          END IF
          TPKT(KPKT)=RNS1(KPKT)*TCORST
          ARE3=ARE3+2.*(TPKT(KPKT)*BPLPKT(IXINDX(KPKT)))*ETA
  190  CONTINUE
*
C... Adjust the width to fill all available space
*
       AUXST=0.
       AUXS=0.
       ARE=0.
       TACC=0.
       DO 557 KPKT=1,NPKTS
          TACC=TACC+TPKT(KPKT)
          DO 558 NL=1,3
             IF (ICLO(NL).EQ.KPKT) THEN
                IF (KPKT.EQ.1) THEN
                   TACC=TACC+2.
                ELSE
                   TACC=TACC+4.
                END IF
             END IF
  558     CONTINUE
          AUXST=SQRT((DCORN*0.5)**2-TACC**2)
          AUXS=ANINT(BSTEP*AINT((2.*AUXST-BPLPKT(IXINDX(KPKT)))/BSTEP))
          BPLPKT(IXINDX(KPKT))=BPLPKT(IXINDX(KPKT))+AUXS
*
          DO 559 MPKT=1,(KPKT-1)
             IF (ABS(BPLPKT(IXINDX(KPKT))-BPLPKT(IXINDX(MPKT)))
     &            .LT.(0.1)) THEN
                TPKT(MPKT)=TPKT(MPKT)+TPKT(KPKT)
                NPKTS=NPKTS-1
             END IF
  559  CONTINUE
*
          ARE=ARE+2.*(TPKT(KPKT)*BPLPKT(IXINDX(KPKT)))*ETA
  557  CONTINUE
*
C... Calculate the step for the yoke
*
       TACC=0.
       DO 120 KPKT=NPKTS,1,-1
          TACC=TACC+TPKT(KPKT)
          DO 230 NL=1,3
             IF (ICLO(NL).EQ.(KPKT+1))TACC=TACC+4.
  230     CONTINUE
          IF (TACC.GE.BNOTCH) THEN
             IF ((BPLPKT(IXINDX(KPKT-1))-
     &          BPLPKT(IXINDX(KPKT))).LT.HNOTCH) THEN
                DO 160 N1=1,M
                   IF (BPLPKT(N1).EQ.BPLPKT(IXINDX(KPKT))) THEN
                      DO 170 I=N1,(M-1)
                         BPLPKT(I)=BPLPKT(I+1)
  170                 CONTINUE
                      M=M-1
                      GOTO 140
                   END IF
  160           CONTINUE
             ELSE
                GOTO 150
             END IF
          END IF
  120  CONTINUE
*
  150  LSH=KPKT
*
       DO 220 I=1,NPKTS
          SW(I)=BPLPKT(IXINDX(I))
          STP=0.
          IF (I.GE.LSH) STP=HNOTCH
          TPHT(I)=SW(I)+STP
  220  CONTINUE
*
       V1=0.
       V2=0.
       DO 300 I=1,NPKTS
          IF (I.GE.LSH)V1=V1+(SW(I)*TPKT(I)*HNOTCH)*ETA
          V2=V2+(TPKT(I)*SW(I)**2)*ETA
  300  CONTINUE
       RMLIMB=3.*(DENCST/100.)*
     &       (HLIMB*ARE+4.*V1-(F1-REST)*FHD**2*3.1416/4.)
       RMCORN=3.*(DENCST/100.)*4.*V2
       RMYOKE=4.*(DENCST/100.)*PLIMB*ARE-(2./3.)*RMCORN
       RMCORE=RMLIMB+RMCORN+RMYOKE
*
       TK=F1-REST
       RLK1=TACC
*
       BSMAX=SW(1)
       BSMIN=SW(NPKTS)
*
C      DO 1199 I = 1, NPKTS
C      WRITE(26,*) 'nsheet, width, thickness ', RNS1(I), SW(I), TPKT(I)
C1199  CONTINUE
*
  199  RETURN
       END
