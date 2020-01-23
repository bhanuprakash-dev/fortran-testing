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
*      Subroutine CONTD
*      ----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 83-04-26 by H. Westberg  , ZKAB
*      Revised: 87-04-27 by Ron Bell     , TRAFO/IK
*      Revised: 88-12-06 BY Per Sãderberg, TRAFO/IK
*
*  1.  Description
C...       Title:  Winding layout for continuous disc windings
C...               Calculations can be done in two ways:
C...            1. A simple calculation using decimal fractions.
C...            2. A full calculation with rounding to integer values.
*
************************************************************************
*
       SUBROUTINE CONTD(IWDG)
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1287(1)    ,  USURG  (1)    ),
     &  (XZ1513       ,  PI            ),
     &  (XZ1472       ,  HLIMB         )
       EQUIVALENCE
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ0671(1)    ,  BDUCT  (1)    ),
     &  (XZ1227(1)    ,  NMSTEP (1)    ),
     &  (XZ1231(1)    ,  NPSTEP (1)    ),
     &  (XZ0921(1)    ,  NGROUP (1)    )
       EQUIVALENCE
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ0751(1)    ,  DYOKE  (1)    ),
     &  (XZ1091(1)    ,  TSPIN  (1)    ),
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ0681(1)    ,  BPART  (1)    )
       EQUIVALENCE
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ1161(1)    ,  ZCODU  (1)    ),
     &  (XZ0891(1)    ,  KCODE  (1)    ),
     &  (XZ0721(1)    ,  IPISOL (1)    ),
     &  (XZ0991(1)    ,  RPART  (1)    )
       EQUIVALENCE
     &  (XZ2563       ,  BBADJU        ),
     &  (XZ2584(1)    ,  EXTRAR (1)    ),
     &  (XZ2585(1)    ,  HCLAC  (1)    ),
     &  (XZ2586(1)    ,  ZDISC  (1)    ),
     &  (XZ0911(1)    ,  KWITYP (1)    )
       EQUIVALENCE
     &  (XZ2589(1)    ,  SWIND  (1)    ),
     &  (XZ2590(1)    ,  RINNER (1)    ),
     &  (XZ2568       ,  BBISGR        ),
     &  (XZ2592(1)    ,  ZRR    (1)    ),
     &  (XZ2593(1)    ,  ZPART  (1)    ),
     &  (XZ2594(1)    ,  ZTUDI  (1)    ),
     &  (XZ2596(1)    ,  TSPIN1 (1)    )
*
       REAL      HPART1(9),EXT10(6),EXT20(6),SWIND(9),RINNER(9),
     &           ZWIND(9),USURG(4),RRWDG(9),BDUCT(9),HWIND(9),CDUCT,
     &           DYOKE(9),TSPIN(9),HPART(9),BPART(9),ACOND(9),ZCODU(9),
     &           RPART(9),EXTRAR(9),HCLAC(9),ZDISC(9),EXTU(9),
     &           ZRR(9),ZPART(9),ZTUDI(9),TSPIN1(9),FACINS,PI,HLIMB,
     &           DCORE,HELP,HELP1,RGROUP,STAB,X1
*
       INTEGER   KGROUP(9),NGROUP(9),KCODE(9),IPISOL(9),KWITYP(9),
     &           IWDG,JTML,NPSTEP(4),NMSTEP(4)
*
       LOGICAL   BBISGR,BBADJU
*
       DATA (EXT20(I),I=1,6)/0.,2.,2.,2.,4.,4./,
     &      (EXT10(I),I=1,6)/2.,2.,4.,6.,6.,8./,
     &      FACINS /0.95/
*
C...   Help variables HELP1 and X1 have to be initiated.
*
       HELP1=0.
       X1=1.
*
C...   LINDNINGSHöJD
*
       HWIND(IWDG)=HLIMB-DYOKE(IWDG)
       JTML=KGROUP(IWDG)
*
C...   TOTAL BREDD AV KYLKANALER
*
       CDUCT=ZCODU(IWDG)*0.005
       RGROUP=FLOAT(NGROUP(IWDG))
       HPART1(IWDG)=HPART(IWDG)+TSPIN(IWDG)
       HELP=HWIND(IWDG)-EXTRAR(IWDG)
       IF (.NOT.BBADJU) GOTO 1000
*
C... Following calculation formulae are used during the optimization
*
       IF (KCODE(IWDG).EQ.4) HELP=HELP-RGROUP*HCLAC(IWDG)*
     &                          (FLOAT(NPSTEP(JTML)+NMSTEP(JTML))+2.)
       ZDISC(IWDG)=HELP/(HPART1(IWDG)+HCLAC(IWDG))
       ZDISC(IWDG)=2.*ANINT(ZDISC(IWDG)/2.)
*
C... Extra turns
*
       IF (USURG(JTML).LE.380.) THEN
          EXTU(IWDG)=ZWIND(IWDG)*3./100.
       ELSE
          EXTU(IWDG)=ZWIND(IWDG)*4./100.
       END IF
*
C... Number of strands
*
       X1=1.
       IF (IPISOL(IWDG).GT.1) X1=2.
  480  ZPART(IWDG)=ACOND(IWDG)/RGROUP/
     &       (BPART(IWDG)*HPART(IWDG)-CORN(BPART(IWDG))/1.E+6)/X1
       ZPART(IWDG)=ANINT(ZPART(IWDG))
       IF (ZPART(IWDG).LT.1.) THEN
          X1=1.
          ZPART(IWDG)=1.
       END IF
       IF (X1.GT.2..OR.TSPIN(IWDG).LE.0.001.OR.ZPART(IWDG).LT.2.
     &        .OR.IPISOL(IWDG).GT.1)  GOTO 490
       X1=ANINT(X1+1.)
       GOTO 480
 490   IF (ZPART(IWDG).GT.8.) ZPART(IWDG)=8.
*
C...   ANTAL VARV/SKIVA
*
       ZTUDI(IWDG)=(ZWIND(IWDG)+EXTU(IWDG))*RGROUP/ZDISC(IWDG)
       ZTUDI(IWDG)=ANINT(ZTUDI(IWDG)*ZPART(IWDG))/ZPART(IWDG)
       IF (ZTUDI(IWDG).LT.2.)  ZTUDI(IWDG)=2.
*
       IF (KWITYP(IWDG).EQ.4)  THEN
  493     STAB=ZTUDI(IWDG)*ZPART(IWDG)/2.
          IF (STAB-AINT(STAB).GT.0.25) GOTO 495
          ZTUDI(IWDG)=ZTUDI(IWDG)+1./ZPART(IWDG)
          GOTO 493
  495     CONTINUE
       END IF
*
C...   ANTAL SKIVOR SLUTJUSTERAS
*
       ZDISC(IWDG)=ANINT((ZWIND(IWDG)+EXTU(IWDG))*RGROUP/ZTUDI(IWDG)+1.)
       ZDISC(IWDG)=2.*ANINT(ZDISC(IWDG)/2.)
*
C...   PARTDIM
*
       HPART(IWDG)=HELP/ZDISC(IWDG)-HCLAC(IWDG)-TSPIN(IWDG)
       BPART(IWDG)=(ACOND(IWDG)/RGROUP/ZPART(IWDG)/X1+
     &           CORN(BPART(IWDG))/1.E+6)/HPART(IWDG)
       IF (BBADJU) THEN
          HPART(IWDG)=ANINT(HPART(IWDG)*20000.)/20000.
          BPART(IWDG)=ANINT(BPART(IWDG)*20000.)/20000.
          ACOND(IWDG)=RGROUP*ZPART(IWDG)*X1*(HPART(IWDG)*BPART(IWDG)-
     &                CORN(BPART(IWDG))/1.E+6)
       END IF
       ZPART(IWDG)=ZPART(IWDG)*X1
*
C... X1 = Number of strands in common covering
*
       IF (X1.LE.1.) THEN
          IF(IPISOL(IWDG).LE.1)  THEN
             HELP1=0.0003
             IF(X1.GT.2..OR.HPART(IWDG).GT.0.012) HELP1=0.0005
             HELP1=TSPIN(IWDG)/FACINS-HELP1
             TSPIN1(IWDG)=0.0003
             IF(X1.GT.2..OR.HPART(IWDG).GT.0.012) TSPIN1(IWDG)=0.0005
             HELP1=TSPIN(IWDG)/FACINS-TSPIN1(IWDG)
          END IF
       END IF
       ZRR(IWDG)=ZPART(IWDG)
       GOTO 9000
 1000  CONTINUE
*
C...  THE EXACT FORMULAS FOR THE DISC WINDING DIMENSIONS
*
C      ERRMIN=1.E+3
C      IF (USURG(JTML).LE.380.) GOTO 1300
*
C...   NOGGRANN BER³KNING AV EXTRA VARV
*
C      C=2.
C      IEXT=1
C      IF (USURG(JTML).GT.500.) IEXT=IEXT+1
C      IF (USURG(JTML).GT.640.) IEXT=IEXT+1
C      IF (ZTUDI(IWDG).GT.6.5)  IEXT=IEXT+1
C      IF (ZTUDI(IWDG).GT.12.5) IEXT=IEXT+1
C      IF (ZTUDI(IWDG).GT.24.5) IEXT=IEXT+1
C      EXTU(IWDG)=EXT20(IEXT)*ANINT(0.2*ZTUDI(IWDG))+
C    &            EXT10(IEXT)*ANINT(0.1*ZTUDI(IWDG))
C      IF (.NOT.BBISGR) EXTU(IWDG)=2.*EXTU(IWDG)
C      IF (     BBISGR) EXTU(IWDG)=1.+EXTU(IWDG)
C      GOTO 1400
C1300  CONTINUE
C      C=2.
C      EXTU(IWDG)=2.
C1400  CONTINUE
*
C...   UTG·ENDE FR·N AVRUNDADE V³RDEN P· ANTAL PARTER OCH ANTAL
C...   VARV/SKIVA KONTROLLERAS 25 OLIKA KOMBINATIONER AV DESSA PARA-
C...   METRAR
C...   DEN KOMBINATION SOM GER DEN MINSTA TOTALA AVVIKELSEN FR·N
C...   FRAMOPTIMERADE V³RDEN V³LJS
*
C      ZPART0=ANINT(ZPART(IWDG))-3.
C      IF (ZPART0.GE.4..AND.ZPART0.LE.7.) ZPART0=3.
C      ZTUDI0=ANINT(ZTUDI(IWDG)*C)/C-3./C
C      USURGK=USURG(JTML)
*
C...   ANTAL PARTER STEGAS
*
C      DO 2000  L1=1,5
C         ZPART1=ZPART0+FLOAT(L1)
C         IF (ZPART1.LT.1.) GOTO 2000
C         X1=1.
C      IF (USURGK.LE.380.) GOTO 1500
C         IF (ZPART1.LT.2.) X1=1.
C         IF (ABS(AINT(ZPART1/3.)-ZPART1/3.).LT.0.01) X1=3.
C         IF (X1.EQ.3..AND.HPART(IWDG).GT.0.012) GOTO 1500
C         X1=1.
C         IF (ABS(AINT(ZPART1/2.)-ZPART1/2.).LT.0.01) X1=2.
C      IF (X1.EQ.0.) GOTO 2000
C1500     CONTINUE
C             X11=X1
*
C...   ANTAL VARV/SKIVA STEGAS
*
C         DO  1800 L2=1,5
C             X1=X11
C             ZTUDI1=ZTUDI0+FLOAT(L2)/C
C            IF (ZTUDI1.LT.0.5) GOTO 1800
*
C...   HOPSPUNNA PARTER KAN ENDAST ANV³NDAS TILLSAMMANS MED ETT HELT
C...   ANTAL VARV/SKIVA
*
C             IF (AINT(ZTUDI1).LT.ZTUDI1.AND.X1.EQ.2.)  X1=1.
*
C...   PRODUKTEN AV ZTUDI OCH ZPART M·STE VARA ETT HELTAL
*
C             IF (ZTUDI1*ZPART1+0.001-AINT(ZPART1*ZTUDI1/X1)*X1.GT.0.01)
C    &        GOTO 1800
C             ZDISC1=(ZWIND(IWDG)+EXTU(IWDG))*RGROUP/ZTUDI1
*
C...   AVRUNDNING UPP·T TILL J³MNT ANTAL SKIVOR
*
C             ZDISC1=ANINT(ZDISC1/(RGROUP*2.)+1.)*RGROUP*2.
*
C...   BER³KNING AV RESULTERANDE PARTHöJD
*
C             IF (KCODE(IWDG).EQ.1)
C    &        HPAR11=(HELP-(ZDISC1-1.)*HCLAC(IWDG))/ZDISC1-TSPIN(IWDG)
C             IF (KCODE(IWDG).EQ.4)
C    &        HPAR11=(HELP-(RGROUP*(FLOAT(NPSTEP(JTML)+NMSTEP(JTML))+
C    &               2.)+ZDISC1-1.)*HCLAC(IWDG))/ZDISC1-TSPIN(IWDG)
C             HPAR11=ANINT(HPAR11*1.E+4)/1.E+4
C             IF (USURGK.LE.380.) GOTO 1600
*
C...   HOPSPUNNA PARTER
*
C            HELP1=0.
C             IF (X1.EQ.3.) HELP1=TSPIN(IWDG)/FACINS-0.00050
C             IF (X1.EQ.2..AND.HPAR11.LE.0.012)
C    &        HELP1=TSPIN(IWDG)/FACINS-0.00030
C             IF (X1.EQ.2..AND.HPAR11.GT.0.012)
C    &        HELP1=TSPIN(IWDG)/FACINS-0.00050
C             IF(IPISOL(IWDG).EQ.1)
C    &                TSPIN1(IWDG)=TSPIN(IWDG)/FACINS-HELP1
C1600        CONTINUE
*
C...   BER³KNING AV RESULTERANDE PARTBREDD
*
C             FACT=(RRWDG(IWDG)-CDUCT)/(ZTUDI1*ZPART1)
C             IF (X1.LT.2.) BPART1=FACT-TSPIN(IWDG)/FACINS
C             IF (X1.GE.2.) BPART1=FACT-HELP1/X1-TSPIN1(IWDG)
C             IF (X1.GE.2.AND.IPISOL(IWDG).GT.1)
C    &        BPART1=(FACT-TSPIN(IWDG)-(X1-1.)*TSPIN1(IWDG)/FACINS)/X1
C             BPART1=ANINT(BPART1*20000.)/20000.
*
C...   BER³KNING AV RESULTERANDE LINDNINGSAREA
*
C             ACOND1=(HPAR11*BPART1-CORN(BPART1)/1.E+6)*ZPART1*RGROUP
*
C...   UPPSKATTNING AV AVVIKELSEN FR·N DE URSPRUNGLIGA DIMENSIONERNA
*
C             ERR=ABS(HPART(IWDG)-HPAR11)/HPART(IWDG)+
C    &            2.*ABS(BPART(IWDG)-BPART1)/BPART(IWDG)+
C    &            10.*ABS(ACOND(IWDG)-ACOND1)/ACOND(IWDG)
CC830201CCCC  IF (ERR.GT.ERRMIN) GOTO 1800
C      IF (X1.GT.1.AND.IPISOL(IWDG).EQ.1)
C    & RRWDGI=((BPART1  +TSPIN1(IWDG))*X1+HELP1)*ZPART1  /X1
C    & *ZTUDI1  +CDUCT
C      IF (X1.GT.1.AND.IPISOL(IWDG).GT.1)
C    & RRWDGI=(BPART1*X1+(X1-1.)*TSPIN1(IWDG)+TSPIN(IWDG)/FACINS)*
C    &       ZPART1/X1*ZTUDI1+CDUCT
C      IF (X1.LE.1.)
C    & RRWDGI=(BPART1  +TSPIN(IWDG)/FACINS)*ZPART1  *ZTUDI1  +CDUCT
C      RRWDGI=ANINT(RRWDGI*1.E+3+0.49)/1.E+3
C      IF (RRWDG(IWDG).LT.RRWDGI-1.)  ERR=ERR*2
C      IF (RRWDG(IWDG).GT.RRWDGI)  ERR=ERR*2
C             IF (ERR.GT.ERRMIN) GOTO 1800
*
C... RADERNA OVAN KOPIERADE FR·N 2880-2960
*
C...   DIMENSIONER SPARAS UNDAN
*
C             A1=ACOND1
C             A2=BPART1
C             A3=HPAR11
C             A4=ZDISC1
C             A5=ZPART1
C             A6=ZTUDI1
C            A7=TSPIN1(IWDG)
C            A8=X1
C         ERRMIN=ERR
C1800     CONTINUE
C2000  CONTINUE
*
C      ACOND(IWDG)=A1
C      BPART(IWDG)=A2
C      HPART(IWDG)=A3
C      ZDISC(IWDG)=A4
C      ZPART(IWDG)=A5
C      ZTUDI(IWDG)=A6
C      ZRR(IWDG)=ZPART(IWDG)
C      TSPIN1(IWDG)=A7
C      X1=A8
C      RPART(IWDG)=NINT(A8)
C      HELP1=TSPIN(IWDG)/FACINS-TSPIN1(IWDG)
*
C... BER³KNING AV LINDNINGENS BREDD OCH INNERDIAMETER
*
 9000  IF (X1.GT.1) THEN
          IF (IPISOL(IWDG).EQ.1) THEN
             RRWDG(IWDG)=((BPART(IWDG)+TSPIN1(IWDG))*X1+HELP1)*
     &                    ZPART(IWDG)/X1*ZTUDI(IWDG)+CDUCT
          ELSE IF (IPISOL(IWDG).GT.1) THEN
             RRWDG(IWDG)=(BPART(IWDG)*X1+(X1-1.)*TSPIN1(IWDG)+
     &             TSPIN(IWDG)/FACINS)*ZPART(IWDG)/X1*ZTUDI(IWDG)+CDUCT
          END IF
       ELSE
          RRWDG(IWDG)=(BPART(IWDG)+TSPIN(IWDG)/FACINS)*ZPART(IWDG)*
     &                ZTUDI(IWDG)+CDUCT
CCCCC  J=0
CCCCC  DO 1099 I=1,200
CCCCC  J=J+I
CCCCC  K=K+I
CCCCC  L=L+I
CCCCC  M=M+I
CCCCC  N=N+I
 1099  CONTINUE
       END IF
*
       IF (BBADJU) RRWDG(IWDG)=ANINT(RRWDG(IWDG)*1.E+5)/1.E+5
*
       IF (IWDG.EQ.1) THEN
          RINNER(IWDG)=DCORE/2.+BDUCT(IWDG)
       ELSE
          RINNER(IWDG)=RINNER(IWDG-1)+BDUCT(IWDG)+RRWDG(IWDG-1)
       END IF
*
       SWIND(IWDG)=(2.*RINNER(IWDG)+RRWDG(IWDG))*PI
       RPART(IWDG)=X1
*
       RETURN
       END
