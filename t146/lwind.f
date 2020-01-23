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
*      Subroutine LWIND
*      ----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 83-08-01 by H.Westberg   , ZKAB
*      Revised: 87-11-19 by Ron Bell     , TRAFO/IK
*
*  1.  Description
C...       Title:  Winding layout for Layer windings
C...   BBARRR = .TRUE.  => BOTH ZAR AND ZRR GIVEN TROUGH INPUT DATA
C...   ZPART = TOTAL NUMBER OF PARTS
C...   ZAR = NUMBER OF AXIAL STRANDS
C...   ZRR = NUMBER OF RADIAL STRANDS
C...   HWIND = WINDING HEIGHT
C...   HSPOL  = COIL HEIGHT
C...   ULAG   = VOLTAGE PER LAYER
C...   ZLAG   = NUMBER OF LAYERS
C...   ZNLAG  = NUMBER OF TURNS PER LAYER
C...   HPARTO &  BPARTO INCOMING PART DIMENSIONS
C...   R      = WINDING INNER DIAMETER IN DIRECTION OF TANK-LENGTH
C...   RINNET = WINDING INNER DIAMETER     IN DIRECTION OF TANK-BREADTH
C...   RR     = RADIAL DIMENSION OF WINDING IN DIRECTION OF TANK-LENGTH
C...   RRTADD = RADIAL ADDITION OF WINDING IN DIRECTION OF TANK-BREADTH
C...   TRR    = RADIAL THICKNESS PER CABLE
C...   SWIND  = WINDING MEAN CIRCUMFERENCE
C...   DSWI = ACCUMULATED INCREASE OF CIRCUMFERENCE. CROSSOVERS
C...          AFFECT BOTH PRESENT DSWI AND DSWI FOR OUTER WINDINGS.
C...          INCREASE OF DSWI FOR TAPPINGS AFFECT ONLY OUTER WINDINGS.
C...          THEREFORE SWIND IS CALCULATED AFTER CROSSOVERS BUT BEFORE
C...          TAPPINGS
C...   RNX = NUMBER OF TURNS WITH CROSSOVERS
C...   NUMBER OF CROSSOVERS = ZLAG - 1
*
************************************************************************
*
       SUBROUTINE LWIND (IWDG)
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0671(1)    ,  BDUCT  (1)    ),
     &  (XZ0681(1)    ,  BPART  (1)    ),
     &  (XZ0721(1)    ,  IPISOL (1)    ),
     &  (XZ0751(1)    ,  DYOKE  (1)    )
       EQUIVALENCE
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ0891(1)    ,  KCODE  (1)    ),
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ0921(1)    ,  NGROUP (1)    ),
     &  (XZ0931(1)    ,  NLOOP  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ1091(1)    ,  TSPIN  (1)    )
       EQUIVALENCE
     &  (XZ1161(1)    ,  ZCODU  (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1239(1)    ,  PUSTEP (1)    ),
     &  (XZ1331(1)    ,  BBRR   (1)    ),
     &  (XZ1405       ,  BLIMB         ),
     &  (XZ1426       ,  DCORE         )
       EQUIVALENCE
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1513       ,  PI            ),
     &  (XZ1559       ,  U0            ),
     &  (XZ1656(1,1)  ,  POSIT  (1,1)  ),
     &  (XZ2298(1)    ,  NCOL   (1)    ),
     &  (XZ2555(1)    ,  KWIND  (1)    ),
     &  (XZ2563       ,  BBADJU        ),
     &  (XZ2581(1)    ,  ZLAG   (1)    ),
     &  (XZ2582(1)    ,  ZNLAG  (1)    ),
     &  (XZ2583(1)    ,  ZCOIAR (1)    )
       EQUIVALENCE
     &  (XZ2584(1)    ,  EXTRAR (1)    ),
     &  (XZ2585(1)    ,  HCLAC  (1)    ),
     &  (XZ2589(1)    ,  SWIND  (1)    ),
     &  (XZ2590(1)    ,  RINNER (1)    )
       EQUIVALENCE
     &  (XZ2591(1)    ,  ZAR    (1)    ),
     &  (XZ2592(1)    ,  ZRR    (1)    ),
     &  (XZ2593(1)    ,  ZPART  (1)    ),
     &  (XZ2595(1)    ,  ZTULO  (1)    ),
     &  (XZ2596(1)    ,  TSPIN1 (1)    )
*
       REAL      HPARTO(9),BPARTO(9),TEST(9),PUSTEP(4),HLIMB,DCORE,
     &           BLIMB,U0,ACOND(9),ZCODU(9),ZWIND(9),HWIND(9),PI,
     &           DYOKE(9),HCLAC(9),EXTRAR(9),SWIND(9),TSPIN(9),RNX,
     &           TSPIN1(9),ZTULO(9),BDUCT(9),ZCOIAR(9),ZAR(9),RRX,
     &           ZRR(9),ZPART(9),BPART(9),HPART(9),POSIT(9,4),TURNH,
     &           ZNLAG(9),ZLAG(9),RINNER(9),RRWDG(9),DSWI(9),ULAG,
     &           HSPOL(9),RINNET(9),RREX,TRR,APART,BLED,COIPG,XL,X1,X2,
     &           X3LAY(9),X4LAY(9),RRTADD(9),XLAY(9),HELP,HELP1,HELP2,
     &           ZLOOP,ZNSPOL,ZREG
*
       INTEGER   KGROUP(9),KWIND(9),NLOOP(9),IPISOL(9),KCODE(9),
     &           NGROUP(9),IHV,IWDG,JTML,JFC,NWARN
*
       LOGICAL BBADJU,BBRR(9),BBARRR(9)
*
       DIMENSION NCOL(9)
       CHARACTER NCOL*1
*
       IHV=0
       X3LAY(IWDG)=0.
       X4LAY(IWDG)=0.
       RRTADD(IWDG)=0.
       BBARRR(IWDG)=.FALSE.
       XLAY(IWDG)=0.
*
       HPARTO(IWDG)=HPART(IWDG)
       BPARTO(IWDG)=BPART(IWDG)
       ZLOOP=FLOAT(NLOOP(IWDG))
       HWIND(IWDG)=HLIMB-DYOKE(IWDG)
       HSPOL(IWDG)=(HWIND(IWDG)-(ZCOIAR(IWDG)-1.)*
     &              EXTRAR(IWDG))/ZCOIAR(IWDG)
       RGROUP=FLOAT(NGROUP(IWDG))
       COIPG=ZCOIAR(IWDG)/RGROUP
       ZNSPOL=ZWIND(IWDG)/COIPG
*
       X1=1.
       X2=0.5
*
       IF (KWIND(IWDG).EQ.13.OR.KWIND(IWDG).EQ.14) THEN
          X1=3./2.
       ELSE IF (KWIND(IWDG).EQ.15.OR.KWIND(IWDG).EQ.16) THEN
          X1=5./3.
       ELSE IF (KWIND(IWDG).EQ.17.OR.KWIND(IWDG).EQ.18) THEN
          X1=7./4.
       ELSE IF (KWIND(IWDG).GE.20) THEN
          X2=1.
       END IF
*
       HELP1=(HCLAC(IWDG)*X1+TSPIN(IWDG))*1.E+3
       ULAG=3.E+3*HELP1**0.70*X2
       HELP2=ULAG
       HELP=2.5E+3*HELP1*X2
       IF (HELP.GT.ULAG) ULAG=HELP
CCCCC  IF (.NOT.BBRR(IWDG)) GOTO 302
       IF (BBRR(IWDG)) GOTO 302
       ZLAG(IWDG)=ANINT(ZNSPOL*U0*BLIMB/ULAG)+1.
  200  IF (BBARRR(IWDG)) THEN
          ZNLAG(IWDG)=(ZNSPOL+(ZLOOP*0.005/COIPG+XLAY(IWDG))/
     &          (HPART(IWDG)+TSPIN(IWDG))/1.01/ZAR(IWDG))/ZLAG(IWDG)-1.
          GOTO 299
       END IF
*
  298  ZNLAG(IWDG)=ZNSPOL/(ZLAG(IWDG)-(ZLOOP*0.005/COIPG+XLAY(IWDG))
     &              /HSPOL(IWDG))-1.
       ZAR(IWDG)=HSPOL(IWDG)/(ZNLAG(IWDG)+1.)/
     &           (HPARTO(IWDG)+TSPIN(IWDG))/RGROUP/1.01
       ZAR(IWDG)=ANINT(ZAR(IWDG)+0.49)
       IF (ZAR(IWDG).LT.1.) ZAR(IWDG)=1.
  299  CONTINUE
       HPART(IWDG)=HSPOL(IWDG)/ZAR(IWDG)/(ZNLAG(IWDG)+1.)/
     &             1.01/RGROUP-TSPIN(IWDG)
       IF (BBADJU) HPART(IWDG)=ANINT(HPART(IWDG)*2.E+4)/2.E+4
*
       IF (.NOT.BBARRR(IWDG)) THEN
          APART=HPART(IWDG)*BPARTO(IWDG)-CORN(BPARTO(IWDG))/1.E+6
          ZRR(IWDG)=ACOND(IWDG)/APART/ZAR(IWDG)
          IF (IPISOL(IWDG).GT.1) ZRR(IWDG)=ZRR(IWDG)/2.
          ZRR(IWDG)=ANINT(ZRR(IWDG))
          IF (ZRR(IWDG).LT.1.) ZRR(IWDG)=1.
          IF (IPISOL(IWDG).GT.1) ZRR(IWDG)=ZRR(IWDG)*2.
       END IF
*
       BPART(IWDG)=(ACOND(IWDG)/ZAR(IWDG)/ZRR(IWDG)+
     &           CORN(BPARTO(IWDG))/1.E+6)/HPART(IWDG)
       ZPART(IWDG)=ZAR(IWDG)*ZRR(IWDG)
       IF (.NOT.BBARRR(IWDG)) THEN
          IF (HPART(IWDG).GE.HPARTO(IWDG)/2.) GOTO 303
          ZLAG(IWDG)=ZLAG(IWDG)+1.
          GOTO 298
       END IF
       IF (BPART(IWDG).LT.BPARTO(IWDG)*2..AND.
     &     HPART(IWDG).GT.HPARTO(IWDG)*0.8) GOTO 303
       ZLAG(IWDG)=ZLAG(IWDG)+1.
       GOTO 200
*
C... Fixed dimensions
*
  302  IF (BBADJU) HPART(IWDG)=ANINT(HPART(IWDG)*2.E+4)/2.E+4
       TURNH=(HPART(IWDG)+TSPIN(IWDG))*1.01*ZAR(IWDG)
       ZNLAG(IWDG)=HSPOL(IWDG)/TURNH-1.
       TRR=(BPART(IWDG)+TSPIN(IWDG))*ZRR(IWDG)+HCLAC(IWDG)
*
       IF (IPISOL(IWDG).GT.1) TRR=(2.*BPART(IWDG)+
     &           TSPIN1(IWDG)+TSPIN(IWDG))*ZRR(IWDG)/2.+HCLAC(IWDG)
       ZLAG(IWDG)=((RRWDG(IWDG)-ZCODU(IWDG)*4.0/1.E+3)/1.03+
     &             HCLAC(IWDG))/TRR
       IF (ZLAG(IWDG).LT.1.) ZLAG(IWDG)=1.
       IF (ZLAG(IWDG)*ZNLAG(IWDG).LT.ZNSPOL) THEN
          NWARN=0
          CALL WMESS
     &    (JFC,' Layer winding '//NCOL(IWDG)//
     &                    ' - data not consistent.           ',NWARN)
       END IF
  303  HELP2=ZNSPOL*U0*BLIMB/ZLAG(IWDG)
*
       IF (KCODE(IWDG).NE.2) THEN
          TEST(IWDG)=HELP2/ULAG
       ELSE
          TEST(IWDG)=0.
       END IF
*
       IF (TEST(IWDG).GT.1..AND.BBADJU) THEN
          WRITE (6,6) IWDG,HELP2,ULAG
          NWARN=0
          CALL WMESS
     &    (JFC,' Layer winding '//NCOL(IWDG)//
     &                    ' - layer voltage too high.        ',NWARN)
       END IF
*
       IF(BBADJU) THEN
          BPART(IWDG)=ANINT(BPART(IWDG)*2.E+4)/2.E+4
          APART=HPART(IWDG)*BPART(IWDG)-CORN(BPART(IWDG))/1.E+6
          ACOND(IWDG)=APART*ZPART(IWDG)*RGROUP
       END IF
*
       TRR=(BPART(IWDG)+TSPIN(IWDG))*ZRR(IWDG)+HCLAC(IWDG)
*
       IF (IPISOL(IWDG).GT.1)
     &         TRR=(2.*BPART(IWDG)+TSPIN1(IWDG)+TSPIN(IWDG))*
     &             ZRR(IWDG)/2.+HCLAC(IWDG)
       IF (.NOT.BBRR(IWDG)) RRWDG(IWDG)=
     &    (ZLAG(IWDG)*TRR-HCLAC(IWDG))*1.03+ZCODU(IWDG)*4.0/1.E+3
       IF (IWDG.EQ.1) THEN
          RINNER(1)=DCORE/2.+BDUCT(1)
          RINNET(1)=DCORE/2.+BDUCT(1)
       ELSE
          RINNER(IWDG)=RINNER(IWDG-1)+RRWDG(IWDG-1)+BDUCT(IWDG)
          RINNET(IWDG)=RINNET(IWDG-1)+RRWDG(IWDG-1)+
     &             RRTADD(IWDG-1)+BDUCT(IWDG)
       END IF
*
       RRTADD(IWDG)=0.
       DSWI(IWDG)=0.
       IF (IWDG.GT.1) DSWI(IWDG)=DSWI(IWDG-1)
       IF(KWIND(IWDG).GE.20) THEN
          BLED=(HPART(IWDG)+TSPIN(IWDG))*ZAR(IWDG)
          RNX=(ZLAG(IWDG)-1.)*(BLED+0.06)/
     &        (2.*RINNER(IWDG)+RRWDG(IWDG))*1.5/PI
          IF(BBADJU) RNX=ANINT(RNX*2.+0.49)/2.
          RRX=RNX*(TRR+2.*X3LAY(IWDG))
          RRTADD(IWDG)=RRX
          DSWI(IWDG)=DSWI(IWDG)+0.667*RRX*PI
       END IF
*
       SWIND(IWDG)=(2.*RINNER(IWDG)+RRWDG(IWDG))*PI+DSWI(IWDG)
*
C...   ANTAL VARV REGLEROMR·DET PER PARALLELL GRUPP= ZREG
C...   ANTAL LAGER SOM KR³VS FöR ATT RYMMA HELA REGLERDELEN=XL
C...   öKNING AV YTTRE OMKRETS P.G.A UTTAG= DSWI(IWDG)
*
       IF(KCODE(IWDG).NE.1.AND.KCODE(IWDG).NE.3) THEN
          JTML=KGROUP(IWDG)
*
          IF(KCODE(IWDG).NE.4) THEN
             XL=ZLAG(IWDG)
             ZREG=ZWIND(IWDG)
          ELSE
             ZREG=ZLOOP*PUSTEP(JTML)*ZWIND(IWDG)*
     &               POSIT(IWDG,1)/POSIT(IWDG,2)
             XL=ZREG/ZNLAG(IWDG)+0.5
             IF (BBADJU) XL=ANINT(XL+0.49)
          END IF
          ZTULO(IWDG)=ZREG/ZLOOP
          IF(KGROUP(IWDG).EQ.IHV) THEN
             RREX=XL*(TSPIN(IWDG)+HCLAC(IWDG)+X4LAY(IWDG))
             RRTADD(IWDG)=RRTADD(IWDG)+RREX
             DSWI(IWDG)=DSWI(IWDG)+0.4*RREX
          END IF
       END IF
*
       RETURN
    6  FORMAT (1X,'LWIND',I4,' Layer voltages:  ACT=',F10.0,' MAX=',
     &         F10.0,' V')
       END
