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
*      Subroutine ADJUST
*      -----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 83-03-17 by H. Westberg  , ZKAB
*      Revised: 87-04-27 by Ron Bell     , TRAFO/IK
*
*  1.  Description
C...       Title:  Adjust the conductor dimensions
C...               to the nearest standard value.
*
************************************************************************
*
       SUBROUTINE ADJUST
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
     &  (XZ0791(1)    ,  FILLF  (1)    ),
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ0891(1)    ,  KCODE  (1)    ),
     &  (XZ0901(1)    ,  KGROUP (1)    )
       EQUIVALENCE
     &  (XZ0911(1)    ,  KWITYP (1)    ),
     &  (XZ0921(1)    ,  NGROUP (1)    ),
     &  (XZ0991(1)    ,  RPART  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ1091(1)    ,  TSPIN  (1)    )
       EQUIVALENCE
     &  (XZ1161(1)    ,  ZCODU  (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1287(1)    ,  USURG  (1)    ),
     &  (XZ1301(1)    ,  BBCURD (1)    ),
     &  (XZ1331(1)    ,  BBRR   (1)    )
       EQUIVALENCE
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1513       ,  PI            ),
     &  (XZ2554(1)    ,  NLORR  (1)    )
       EQUIVALENCE
     &  (XZ2555(1)    ,  KWIND  (1)    ),
     &  (XZ2563       ,  BBADJU        ),
     &  (XZ2581(1)    ,  ZLAG   (1)    ),
     &  (XZ2582(1)    ,  ZNLAG  (1)    ),
     &  (XZ2583(1)    ,  ZCOIAR (1)    )
       EQUIVALENCE
     &  (XZ2584(1)    ,  EXTRAR (1)    ),
     &  (XZ2585(1)    ,  HCLAC  (1)    ),
     &  (XZ2586(1)    ,  ZDISC  (1)    ),
     &  (XZ2589(1)    ,  SWIND  (1)    ),
     &  (XZ2590(1)    ,  RINNER (1)    )
       EQUIVALENCE
     &  (XZ2592(1)    ,  ZRR    (1)    ),
     &  (XZ2593(1)    ,  ZPART  (1)    ),
     &  (XZ2594(1)    ,  ZTUDI  (1)    ),
     &  (XZ2596(1)    ,  TSPIN1 (1)    )
*
       REAL       ACOND(9),BDUCT(9),BPART(9),DYOKE(9),FILLF(9),
     &            HPART(9),HWIND(9),RRWDG(9),TSPIN(9),USURG(4),
     &            ZCODU(9),ZWIND(9),ZNLAG(9),ZLAG(9),ZCOIAR(9),
     &            HCLAC(9),ZDISC(9),SWIND(9),RINNER(9),RPART(9),
     &            ZRR(9),ZPART(9),ZTUDI(9),TSPIN1(9),EXTRAR(9),
     &            DCORE,HLIMB,PI,Q,RGROUP,X1,ZPART1
*
       INTEGER    KGROUP(9),NGROUP(9),KCODE(9),IPISOL(9),NDISC(9),
     &            NLORR(9),KWITYP(9),KWIND(9),IWDG,KW,NWILI
*
       LOGICAL    BBCURD(9),BBRR(9),BBADJU
*
       EXTERNAL   CORN
*
       print*,'Inside adjust.f'
       BBADJU=.TRUE.
       HLIMB=ANINT(HLIMB*1000.)/1000.
       write(*,*) ' adjust line 100', HLIMB
*
       DO 9900  IWDG=1,NWILI
       HWIND(IWDG)=HLIMB-DYOKE(IWDG)
       RGROUP=FLOAT(NGROUP(IWDG))
       USURGK=USURG(KGROUP(IWDG))
*
C... Disc winding
*
       IF (KWIND(IWDG).NE.0.AND.KWIND(IWDG).NE.100) GOTO 4000
*
       IF (.NOT.BBRR(IWDG)) CALL CONTD(IWDG)
*
       ZPART1=ZPART(IWDG)
       X1=1.
       IF (USURGK.LE.380.) GOTO 2000
       IF (ABS(AINT(ZPART1/3.)-ZPART1/3.).LT.0.01) X1=3.
       IF (X1.EQ.3..AND.HPART(IWDG).GT.0.012) GOTO 2000
       X1=1.
       IF (ABS(AINT(ZPART1/2.)-ZPART1/2.).LT.0.01) X1=2.
 2000  CONTINUE
       IF (AINT(ZTUDI(IWDG)).LT.ZTUDI(IWDG).AND.
     &     AINT(ZPART1/(X1*2.)).LT.ZPART1/(X1*2.)) X1=1.
       HELP1=0.
       IF (X1.EQ.1.) GOTO 2500
       IF (X1.EQ.3.) HELP1=TSPIN(IWDG)/0.95-0.0005
       IF (X1.EQ.3.) GOTO 2500
       IF (HPART(IWDG).GT.0.012) HELP1=TSPIN(IWDG)/0.95-0.0005
       IF (HPART(IWDG).LE.0.012) HELP1=TSPIN(IWDG)/0.95-0.0003
 2500  CONTINUE
       TSPIN1(IWDG)=TSPIN(IWDG)/0.95-HELP1
       IF (IPISOL(IWDG).EQ.2) TSPIN1(IWDG)=0.15E-3
       IF (IPISOL(IWDG).EQ.3) TSPIN1(IWDG)=0.18E-3
       IF (.NOT.BBRR(IWDG))
     & RRWDG(IWDG)=((BPART(IWDG)+TSPIN1(IWDG))*X1+HELP1)*
     &              ZPART1/X1*ZTUDI(IWDG)+ZCODU(IWDG)*0.005
       IF (.NOT.BBRR(IWDG).AND.IPISOL(IWDG).GE.2.AND.
     &                      IPISOL(IWDG).LE.3)
     & RRWDG(IWDG)=(BPART(IWDG)*X1+TSPIN1(IWDG)*(X1-1.)+TSPIN(IWDG))*
     &              ZPART1/X1*ZTUDI(IWDG)+ZCODU(IWDG)*0.005
       RPART(IWDG)=NINT(X1)
       GOTO 8900
*
C... Screw
*
 4000  IF (KWIND(IWDG).GT.6) GOTO 6000
       NDISC(IWDG)=NINT(ZDISC(IWDG))
       IF (.NOT.BBRR(IWDG))  GOTO 8900
       Q=1.
       IF (KWIND(IWDG).EQ.1.AND.HCLAC(IWDG).LT.0.0025) Q=2.
       IF (KWIND(IWDG).GT.1) Q=0.
*
       IF (.NOT.BBRR(IWDG)) THEN
          ZRR(IWDG)=ANINT(ZRR(IWDG))
          ZPART(IWDG)=ZRR(IWDG)*FLOAT(KWIND(IWDG))
          BPART(IWDG)=ACOND(IWDG)/(HPART(IWDG)*ZPART(IWDG))/RGROUP
          IF (BPART(IWDG).GT.0.003) ZRR(IWDG)=ZRR(IWDG)+1.
          ZPART(IWDG)=ZRR(IWDG)*FLOAT(KWIND(IWDG))
       END IF
*
       KW=KWIND(IWDG)
       ZDISC(IWDG)=(ZWIND(IWDG)+1.)*RGROUP*KW
       NDISC(IWDG)=NINT(ZDISC(IWDG))
       ZDISC(IWDG)=FLOAT(NDISC(IWDG))
       IF (BBRR(IWDG)) GOTO 8900
       HPART(IWDG)=((HWIND(IWDG)-EXTRAR(IWDG))/RGROUP-
     &              KW*ZWIND(IWDG)*HCLAC(IWDG)-HCLAC(IWDG)*
     &              (ZRR(IWDG)+3.)*Q-(KW-1.)*HCLAC(IWDG))/
     &              (KW*(ZWIND(IWDG)+1.))-TSPIN(IWDG)
       IF (.NOT.BBRR(IWDG)) THEN
          HPART(IWDG)=ANINT(HPART(IWDG)*20000.)/20000.
          BPART(IWDG)=(ACOND(IWDG)/ZPART(IWDG)/RGROUP+
     &                 CORN(BPART(IWDG))*1.E-6)/HPART(IWDG)
          BPART(IWDG)=ANINT(BPART(IWDG)*20000.)/20000.
       END IF
       RRWDG(IWDG)=(BPART(IWDG)+TSPIN(IWDG)/0.95)*ZRR(IWDG)+
     &              ZCODU(IWDG)*0.005
       GOTO 8900
6000   CONTINUE
*
C... Sling
*
       IF (KCODE(IWDG).NE.2) THEN
          IF (KWIND(IWDG).GT.10.AND.KWIND(IWDG).LT.40) GOTO 9000
       END IF
*
       IF (.NOT.BBRR(IWDG)) THEN
          HPART(IWDG)=ANINT(HPART(IWDG)*20000.)/20000.
          BPART(IWDG)=(ACOND(IWDG)/ZPART(IWDG)+
     &                 CORN(BPART(IWDG))*1.E-6)/HPART(IWDG)/RGROUP
          BPART(IWDG)=ANINT(BPART(IWDG)*20000.)/20000.
          ZLAG(IWDG)=1.
          ZCOIAR(IWDG)=1.
          ZNLAG(IWDG)=ZWIND(IWDG)
          RRWDG(IWDG)=FLOAT(NLORR(IWDG))*(BPART(IWDG)+TSPIN(IWDG)/0.95)
       END IF
*
 8900  IF (.NOT.BBRR(IWDG)) ACOND(IWDG)=ZPART(IWDG)*
     &         (HPART(IWDG)*BPART(IWDG)-CORN(BPART(IWDG))*1.E-6)*RGROUP
       RRWDG(IWDG)=ANINT(RRWDG(IWDG)*1000.+0.49)/1000.
*
       IF (IWDG.EQ.1) THEN
          RINNER(IWDG)=DCORE/2.+BDUCT(IWDG)
       ELSE
          RINNER(IWDG)=RINNER(IWDG-1)+BDUCT(IWDG)+RRWDG(IWDG-1)
       END IF
*
       SWIND(IWDG)=(2.*RINNER(IWDG)+RRWDG(IWDG))*PI
*
 9000  FILLF(IWDG)=ACOND(IWDG)*ZWIND(IWDG)/(HWIND(IWDG)*RRWDG(IWDG))
*
 9900  CONTINUE
*
       print*,'coming out of adjust.f'
       RETURN
       END
