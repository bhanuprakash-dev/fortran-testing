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
*      Subroutine COR2
*      ---------------
*
C...   Title:  Calculation of side-limb cores
*
*      Written: XX-XX-XX by A.N.Other      , XXXX
*      Revised: 85-10-18 by Ron Bell       , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 89-10-02 by B-G Bladh      , SETFO/K1
*      Revised: 90-02-12 by Per S�derberg  , SETFO/IK
*      Revised: 92-04-15 by B-G Bladh      , SETFO/TS
*
************************************************************************
*
       SUBROUTINE COR2(BLIMB,BMAXPU,CORBND,DCORE,DOUTW,DPHSL,FEXTV,
     &                 FNEXTV,FREQ,GACTP,HLIMB,ILACK,JFC,KCOOL,
     &                 KCORE,NPHAS,PLIMB,PLSL,QFINAL,TTOILM,
     &                 TTOILC,TWSUP,TYPCOR,YHADD,
     &                 ACORE,ANDEL,BDRAG,DKSL,GCORN,GLIMBM,GLIMBY,
     &                 GYOKE,HYOKE,KBAND,NCOOLC,RDRAG,RLYOKE,SBF,
     &                 TASEC,TCORE,TDRAG,U0,YOKAMP,CORESE,CORTYP,
     &                 BBSTLA,ISTGRD,NCOOLI,NCOOLW)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       CHARACTER CORBND*8, CORESE*4, CORTYP*4
*
       REAL      COM(110),RNP(110),B1(0:110),B2(110),B3(110),
     &           RK1(110),RK2(110),TP(0:110),TS(110),ACORE,ALT,ANDEL,
     &           BDRAG,BEL,BLIMB,BMAXPU,BT,B1MAX,B1MIN,DCORE,DKSL,DN,
     &           DOUTW,DPHSL,FACTOR,FEXTV,FNEXTV,FREQ,GACTP,GCORN,
     &           GLIMBM,GLIMBY,GYOKE,HLIMB,HYOKE,PLIMB,PLSL,PRMAX,
     &           QFINAL,RDRAG,RLYOKE,SBF,TASEC,TCORE,TDRAG,TEMP,TJPL,
     &           TKAN,TT,TTOILC,TTOILM,TWSUP,TYPCOR,T1,T2,UTURN0,U0,
     &           YHADD,YOKAMP
*
       INTEGER   ILACK,JFC,KBAND,KCOOL,KCORE,NCOOLC,NPHAS,NPKT,ISTGRD,
     &           NCOOLI, NCOOLW
*
       LOGICAL   BBFALS,BBSTLA
*
       BBFALS=.FALSE.
       FACTOR=1.E+3
*
       DO 9 I=1,110
       COM(I)=0.
       RNP(I)=0.
       B2(I)=0.
       B3(I)=0.
       RK1(I)=0.
       RK2(I)=0.
       TS(I)=0.
    9  CONTINUE
*
       DO 19 I=0,110
       B1(I)=0.
       TP(I)=0.
   19  CONTINUE
*
CC*SEBL End of the declarations block.
*
       KBAND=1
       IF(HLIMB.GT.4) KBAND=2
       IF(CORBND(1:1).EQ.'S') KBAND=2
       IF(KCORE.EQ.1.OR.KCORE.EQ.6) KBAND=0
       COM(97)=KBAND
*
C... Choose calculation based on core type
*
C,,, KCORE.NE.4
*
       IF(KCORE.NE.4) THEN
          WRITE(*,*) 'KCORE.NE.4'
          COM(7)=5.
          COM(34)=24.
          IF(ISTGRD.EQ.3) COM(34)=32.
          IF(ISTGRD.EQ.4) COM(34)=32.
*
C... Calculate flitch plates
*
          CALL DRAG1(TYPCOR,QFINAL/FACTOR,GACTP,RDRAG,
     &    BDRAG,TDRAG,ALT,BEL,PRMAX)
*
C... Calculate flitch plates
*
C,,, Repeat until PRMAX.LT.0.01
*
  300     IF(PRMAX.LT.0.01) GOTO 399
          QFINAL=PRMAX*0.999E+3
          WRITE(*  ,80) QFINAL/1.E+6
          WRITE(JFC,80) QFINAL/1.E+6
*
C... Calculate flitch plates
*
          CALL DRAG1(TYPCOR,QFINAL/FACTOR,GACTP,RDRAG,
     &                BDRAG,TDRAG,ALT,BEL,PRMAX)
          GOTO 300
  399     CONTINUE
*
C... KCORE.EQ.4
*
       ELSE
              WRITE(*,*) 'KCORE.EQ.4'
          TEMP=TTOILM+20.
          IF(KCOOL.EQ.3) TEMP=10.+TTOILM
          IF(TTOILC.GT.0.1) TEMP=30.+TTOILC
          NCOOLW=NCOOL(CORESE,TYPCOR,DCORE,TEMP,BLIMB*BMAXPU,FREQ,
     &                 BBSTLA,JFC,ISTGRD)
          IF (NCOOLI.LT.0) THEN
              NCOOLC = NCOOLW
          ELSE
              NCOOLC = NCOOLI
          ENDIF
          WRITE (*,*) '#NCOOLC = ', NCOOLC
*
          T1=0.
          TKAN=0.
          IF(TYPCOR.GT.6.) TKAN=6.
*
C... Indata to CORE
*
          CALL PRECOR(ANINT(DCORE*FACTOR),ANINT(HLIMB*FACTOR),KCORE,
     &             0.8,DN,B1MAX,B1MIN,T1,T2,
     &             BT,TT,BBFALS,RDRAG,BDRAG,TDRAG,KBAND,2000.,10.,
     &             TYPCOR,TKAN)
*
          TASEC=T1/FACTOR
          COM(7)=4.
          COM(8)=NCOOLC
          COM(9)=ANINT(YHADD*FACTOR)
          COM(11)=DN
          COM(12)=B1MIN
          COM(13)=B1MAX
          COM(34)=24.
          IF(ISTGRD.EQ.3 ) COM(34)=32.
          IF(ISTGRD.EQ.4 ) COM(34)=32.
       END IF
*
C... LTB & TCA-cores
*
       COM(1)=1.
       COM(2)=ANINT(TYPCOR)
       COM(3)=ANINT(DCORE*FACTOR)
       COM(4)=ANINT(PLIMB*FACTOR)
       COM(5)=ANINT(PLSL*FACTOR)
       COM(6)=ANINT(HLIMB*FACTOR)
       COM(14)=FEXTV
       COM(15)=ILACK
       COM(16)=1.732
       COM(21)=ALT
       COM(22)=ANINT(TWSUP*FACTOR)
       COM(33)=10.
       COM(36)=RDRAG
       COM(37)=BDRAG
       COM(38)=TDRAG
       COM(41)=6.
       COM(43)=FNEXTV
       TJPL=0.3
       IF(ISTGRD.EQ.3 ) TJPL=0.23
       IF(ISTGRD.EQ.4 ) TJPL=0.23
       COM(42)=TJPL*COM(43)
       COM(48)=0.
       COM(71)=0.
       COM(95)=0.8
       COM(99)=0.
*
C... Calculate the core section
*
       CALL CORE2(COM,RNP,B1,B2,B3,RK1,RK2,TP,TS)
       DKSL=COM(50)/FACTOR
       PLSL=(DOUTW+DKSL)/2.+DPHSL+TASEC
       COM(5)=ANINT(PLSL*FACTOR)
*
C... Calculate the core section
*
       CALL CORE2(COM,RNP,B1,B2,B3,RK1,RK2,TP,TS)
       NPKT=NINT(COM(53))
*
C... Calculate the proportion of varnished laminations
C... This is always zero as we no longer use varnished core steel
*
CCCCC  CALL LANDEL(TYPCOR,ANINT(DCORE*FACTOR),ILACK,NPKT,B1,TP,ANDEL)
       ANDEL=0.
*
       UTURN0=COM(44)
       U0=UTURN0*FREQ/50.
       ACORE=COM(46)/1.E+4
       RLYOKE=COM(93)/FACTOR
       HYOKE=COM(91)/FACTOR
       GYOKE=RLYOKE*FACTOR*COM(57)+COM(58)
       TASEC=COM(71)/FACTOR
       TCORE=COM(92)/FACTOR
       SBF=COM(76)
       GLIMBM=COM(77)
       GLIMBY=COM(78)
       WRITE(*,*) 'COR2.F GCORN', GCORN
       GCORN=COM(90)
       WRITE(*,*) 'COR2.F COM(90)', COM(90)
       NCOOLC=COM(87)
       YOKAMP=COM(47)
       BDRAG=BDRAG/FACTOR
       TDRAG=TDRAG/FACTOR
*
       RETURN
   80  FORMAT('0',70('=')/'0',' Assy press reduced to',
     &        F8.2,' MN'/'0',70('='))
       END
