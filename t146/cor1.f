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
*      Subroutine COR1
*      ---------------
*
C...   Title:  Calculation of cores without side-limbs
*
*      Written: XX-XX-XX by A.N.Other      , XXXX
*      Revised: 85-10-18 by Ron Bell       , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 89-10-02 by B-G Bladh      , SETFO/K1
*      Revised: 90-02-12 by Per S�derberg  , SETFO/IK
*      Revised: 92-04-14 by B-G Bladh      , SETFO/TS
*
************************************************************************
*
       SUBROUTINE COR1(BLIMB,BMAXPU,CORBND,DCORE,FEXTV,FNEXTV,FREQ,
     &                 HLIMB,ILACK,JFC,KCOOL,KCORE,NPHAS,NWOULI,PLIMB,
     &                 TTOILM,TTOILC,TWSUP,TYPCOR,UMAXPU,YHADD,
     &                 YHRED,
     &                 ACORE,ANDEL,BDRAG,GCORN,GLIMBM,GLIMBY,GYOKE,
     &                 HYOKE,KBAND,NCOOLC,RDRAG,RLYOKE,TASEC,TCORE,
     &                 TDRAG,U0,YOKAMP,CORESE,CORTYP,BBSTLA,ISTGRD,
     &                 NCOOLI,NCOOLW)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL      COM(50),RNP(100),B1(0:100),B3(100),RK1(100),TP(0:100),
     &           TS(100),FACTOR,ACORE,ANDEL,BDRAG,BHELP,BLIMB,BMAXPU,
     &           BT,B1MAX,B1MIN,DCORE,DN,FEXTV,FNEXTV,FREQ,GCORN,
     &           GLIMBM,GLIMBY,GYOKE,HLIMB,HYOKE,PLIMB,RDRAG,RLYOKE,
     &           TASEC,TCORE,TDRAG,TEMP,TJPL,TKAN,TT,TTOILC,TTOILM,
     &           TYPCOR,T1,T2,UMAXPU,UTURN0,U0,YHADD,YHRED,YOKAMP
*
       INTEGER   ILACK,JFC,KBAND,KCOOL,KCORE,NCOOLC,NPHAS,NWOULI,ISTGRD,
     &           NCOOLI, NCOOLW
*
       LOGICAL   BBFALS,BBSTLA
*
       CHARACTER CORBND*8, CORESE*4, CORTYP*4
*
       DATA
     &    COM/50*0./,RNP/100*0./,B1/101*0./,B3/100*0./,FACTOR/1.E+3/,
     &    RK1/100*0./,TP/101*0./,TS/100*0./,BBFALS/.FALSE./
*
CC*SEBL End of the declarations block.
*
       IF(KCORE.EQ.2) COM(37)=ANINT(YHRED*FACTOR)
       IF(KCORE.EQ.4) COM(37)=ANINT(YHADD*FACTOR)
*
       TEMP=TTOILM+20.
       IF(KCOOL.EQ.3) TEMP=TTOILM+10.
       IF(TTOILC.GT.0.1) TEMP=TTOILC+30.
*
C... No. of cooling ducts
*
CC*SBBL
*
       BHELP=BLIMB*BMAXPU
       NCOOLW=NCOOL(CORESE,TYPCOR,DCORE,TEMP,BHELP,FREQ,BBSTLA,JFC,
     & ISTGRD  )

          IF (NCOOLI.LT.0) THEN
              NCOOLC = NCOOLW
          ELSE
              NCOOLC = NCOOLI
          ENDIF
*
CC*SEBL
*
C... Glued core    given by: KBAND = 0
C... Standard with ASECOND : KBAND = 1
C... Steel banding given by: KBAND = 2
*
CC*SBBL Start of the Banding block
*
       KBAND=1
       IF(HLIMB.GT.4.) KBAND=2
       IF(CORBND(1:1).EQ.'S') KBAND=2
       IF(KCORE.EQ.1.OR.KCORE.EQ.6) KBAND=0
*
CC*SEBL End of the Banding block
*
C... Determine Core input
*
CC*SBBL Start of the Core input block
*
       T1=0.
       TKAN=0.
       IF(TYPCOR.GT.6.) TKAN=6.
*
C... Indata to CORE
*
       BT=0.
       T2=0.
       TT=0.
       DN=0.
       B1MAX=0.
       B1MIN=0.
       CALL PRECOR(ANINT(DCORE*FACTOR),ANINT(HLIMB*FACTOR),KCORE,
     &             0.8,DN,B1MAX,B1MIN,T1,T2,
     &             BT,TT,BBFALS,RDRAG,BDRAG,TDRAG,KBAND,2000.,10.,
     &             TYPCOR,TKAN)

	
	TASEC=T1/FACTOR
       BDRAG=BDRAG/FACTOR
       TDRAG=TDRAG/FACTOR
*
CC*SEBL End of the Core input block
*
C... Call of CORE
*
CC*SBBL Start of the CORE block
*
       COM(1)=ANINT(FLOAT(NPHAS))
       COM(2)=ANINT(DCORE*FACTOR)
       COM(3)=DN
       COM(5)=ANINT(PLIMB*FACTOR)
       COM(6)=ANINT(HLIMB*FACTOR)
       COM(7)=10.
       COM(8)=24.
       IF(ISTGRD.EQ.3) COM(8)=32.
       IF(ISTGRD.EQ.4) COM(8)=32.
       TJPL=0.3
       IF(ISTGRD.EQ.3) TJPL=0.23
       IF(ISTGRD.EQ.4) TJPL=0.23
       COM(9)=TJPL
       COM(10)=FNEXTV
       COM(11)=ANINT(FLOAT(NCOOLC))
       COM(12)=2.
       COM(13)=B1MIN
       COM(14)=B1MAX
       COM(19)=TT
       COM(33)=ANINT(TWSUP*FACTOR)
       COM(34)=40.
       COM(35)=60.
       COM(36)=ANINT(FLOAT(KCORE))
       COM(38)=ANINT(FLOAT(ILACK))
       COM(40)=FEXTV
       COM(41)=1.732
*
C... Calculate the core section
*
       write(*,*)"CALLING CORE"

       CALL CORE(COM,RNP,B1,B3,RK1,TP,TS)



*
CCCCC  TYP=ANINT(FLOAT(NPHAS))
CCCCC  NPKT=NINT(COM(30))
*
CC*SEBL End of the CORE block
*
C... Calculate the proportion of varnished laminations
C... This is always zero as we no longer use varnished core steel
*
CCCCC  CALL LANDEL(TYP,ANINT(DCORE*FACTOR),ILACK,NPKT,B1,TP,ANDEL)
       ANDEL=0.
*
C... Calculations of some core parameters
*
CC*SBBL Start of the core parameter block
*
       UTURN0=COM(20)

       U0=UTURN0*FREQ/50.
*
       GLIMBM=COM(15)
       GYOKE=COM(16)
       GCORN=COM(17)
       GLIMBY=0.
       HYOKE=COM(28)/FACTOR
       TCORE=COM(29)/FACTOR
       ACORE=COM(22)/1.E+4
       RLYOKE=(FLOAT(NWOULI)-1.)*PLIMB+B1MAX/FACTOR
       YOKAMP=COM(23)
*
CC*SEBL End of the core parameter block
*
       RETURN
       END
