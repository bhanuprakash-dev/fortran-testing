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
*      Subroutine CORE1N
*      -----------------
*
C...   Title:  Calculation of core data
*
*      Written: XX-XX-XX by A.N.Other      , XXXX
*      Revised: 85-10-18 by Ron Bell       , TRAFO/IK
*      Revised: 87-03-09 by H.H�glund      , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 89-10-02 by B-G Bladh      , SETFO/K1
*      Revised: 90-02-12 by Per S�derberg  , SETFO/IK
*      Revised: 91-09-05 by Jan Johansson  , SETFO/TS added TA1 core
*      Revised: 91-10-10 by B-G Bladh      , SETFO/TS
*        COR130 is included (old 130 kV core)
*      Revised: 92-04-15 by B-G Bladh      , SETFO/TS
************************************************************************
*
       SUBROUTINE CORE1N(BBEXAC,BBSLIM,BLIMB,BMAXPU,CORBND,DCORE,
     &                   DOUTW,DPHAS,DPHSL,FEXTV,FNEXTV,FREQ,GACTP,
     &                   HLIMB,ILACK,JFC,KCOOL,KCORE,NPHAS,NWOULI,
     &                   QFINAL,TTOILM,TTOILC,TWSUP,TYPCOR,U0IN,UDIM1,
     &                   UMAXPU,YHADD,YHRED,
     &                   ACORE,ANDEL,ASECL,BDRAG,DKSL,GCORLA,
     &                   GCORN,GLIMBM,GLIMBY,GYOKE,HCORE,HYOKE,KBAND,
     &                   NCOOLC,PLIMB,PLSL,RDRAG,RLTRAN,RLYOKE,SBF,
     &                   TASEC,TCORE,TDRAG,TURNRA,U0,YOKAMP,DCFCTR,
     &                   BBEXON,CORESE,CORTYP,BBSTLA, STKFAC,ISTGRD,
     &                   NCOOLI, NCOOLW,SPFADJ)
*
C... Declarations
       include'com1.h'
       EQUIVALENCE
     &            (XZ1533, DN)
       REAL DN
*
CC*SBBL Start of the declarations block.
*
       REAL        DCORE0,HLIMB0,U00,ACORE0,DKSL0,HYOKE0,GLIMM0,
     &             GLIMY0,GYOKE0,GCORN0,ASECL0,RLYOK0,RLTRA0,
     &             STKFAC,SPFADJ
       COMMON
     &    /PROPOR/ DCORE0,HLIMB0,U00,ACORE0,DKSL0,HYOKE0,GLIMM0,
     &             GLIMY0,GYOKE0,GCORN0,ASECL0,RLYOK0,RLTRA0
*
       REAL      ACC,ACORE,AKRPC,AKRP0,ALCC,ALC0,ANDEL,ASECL,
     &           BDRAG,BLIMB,BMAXPU,DCFCTR,DCORE,DKSL,DOUTW,DPHAS,
     &           DPHSL,FEXTV,FNEXTV,FREQ,GACTP,GCORLA,GCORN,GLIMBM,
     &           GLIMBY,GYOKE,HCORE,HLIMB,HYOKE,PLIMB,PLSL,QFINAL,
     &           RDRAG,RLTRAN,RLYOKE,SBF,TASEC,TCORE,TDRAG,TTOILC,
     &           TTOILM,TURNRA,TWSUP,TYPCOR,T1C,T10,T2C,T20,UDIM1,
     &           UMAXPU,U0,U0IN,X,XX,XXX,YHADD,YHRED,YOKAMP,ZLIMB
*
       INTEGER   ILACK,JFC,KBAND,KCOOL,KCORE,MC,M0,NCOOLC,NPHAS,
     &           NWOULI,N1C,N10,N2C,N20, NCOOLI,NCOOLW
*
       LOGICAL   BBSLIM,BBEXAC,BBEXON,BBSTLA, BBLARM
*
       CHARACTER CORBND*8,CORESE*4,CORTYP*4, ALARM*50

* Local for the TA1 core

       REAL DN0,  FHD,  MWB, ACCL, ACCV
       REAL OVRLPI,  DIWBI, DEWBI, MAP, TCORST
       INTEGER NK, KB , ISTGRD, IFLPMT
       INTEGER NPACK, NSHT(100)
       REAL    A, MLMB, MYK, MCRNR, TASC, ETA,BSTEPI,BPLMAI
       REAL SHWDT(100), THKPCK(100), MAXWDT, MINWDT, HYK, HCR, TKO,
     &      RNFLP
*
CC*SEBL End of the declarations block.
*
C--- Set BBEXON=.TRUE. if core diameter has changed sufficiently
*
       print*,'inside core1n'
       IF (.NOT.(BBEXAC.OR.BBEXON)) THEN
           BBEXON=.FALSE.
           IF (((ABS(1.-DCORE0/DCORE).GT.DCFCTR)
     &      .OR.(ABS(1.-HLIMB0/HLIMB).GT.DCFCTR)))
     &     BBEXON=.TRUE.
       END IF
*
       print *,BBEXAC,BBEXON,'core1n'
       IF (BBEXAC.OR.BBEXON) THEN
*
C--- Round off core diameter to integral mm
*
          IF (BBEXAC) DCORE=ANINT(1.E+3*DCORE)/1.E+3
*
          DCORE0=DCORE
          HLIMB0=HLIMB
          ZLIMB =FLOAT(NWOULI)

*
C... Calculate the core
*
C... NO side-limbs
*
CTA1C          IF (.NOT.BBSLIM .AND. KCORE.EQ.101) THEN
          IF (.NOT.BBSLIM .AND. KCORE.EQ.7682) THEN
C,,,TA1 core
             CALL PRETA1(DCORE, HLIMB, CORBND, TTOILC, BLIMB,
     &                   FREQ, TCORST,
     &                   DN0,  NK, FHD, IFLPMT,
     &                   MWB, ACCL, ACCV, TASC, KB,ETA, BSTEPI,
     &                   BPLMAI, OVRLPI,NCOOLI,NCOOLW)
             DN = DN0
             RNK = REAL(NK)
CCC
CCC CALL CORTA1(DN, TCORST, RNK, PLIMB, HLIMB, FHD, IFLPMT,
CCC  &                   MWB, QFINAL, ACCL, ACCV,ETA,BSTEPI,BPLMAI,
CCCC &                   .FALSE.,OVRLPI,BBSTLA , DEWBI, DIWBI, MAP,
CCC  &                   A, MLMB, MYK, MCRNR,
CCC  &                   NPACK, NSHT, SHWDT, THKPCK, MAXWDT, MINWDT,
CCC  &                   HYK, HCR, TKO, RNFLP, BBLARM, ALARM)
             ACORE0 = A
             ANDEL = 0.
             BDRAG = 0.
             DKSL0 =0.
             GCORN0 = MCRNR
             GLIMM0 = MLMB
             GLIMY0 = 0.
             GYOKE0 = MYK
             HCORE = HCR
             HYOKE0 = HYK
             KBAND = KB
             NCOOLC = NK
             RDRAG = 0.
             RLYOK0 = 2.*PLIMB + DN
             TASEC = TASC
             TCORE = TKO
             TDRAG = 0.
             U00 = (2.*3.14159*FREQ*ACORE0*1.)/SQRT(2.)
             YOKAMP = 1.

          ELSE IF(.NOT.BBSLIM .AND. KCORE.EQ.3) THEN
****   130 -kv CORE****
       CALL COR130(ZLIMB, DCORE, NPHAS, FREQ, BLIMB, UMAXPU,
     &             HLIMB, PLIMB,
     &             ACORE0,ANDEL,BDRAG,GCORN0,GLIMM0,GLIMY0,GYOKE0,
     &             HYOKE0,NCOOLC,RDRAG,RLYOK0,TASEC,TCORE,
     &             TDRAG,U00,YOKAMP,CORBND,KBAND,STKFAC,
     &             NCOOLI,NCOOLW,SPFADJ)
*
          ELSE IF(.NOT.BBSLIM .AND. KCORE.NE.3 .AND.
     &            KCORE .NE. 101) THEN
*
	
          CALL COR1(BLIMB,BMAXPU,CORBND,DCORE,FEXTV,FNEXTV,FREQ,
     &              HLIMB,ILACK,JFC,KCOOL,KCORE,NPHAS,NWOULI,PLIMB,
     &              TTOILM,TTOILC,TWSUP,TYPCOR,UMAXPU,YHADD,
     &              YHRED,
     &              ACORE0,ANDEL,BDRAG,GCORN0,GLIMM0,GLIMY0,GYOKE0,
     &              HYOKE0,KBAND,NCOOLC,RDRAG,RLYOK0,TASEC,TCORE,
     &              TDRAG,U00,YOKAMP,CORESE,CORTYP,BBSTLA,ISTGRD,
     &              NCOOLI,NCOOLW )

          DKSL0=0.
*
C... Side-limbs
*
          ELSE
*
C... Calculate the core
*
          CALL COR2(BLIMB,BMAXPU,CORBND,DCORE,DOUTW,DPHSL,FEXTV,
     &              FNEXTV,FREQ,GACTP,HLIMB,ILACK,JFC,KCOOL,
     &              KCORE,NPHAS,PLIMB,PLSL,QFINAL,TTOILM,
     &              TTOILC,TWSUP,TYPCOR,YHADD,
     &              ACORE0,ANDEL,BDRAG,DKSL0,GCORN0,GLIMM0,GLIMY0,
     &              GYOKE0,HYOKE0,KBAND,NCOOLC,RDRAG,RLYOK0,SBF,
     &              TASEC,TCORE,TDRAG,U00,YOKAMP,
     &              CORESE,CORTYP,BBSTLA,ISTGRD,NCOOLI,NCOOLW)
          END IF
*
C... Turn voltage & core area
*
C,,, If turn voltage at 1T given
*
          IF (U0IN.GT.0.1) THEN
             U00=U0IN*FREQ/50.
             ACORE0=U0IN/222.15
          END IF
          ASECL=0.
          ASECL0=0.
*
C... Core banding
*
C,,, ASECOND

*
          IF (KBAND.EQ.1 .AND. KCORE .NE. 101) THEN
             ACC=0.8
*
C... ASECOND
*
             CALL ASEC(1.E+3*DCORE,1.E+3*HLIMB,ACC,
     &                 N1C,N2C,T1C,T2C,ALCC,AKRPC,MC,
     &                 N10,N20,T10,T20,ALC0,AKRP0,M0)
*
             ASECL0=FLOAT(NWOULI)*ALCC
             IF (BBSLIM) ASECL0=ASECL0+2.*ALC0
          END IF
       END IF
*
C... Proportionation of core data
*
       X=(DCORE-0.024)/(DCORE0-0.024)
       XX=X*X
       XXX=X*X*X
       U0=U00*XX
       ACORE=ACORE0*XX
       DKSL=DKSL0*X
       HYOKE=HYOKE0*X
       TURNRA=UDIM1/U0/BLIMB
       RLYOKE=RLYOK0
       HCORE=HLIMB+2.*HYOKE
       print *,KBAND,'KBANDDDDDD'
       IF (KBAND.EQ.1) ASECL=ASECL0*X*HLIMB/HLIMB0
*
C... Length of the transformer
*
C,,, NO side-limbs
*
       IF (.NOT.BBSLIM) THEN
          PLSL=0.
          RLTRAN=FLOAT(NWOULI)*PLIMB-DPHAS
          IF (NWOULI.EQ.1) RLTRAN=DOUTW+DPHSL+DCORE
*
C... Side-limbs
*
       ELSE
          PLSL=(DOUTW+DKSL)/2.+DPHSL+TASEC
          RLTRAN=FLOAT(NWOULI-1)*PLIMB+2.*PLSL+DKSL
       END IF
*

       

       IF (BBEXAC.OR.BBEXON) RLTRA0=RLTRAN
       WRITE(*,*) 'CORE1N $XXX', XXX
       WRITE(*,*) 'CORE1N $HLIMB', HLIMB
       WRITE(*,*) 'CORE1N $HLIMB0', HLIMB0
       WRITE(*,*) 'CORE1N $GCORN0', GCORN0
       WRITE(*,*) 'CORE1N $GLIMY0', GLIMY0
       WRITE(*,*) 'CORE1N $GLIMM0', GLIMM0
       WRITE(*,*) 'CORE1N $RLTRAN', RLTRAN
       GCORN=GCORN0*XXX
       GLIMBM=GLIMM0*HLIMB/HLIMB0*XX
       GLIMBY=GLIMY0*HLIMB/HLIMB0*XX
       GYOKE=GYOKE0*RLTRAN/RLTRA0*XX
       WRITE (*,*) 'CORE1N GLIMBM, GLIMBY', GLIMBM, GLIMBY 
       WRITE (*,*) 'CORE1N GYOKE, GCORN', GYOKE, GCORN
       GCORLA=GLIMBM+GLIMBY+GYOKE+GCORN
*
       print*,'coming out of core1n'
       RETURN
       END
