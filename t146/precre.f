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
*      Subroutine PRECRE
*      -----------------
*
C...   Title:  Assign core variables
*
*      Written: XX-XX-XX by A.N.Other      , XXXX
*      Revised: 86-04-03 by Ron Bell       , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 90-02-12 by Per Sãderberg  , SETFO/IK
*      Revised: 91-08-21 by B-G Bladh      , SETFO/TS
*      Revised: 91-09-05 by J Johansson    , SETFO/TS, added TA1 core
*      Revised: 91-10-09 by B-G Bladh      , SETFO/TS
*        130-kV core is included.
*
************************************************************************
*
       SUBROUTINE PRECRE(AARR,BBSLIM,TARR,BBVFR,BMAXPU,JFC,
     &                   NPHAS,NWOULI,UARR,UMAXPU,
     &                   BLTOPM,CHCORE,DCOMAX,DCOMIN,FEXTV,FNEXTV,
     &                   KCORE,NCLA,ISTGRD,U0IN,YHADD,
     &                   YHRED,BBERR,BBTS,BBHELP)
*
C... Declarations
*
CC*SBBL Start of declarations block.
*
       REAL      AARR(200),BLTOPM,BMAX,BMAXPU,DCOMAX,DCOMIN,
     &           FEXTV,FNEXTV,UMAXPU,U0IN,YHADD,YHRED
*
       INTEGER   JFC,KCORE,NCLA,NPHAS,NWOULI,ISTGRD
*
       LOGICAL   BBSLIM,BBVFR,BBERR,BBTS,BBHELP(10)
*
       DIMENSION UARR(100), TARR(100)
       CHARACTER UARR*4,CHCORE*60, TARR*8
*
CC*SEBL End of declarations block.
*
C... KCORE
*
CC*SBBL Start of KCORE block.
*
       ISTGRD=1
       IF(TARR(1).EQ.'HI-B    ') ISTGRD = 2
       IF(TARR(1).EQ.'ZDKH    ') ISTGRD = 3
       IF(TARR(1).EQ.'PLJT    ') ISTGRD = 4
*
       KCORE=2
       IF(UARR(21).EQ.'TAA ') THEN
          KCORE=1
       ELSE IF(UARR(21).EQ.'130 ') THEN
          KCORE=3
       ELSE IF(UARR(21).EQ.'TCA ') THEN
          KCORE=4
       ELSE IF(UARR(21).EQ.'TAC ') THEN
          KCORE=5
       ELSE IF(UARR(21).EQ.'TAD ') THEN
          KCORE=6
       ELSE IF(UARR(21).EQ.'TA1 ') THEN
          KCORE=101
       END IF
*
C... No side-limb core available for TAA series
*
       IF(KCORE.EQ.1.AND.BBSLIM) CALL FPRINT(1,
     &    BBERR,BBHELP,BBTS,JFC,
     &    'Side-limb core does not exist for the TAA-series  ')
       IF(KCORE.EQ.101.AND.BBSLIM) CALL FPRINT(1,
     &    BBERR,BBHELP,BBTS,JFC,
     &    'Side-limb core does not exist for the TA1-series  ')
*
C... No side-limb core available for 130 series
*
       IF(KCORE.EQ.3.AND.BBSLIM) CALL FPRINT(1,
     &    BBERR,BBHELP,BBTS,JFC,
     &    'Side-limb core does not exist for the 130-kV ser. ')
*
CC*SEBL End of KCORE block.
*
C... Flux density
*
CC*SBBL Start of flux density BLOCK.
*
C... Maximum flux density
*
       BMAX=1.95
       IF(TARR(1).EQ.'HI-B    ') BMAX = 1.98
       IF(TARR(1).EQ.'ZDKH    ') BMAX = 1.98
       IF(TARR(1).EQ.'PLJT    ') BMAX = 1.98
       IF(AARR(147).GT.0.1)  BMAX=AARR(147)
*
C--- Maximum flux density during optimisation = BLTOPM
*
       BLTOPM=BMAX/BMAXPU/UMAXPU
*
CC*SEBL End of Flux density block.
*
C... Core dimension adjustment
*
CC*SBBL Start of Core adjustment block.
*
       YHADD=AARR(150)/1.E+3
*
C... Area re-inforcement for TCA (Dimension :  length)
*
       IF(YHADD.GT.0..AND.KCORE.NE.4.AND.KCORE.NE.5) CALL FPRINT(1,
     &    BBERR,BBHELP,BBTS,JFC,
     &    'Yoke height addition only for TCA                 ')
*
       YHRED=AARR(160)/1.E+3
*
C...Yoke height reduction for TBA
*
       IF(YHRED.GT.0..AND.KCORE.NE.2) CALL FPRINT(1,
     &    BBERR,BBHELP,BBTS,JFC,
     &    'Yoke height reduction only for TBA                ')
*
       FNEXTV=AARR(80)
*
C... Filling factor for the core-limb (no extra varnish)
*
       IF(FNEXTV.LT.0.9.OR.FNEXTV.GT.1.) CALL FPRINT(1,
     &    BBERR,BBHELP,BBTS,JFC,
     &    'Fillfactor core, without ext.varn. outside limits ')
*
       FEXTV=AARR(90)
*
C... Filling factor for the core-limb (with extra varnish)
*
       IF(FEXTV.LT.0.9.OR.FEXTV.GT.1.) CALL FPRINT(1,
     &    BBERR,BBHELP,BBTS,JFC,
     &    'Fillfactor core, with ext.varn. outside limits    ')
*
CC*SEBL End of Core adjustment block.
*
C--- Turn voltage given as additional indata
*
       U0IN=AARR(145)
*
C--- Possibly an indata check can be included here
*
C... Minimum and maximum core diameter
*
C,,, TAA core
*
       IF(KCORE.EQ.1) THEN
          CHCORE=
     &    'Without sidelimb acc.to LT-TA 4610-110E (TAA-Core)          '
          NCLA=2
          DCOMIN=265.E-3
          DCOMAX=600.E-3
*
C... TBA core
*
       ELSE IF(KCORE.EQ.2) THEN
          CHCORE=
     &    'Without sidelimb acc.to LT-TA 4611-107 (TBA-Core)           '
          NCLA=3
          DCOMIN=265.E-3
          DCOMAX=1040.E-3
*
C... 130 kV core
*
       ELSE IF(KCORE.EQ.3) THEN
          CHCORE=
     &    'Without sidelimb , 130 kV core                              '
          NCLA=3
          DCOMIN=265.E-3
          DCOMAX=670.E-3
*
C... TCA core
*
       ELSE IF(KCORE.EQ.4.OR.KCORE.EQ.5) THEN
*
C... TCA core without side-limb
*
         IF (.NOT.BBSLIM) THEN
          CHCORE=
     &    'TCA - Core without sidelimbs                                '
          NCLA=3
          DCOMIN=500.E-3
          DCOMAX=1040.E-3
*
C... TCA core with side-limb
*
         ELSE
          CHCORE=
     &    'TCA - Core with sidelimbs                                   '
          NCLA=3
          DCOMIN=680.E-3
          DCOMAX=1500.E-3
         END IF
*
C... TAC core without side-limbs
*
       ELSE IF(KCORE.EQ.6) THEN
          IF(UARR(21).EQ.'TAC ') CHCORE=
     &    'TAC - Core without sidelimbs                                '
          IF(UARR(21).EQ.'TAD ') CHCORE=
     &    'TAD - Core without sidelimbs                                '
          NCLA=3
          DCOMIN=265.E-3
          DCOMAX=600.E-3
*
C... TA1 core
*
       ELSE IF(KCORE .EQ. 101) THEN
          CHCORE=
     &    'Core without sidelimb acc.to 1ZBA-TS 4610-101 (TA1-Core)    '
          NCLA=3
          DCOMIN=400.E-3
          DCOMAX=1000.E-3
*
C... LTB core
*
       ELSE
          CHCORE=
     &    'Core with sidelimb acc.to LT-TA 4610-111E (LTB-Core)        '
          NCLA=3
          DCOMIN=555.E-3
          DCOMAX=1500.E-3
       END IF
*
       IF(AARR(170).GT.0.) DCOMIN=AARR(170)/1.E+3
       IF(AARR(171).GT.0.) DCOMAX=AARR(171)/1.E+3
*
       RETURN
       END
