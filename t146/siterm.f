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
*      Subroutine SITERM
*      -----------------
*
C...   Title: Transfers terminal data from indata fields to
C...          internal variables, and converts them to SI-units.
C...          Corresponding indata checks are performed.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 83-04-29 by B.Holmgren , KYT
*      Revised: 84-08-08 by P.L-V.     , KYT
*      Revised: 86-09-12 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE SITERM
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ0231(1)    ,  DARR   (1)    ),
     &  (XZ0301(1)    ,  IARR   (1)    ),
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ1215(1)    ,  KCON   (1)    ),
     &  (XZ1219(1)    ,  KTYPRW (1)    )
       EQUIVALENCE
     &  (XZ1227(1)    ,  NMSTEP (1)    ),
     &  (XZ1231(1)    ,  NPSTEP (1)    ),
     &  (XZ1239(1)    ,  PUSTEP (1)    ),
     &  (XZ1255(1)    ,  SLINE  (1)    ),
     &  (XZ1259(1)    ,  SRATE  (1)    )
       EQUIVALENCE
     &  (XZ1283(1)    ,  UNLINE (1)    ),
     &  (XZ1287(1)    ,  USURG  (1)    ),
     &  (XZ1291(1)    ,  XRINT  (1)    ),
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1373       ,  BBAUTO        )
       EQUIVALENCE
     &  (XZ1392       ,  BBUPTR        ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1500       ,  NPG           ),
     &  (XZ1504       ,  NSG           )
       EQUIVALENCE
     &  (XZ3010(1)    ,  NXSTEP (1)    ),
     &  (XZ3289(1)    ,  FARR   (1)    )
*
       REAL      AARR(200),DARR(100),SRATE(4),UNLINE(4),
     &           PUSTEP(4),USURG(4),SLINE(4),XRINT(4),FARR(100),HLP
*
       INTEGER   IARR(100),NPSTEP(4),NMSTEP(4),KTYPRW(4),KCON(4),
     &           NXSTEP(3),JTML,NG,NPG,NSG,JFC
*
       DIMENSION TARR(100)
       CHARACTER TARR*8
*
       LOGICAL   BBAUTO,BBUPTR,BBVR(4),BBA,BBB,BBVFR,BBTS,BBERR,
     &           BBHELP(10)
*
CC*SEBL End of the declarations block.
*
       BBVFR=.FALSE.
       IF (TARR(2).EQ.'YES     ') BBVFR=.TRUE.
*
C... For each terminal
*
       DO 299 JTML=1,NG
       SRATE(JTML)=1.E+6*AARR(10+JTML)
       UNLINE(JTML)=1.E+3*AARR(15+JTML)
*
C... Set Step dimensions
*
C,,, If a regulated terminal
*
       IF (BBVR(JTML)) THEN
          NPSTEP(JTML)=IARR(5+JTML)
          NMSTEP(JTML)=IARR(10+JTML)
          PUSTEP(JTML)=AARR(20+JTML)/1.E+2
*
C... Tap outside specified range
*
          IF ((NINT(FARR(52+JTML)).GT.(NPSTEP(JTML)).OR.
     &        NINT(FARR(52+JTML)).LT.(-NMSTEP(JTML))))
     &    CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &    'SITERM:Calculation tap chosen beyond the regulation range')
*
          NXSTEP(JTML)=0.5+FARR(52+JTML)+NMSTEP(JTML)
*
          KTYPRW(JTML)=0
          IF (TARR(10+JTML).EQ.'L       ') KTYPRW(JTML)=1
          IF (TARR(10+JTML).EQ.'PM      ') KTYPRW(JTML)=2
          IF (TARR(10+JTML).EQ.'CF      ') KTYPRW(JTML)=3
*
C... Regulation type not recognised
*
          IF (KTYPRW(JTML).EQ.0)
     &    CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &                'SITERM:Regulating Winding not: L PM CF')
       END IF
*
       KCON(JTML)=-1
       IF (TARR(20+JTML).EQ.'I     ') KCON(JTML)=0
       IF (TARR(20+JTML).EQ.'Y     ') KCON(JTML)=1
       IF (TARR(20+JTML).EQ.'YN    ') KCON(JTML)=1
       IF (TARR(20+JTML).EQ.'D     ') KCON(JTML)=3
       IF (TARR(20+JTML).EQ.'I/AUTO') KCON(JTML)=10
       IF (TARR(20+JTML).EQ.'I/A   ') KCON(JTML)=10
       IF (TARR(20+JTML).EQ.'Y/AUTO') KCON(JTML)=11
       IF (TARR(20+JTML).EQ.'Y/A   ') KCON(JTML)=11
*
C... Connection illegally stated
*
       IF (KCON(JTML).EQ.-1)
     & CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &             'SITERM:Connection illegally stated')
*
C... Autoconnection only for Term 1 and 2
*
       IF (KCON(JTML).GT.9.AND.JTML.GT.2)
     & CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &             'SITERM:Autoconnection only for Term 1 and 2')
*
       USURG(JTML)=1.E+3*AARR(25+JTML)
       SLINE(JTML)=1.E+9*AARR(30+JTML)
       IF (JTML.LE.3) XRINT(JTML)=AARR(35+JTML)/1.E+2
  299  CONTINUE
       XRINT(4)=0.
*
       BBAUTO=(KCON(1).GE.10.OR.KCON(2).GE.10)
*
       HLP=ABS(SRATE(1)-SRATE(2))
*
C... Rating 1 not equal 2 when autoconnected
*
       IF (BBAUTO.AND. HLP.GT. 1.)
     & CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     & 'SITERM:Rating 1 not equal 2 when autoconnected')
*
C... Connection 1 not equal 2 when autoconnected
*
       IF (BBAUTO.AND.KCON(1).NE.KCON(2))
     & CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     & 'SITERM:Connection 1 not equal 2 when autoconnected')
*
       BBA=(NG.GE.3.AND.BBVR(3))
       BBB=BBVR(1).OR..NOT.BBVR(2).OR.BBA
C... ONLY Terminal 2 shall be regulated when ind.regulation
*
       IF (BBVFR.AND.BBB)
     & CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     & 'SITERM:ONLY Terminal 2 shall be regulated when ind.regulation')
*
C...BBUPTR = Step-up transformer
C...NPG=Terminal number for the parallel winding
C...NSG=Terminal number for the series   winding
*
       BBUPTR=(UNLINE(2).GE.UNLINE(1))
       NPG=2
       IF (BBUPTR) NPG=1
       NSG=3-NPG
*
       RETURN
       END
