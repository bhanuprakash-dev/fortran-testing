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
*      Subroutine SIACTP
*      -----------------
*
C...   Title:  Convert indata to SI-units and
C...           transfer them to internal variables
C...           for variables which do NOT refer to
C...           Tank, Terminals or Windings
*
*      Written: XX-XX-XX by A.N.Other      , XXXX
*      Revised: 85-10-18 by Ron Bell       , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 90-02-12 by Per Sãderberg  , SETFO/IK
*      Revised: 91-09-13 by B-G Bladh      , SETFO/TS
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE SIACTP
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ0181(1)    ,  BARR   (1)    ),
     &  (XZ0231(1)    ,  DARR   (1)    ),
     &  (XZ0301(1)    ,  IARR   (1)    ),
     &  (XZ0401(1)    ,  TARR   (1)    )
       EQUIVALENCE
     &  (XZ1299       ,  BB03          ),
     &  (XZ1300       ,  BB051         ),
     &  (XZ1370       ,  BB08          ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1371       ,  BB132         ),
     &  (XZ1376       ,  BBDSHE        )
       EQUIVALENCE
     &  (XZ1381       ,  BBFLDS        ),
     &  (XZ1390       ,  BBSLIM        ),
     &  (XZ1395       ,  BBWISU        ),
     &  (XZ1398       ,  AFIELD        )
       EQUIVALENCE
     &  (XZ1405       ,  BLIMB         ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1429       ,  DPHAS         ),
     &  (XZ1430       ,  DPHSL         )
       EQUIVALENCE
     &  (XZ1446       ,  FREQ          ),
     &  (XZ1467       ,  GTRPM         ),
     &  (XZ1473       ,  HLMBMA        ),
     &  (XZ1478       ,  HLMBMI        ),
     &  (XZ1480       ,  ILACK         ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1488       ,  KCOOL         )
       EQUIVALENCE
     &  (XZ1489       ,  KCOOL1        ),
     &  (XZ1490       ,  KCOOL2        ),
     &  (XZ1501       ,  NPHAS         ),
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1510       ,  P0LOSS        )
       EQUIVALENCE
     &  (XZ1514       ,  PKLOSS        ),
     &  (XZ1516       ,  PLOADM        ),
     &  (XZ1520       ,  PNOLOM        ),
     &  (XZ1523       ,  QFINAL        ),
     &  (XZ1543       ,  SOUNDM        )
       EQUIVALENCE
     &  (XZ1553       ,  TTOILC        ),
     &  (XZ1554       ,  TTOILM        ),
     &  (XZ1556       ,  TWINDM        ),
     &  (XZ1557       ,  TWSUP         ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ1560       ,  UMAXPU        )
       EQUIVALENCE
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ2295       ,  CORBND        ),
     &  (XZ2701       ,  EXTCOR        ),
     &  (XZ1419       ,  CPUIND        )
*
       REAL      AARR(200),BARR(100),DARR(100),AFIELD,BLIMB,DCORE,
     &           DPHAS,DPHSL,FONAN,FREQ,GTRPM,HLMBMA,PKLOSS,PLOADM,
     &           PNOLOM,P0LOSS,QFINAL,SOUNDM,TTOILC,TTOILM,TWINDM,
     &           TWSUP,UMAXPU,ZWOULI,HLMBMI
*
       INTEGER   IARR(100),ILACK,KCOOL,KCOOL1,KCOOL2,NPHAS,NWOULI,JFC,
     &           ISTOP
*
       LOGICAL   BBSLIM,BBWISU,BBFLDS,BBDSHE,BBTS,BBERR,
     &           BB03,BB08,BB051,BB132,FBB,BBHELP(10),
     &           EXTCOR
*
       DIMENSION TARR(100)
       CHARACTER TARR*8,CORBND*8
*
*
CC*SEBL End of the declarations block.
*
       NPHAS=IARR(1)
       FREQ=AARR(2)
       UMAXPU=1.+(AARR(5)/1.E+2)
*
C... Maximum losses - Loss evaluation
*
CC*SBBL Start of the Max. losses block.
*
       PNOLOM=0.
       IF (AARR(6).LT.0.) PNOLOM=-1.E+3*AARR(6)
       P0LOSS=0.
       IF (AARR(6).GE.0.) P0LOSS=AARR(6)/1.E+3
       PLOADM=0.
       IF (AARR(7).LT.0.) PLOADM=-1.E+3*AARR(7)
       PKLOSS=0.
       IF (AARR(7).GE.0.) PKLOSS=AARR(7)/1.E+3
*
CC*SEBL End of the Max. losses block.
*
       DPHAS=AARR(141)/1.E+3
       DPHSL=AARR(142)/1.E+3
       NWOULI=IARR(51)
       ZWOULI=FLOAT(NWOULI)
       TWSUP=BARR(30)/1.E+3
       DCORE=AARR(143)/1.E+3
       BLIMB=AARR(146)
       FONAN=AARR(154)
       QFINAL=1.E+6*BARR(31)
*
       IF (ABS(AARR(4)).GT.100.) DARR(20)=AARR(4)
*
C--- This transfer because HLIMB can be input in two ways.
C--- In future only DARR(20) will be used for this purpose.
*
       CORBND=TARR(80)
       ILACK=DARR(30)
*
       KCOOL=-1
       IF (TARR(56).EQ.'NO    ') KCOOL=0
       IF (TARR(56).EQ.'ONAN  ') KCOOL=1
       IF (TARR(56).EQ.'ONAF  ') KCOOL=2
       IF (TARR(56).EQ.'OFAF  ') KCOOL=3
       IF (TARR(56).EQ.'ONAN/F') KCOOL=4
*
C... Illegal mixture of cooling type and self cool ratio
*
       IF (FONAN.GT.1.001) THEN
          IF (KCOOL.LE.1 .OR. KCOOL.EQ.3) THEN
            CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &'SIACTP: Illegal mixture of self cooling ratio and cooling type')
*....        Backtracking
             IF(ISTOP.EQ.1) RETURN
          ENDIF
       ENDIF
*
       IF (KCOOL.EQ.1) THEN
          IF (FONAN.LE.0.99 .OR. FONAN .GE.1.01) THEN
             CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &'SIACTP: Illegal mixture of self cooling ratio and cooling type')
*....        Backtracking
             IF(ISTOP.EQ.1) RETURN
          ENDIF
       ENDIF
*
C... Illegal cooling type
*
       IF (KCOOL.LT.0) THEN
           CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &                             'SIACTP:Illegal cooling type')
*....      Backtracking
           IF(ISTOP.EQ.1) RETURN
       ENDIF
*
C... Calculation of preliminary cooling variables
*
CC*SBBL Start of the prel cooling block.
*
       KCOOL2=KCOOL
       IF (KCOOL.GT.3) KCOOL2=2
       IF (KCOOL.EQ.0) KCOOL2=1
*
*
       TWINDM=AARR(155)
       TTOILM=AARR(156)
       TTOILC=AARR(100)
*
C...   Set certain values for AARR
*
       CALL PREACT
*
       GTRPM=1.E+3*AARR(173)
       SOUNDM=AARR(177)
       AFIELD=AARR(157)
       HLMBMA=DARR(10)/1.E+3
       HLMBMI=DARR(40)/1.E+3
*
CC*SEBL End of the prel cooling block.
*
       BBWISU=.FALSE.
       IF (BARR(30).GT.0.) BBWISU=.TRUE.
       BBSLIM=FBB('SIACTP',BBERR,BBHELP,BBTS,JFC,TARR(51))
       EXTCOR=.FALSE.
       IF (TARR(1).EQ.'EXT     ') EXTCOR=.TRUE.
*
       BB051=FBB('SIACTP',BBERR,BBHELP,BBTS,JFC,TARR(73))
       BB132=FBB('SIACTP',BBERR,BBHELP,BBTS,JFC,TARR(75))
       BBFLDS=FBB('SIACTP',BBERR,BBHELP,BBTS,JFC,TARR(3))
       BBDSHE=FBB('SIACTP',BBERR,BBHELP,BBTS,JFC,TARR(4))
       
*
C... Reoptimisation

       CPUIND=AARR(161)
*
       RETURN
       END
