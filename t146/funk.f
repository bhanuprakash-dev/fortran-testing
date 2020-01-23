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
*      Subroutine FUNK
*      ---------------
*
C...   Title:  Performs a complete function calculation
*
*      Written: XX-XX-XX by A.N.Other      , XXXX
*      Revised: 83-06-16 by H.Westberg     , ZKAB
*      Revised: 85-10-18 by Ron Bell       , TRAFO/IK
*      Revised: 87-03-02 by H.H�glund      , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 89-10-02 by B-G Bladh      , SETFO/K1
*      Revised: 90-02-12 by Per S�derberg  , SETFO/IK
*      Revised: 91-10-10 by B-G Bladh      , SETFO/TS
*        Fillfactor (STKFAC) is transmitted to COR130 via COR1N
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE FUNK(XA,VALUE,G,*)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL           RES79(241,6)
       COMMON/KONSRE/ RES79
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0080       ,  FNEXTV        ),
     &  (XZ0090       ,  FEXTV         ),
     &  (XZ0145       ,  U0IN          ),
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ1267(1)    ,  UDIM   (1)    ),
     &  (XZ1301(1)    ,  BBCURD (1)    ),
     &  (XZ1311(1)    ,  BBHELP (1)    )
       EQUIVALENCE
     &  (XZ1375       ,  BBDLI         ),
     &  (XZ1382       ,  BBFLU         ),
     &  (XZ1384       ,  BBHLI         ),
     &  (XZ1379       ,  BBERR1        ),
     &  (XZ1380       ,  BBEXAC        )
       EQUIVALENCE
     &  (XZ1387       ,  BBOUT         ),
     &  (XZ1390       ,  BBSLIM        ),
     &  (XZ1396       ,  ACORE         ),
     &  (XZ1399       ,  AVALUE        ),
     &  (XZ1400       ,  ANDEL         )
       EQUIVALENCE
     &  (XZ1402       ,  ASECL         ),
     &  (XZ1404       ,  BDRAG         ),
     &  (XZ1405       ,  BLIMB         ),
     &  (XZ1407       ,  BMAXPU        ),
     &  (XZ1414       ,  CLOSSV        )
       EQUIVALENCE
     &  (XZ1415       ,  COST          ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1427       ,  DKSL          ),
     &  (XZ1428       ,  DOUTW         ),
     &  (XZ1429       ,  DPHAS         )
       EQUIVALENCE
     &  (XZ1430       ,  DPHSL         ),
     &  (XZ1431       ,  DRV           ),
     &  (XZ1446       ,  FREQ          ),
     &  (XZ1452       ,  GACTP         ),
     &  (XZ1455       ,  GCORLA        ),
     &  (XZ1456       ,  GCORN         )
       EQUIVALENCE
     &  (XZ1459       ,  GLIMBM        ),
     &  (XZ1460       ,  GLIMBY        ),
     &  (XZ1470       ,  GYOKE         ),
     &  (XZ1471       ,  HCORE         ),
     &  (XZ1472       ,  HLIMB         )
       EQUIVALENCE
     &  (XZ1477       ,  HYOKE         ),
     &  (XZ1480       ,  ILACK         ),
     &  (XZ1483       ,  MNLY          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1488       ,  KCOOL         ),
     &  (XZ1491       ,  KCORE         ),
     &  (XZ0581(21)   ,  CORESE        ),
     &  (XZ0581(22)   ,  CORTYP        )
       EQUIVALENCE
     &  (XZ1496       ,  NCOOLC        ),
     &  (XZ1501       ,  NPHAS         ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1510       ,  P0LOSS        ),
     &  (XZ1514       ,  PKLOSS        )
       EQUIVALENCE
     &  (XZ1515       ,  PLIMB         ),
     &  (XZ1518       ,  PLSL          ),
     &  (XZ1523       ,  QFINAL        ),
     &  (XZ1526       ,  RDRAG         ),
     &  (XZ1527       ,  ISTGRD        ),
     &  (XZ1530       ,  RLTRAN        )
       EQUIVALENCE
     &  (XZ1531       ,  RLYOKE        ),
     &  (XZ1537       ,  SBF           ),
     &  (XZ1548       ,  TASEC         ),
     &  (XZ1550       ,  TCORE         ),
     &  (XZ1551       ,  TDRAG         )
       EQUIVALENCE
     &  (XZ1553       ,  TTOILC        ),
     &  (XZ1554       ,  TTOILM        ),
     &  (XZ1555       ,  TURNRA        ),
     &  (XZ1557       ,  TWSUP         ),
     &  (XZ1558       ,  TYPCOR        )
       EQUIVALENCE
     &  (XZ1559       ,  U0            ),
     &  (XZ1560       ,  UMAXPU        ),
     &  (XZ1565       ,  YHADD         ),
     &  (XZ1566       ,  YHRED         ),
     &  (XZ1567       ,  YOKAMP        )
       EQUIVALENCE
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ1626(1)    ,  OBJ    (1)    ),
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1689(1,1,1),  URC    (1,1,1)),
     &  (XZ2295       ,  CORBND        ),
     &  (XZ2307(1)    ,  GREL   (1)    ),
     &  (XZ2500       ,  BBOOVN        )
       EQUIVALENCE
     &  (XZ2501       ,  VALUEM        ),
     &  (XZ2502       ,  VALUEO        ),
     &  (XZ2566       ,  BBEXON        ),
     &  (XZ2735       ,  STKFAC        ),
     &  (XZ2780       ,  NCOOLI        ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ2781       ,  NCOOLW        ),
     &  (XZ2799       ,  SPFADJ        )
*
       REAL       UDIM(4),XA(*),G(*),URC(4,4,3),P00(3),
     &            ACOND(9),ACORE,ANDEL,ASECL,GREL(96),
     &            AVALUE,BDRAG,BLIMB,BMAXPU,CLOSSV,CMANUF,COST,DCFCTR,
     &            DCORE,DKSL,DOUTW,DPHAS,DPHSL,DRV,FEXTV,FNEXTV,FREQ,
     &            GACTP,GCORLA,GCORN,GLIMBM,GLIMBY,GYOKE,HCORE,HELP,
     &            HLIMB,HYOKE,PKLOSS,PLIMB,PLSL,P0LOSS,QFINAL,RDRAG,
     &            RLTRAN,RLYOKE,SBF,TASEC,TCORE,TDRAG,THOU,TTOILC,
     &            TTOILM,TURNRA,TWSUP,TYPCOR,UMAXPU,U0,U0IN,VALUE,
     &            VALUEM,VALUEO,X,Y,YHADD,YHRED,YOKAMP,ZWOULI, STKFAC,
     &SPFADJ
*
       INTEGER  I,IFILE,ILACK,INTRVL,IRANGE,IROW,ISTEG,ISTEG1,ISTEG2,
     &          ISTEP,IWDG,JCOL,JFC,KBAND,KCOOL,KCORE,MNLY,
     &          NCOOLC,NPHAS,NWILI,NWOULI,IFILE1,ISTGRD,NCOOLI,NCOOLW,
     &          ISTOP
*
       DIMENSION OBJ(6)
       CHARACTER  CORBND*8,OBJ*4,CORESE*4,CORTYP*4
*
       LOGICAL    BBERR1,BBSLIM,BBEXAC,BBOOVN,BBOUT,
     &            BBHELP(10),BBRESC,BBRES,BBEXON,BBDUMM,BBCURD(9),
     &            BBDLI,BBFLU,BBHLI,BBSTLA
*
       DATA ISTEG /0/,INTRVL /200/,ISTEG2/0/
*
CC*SEBL End of the declarations block.
       write(*,*) "variable_r0",PLIMB
       write(*,*) 'LIMB Pitch ran',XZ1515*1000
*
C... Count the number of times FUNK has been called
*
       print*,'inside first funk'
       
       ISTEG=ISTEG+1
       IF (ISTEG.EQ.ISTEG/INTRVL*INTRVL) WRITE (*,10) ISTEG,
     &               ' calculations. Diameter = ',1.E+3*DCORE,'mm',
     &               ' Limb height = ',1.E+3*HLIMB,'mm'
*
C... Assignation of free variables
*
       CALL FUNKFR(XA,ACOND,BBCURD,BBDLI,BBFLU,BBHLI,
     &             BLIMB,DCORE,HLIMB,NWILI)
*
       CALL PREVAL(XA,ACOND)
*
C... Core calculations
*
CC*SBBL Start of the core calculations block.
*
       BBEXON=.FALSE.
       PLIMB=DOUTW+DPHAS
*
C--- DCFCTR used to decide when to re-calculate the core exactly
C--- ( 5.*DRV = 5% of either limb height or core diameter
C---   assuming DRV = 0.01 )
*
       DCFCTR=10.*DRV
*
C... Core calculations
*
       BBSTLA = (XZ0581(23).EQ.'YES ')
       CALL CORE1N(BBEXAC,BBSLIM,BLIMB,BMAXPU,CORBND,DCORE,
     &             DOUTW,DPHAS,DPHSL,FEXTV,FNEXTV,FREQ,GACTP,
     &             HLIMB,ILACK,JFC,KCOOL,KCORE,NPHAS,NWOULI,
     &             QFINAL,TTOILM,TTOILC,TWSUP,TYPCOR,U0IN,UDIM(1),
     &             UMAXPU,YHADD,YHRED,
     &             ACORE,ANDEL,ASECL,BDRAG,DKSL,GCORLA,
     &             GCORN,GLIMBM,GLIMBY,GYOKE,HCORE,HYOKE,KBAND,
     &             NCOOLC,PLIMB,PLSL,RDRAG,RLTRAN,RLYOKE,SBF,
     &             TASEC,TCORE,TDRAG,TURNRA,U0,YOKAMP,DCFCTR,BBEXON,
     &        CORESE,CORTYP,BBSTLA,STKFAC,ISTGRD,NCOOLI,NCOOLW,SPFADJ)
*
CC*SEBL End of the core calculations block.
*
C... Currents, losses and reactances
       CALL OPCALC(BBOUT)
*....  Backtracking
       IF(ISTOP.EQ.1) RETURN
*
C... Core. The limb-pitch is now calculated accurately
*
CC*SBBL Start of the core calculations block.
*
       PLIMB=DOUTW+DPHAS

       write(*,*) "variable_r4",PLIMB
       write(*,*) "variable_r5",DOUTW
       write(*,*) "variable_r6",DPHAS
*
C... Core calculations
*
       IF (BBEXON) WRITE (*,10) ISTEG,
     &      ' calculations. Diameter = ',1.E+3*DCORE,'mm',
     &      ' Limb height = ',1.E+3*HLIMB,'mm',' Adjusting the core.'
*
       CALL CORE1N(BBEXAC,BBSLIM,BLIMB,BMAXPU,CORBND,DCORE,
     &             DOUTW,DPHAS,DPHSL,FEXTV,FNEXTV,FREQ,GACTP,
     &             HLIMB,ILACK,JFC,KCOOL,KCORE,NPHAS,NWOULI,
     &             QFINAL,TTOILM,TTOILC,TWSUP,TYPCOR,U0IN,UDIM(1),
     &             UMAXPU,YHADD,YHRED,
     &             ACORE,ANDEL,ASECL,BDRAG,DKSL,GCORLA,
     &             GCORN,GLIMBM,GLIMBY,GYOKE,HCORE,HYOKE,KBAND,
     &             NCOOLC,PLIMB,PLSL,RDRAG,RLTRAN,RLYOKE,SBF,
     &             TASEC,TCORE,TDRAG,TURNRA,U0,YOKAMP,DCFCTR,BBEXON,
     &       CORESE,CORTYP,BBSTLA,STKFAC,ISTGRD,NCOOLI,NCOOLW,SPFADJ)
*
CC*SEBL End of the core calculations block.
*
C... No-load losses
*
C... BBRESC=.FALSE. because no resonance calc. needed here (See PRNLD)
*
CC*SBBL Start of the core resonance calculation block.
*
       BBRESC=.FALSE.
       BBRES=.FALSE.
*
C... No-load losses and resonance
*
       write(*,*)"FUNK>>NLOADN"
       CALL NLOADN(BBRESC,BBRES)
*
CC*SEBL End of the core resonance calculation block.
*
C... Cooling calculation
*
       CALL COOLDP
*
C... Short-circuit forces
*
       CALL SCDIME(BBOUT)
*
C--- Force exact MASS & COST calculations when BBEXON=.TRUE.
*
       IF (BBEXON) THEN
          BBDUMM=BBEXAC
          BBEXAC=.TRUE.
       ENDIF
*
C... Masses & costs
*
*
C... Initialisation of result fields for masses & costs in MNL79.
*
       IF(MNLY.LT.91) THEN
          DO 21 IROW=1,241
          DO 21 JCOL=1,6
 21          RES79(IROW,JCOL)=0.
*
C... Initialisation of summation vector in subroutine COST (ZKLIB)
*
          CALL COSTZO
*
C... MNL79 masses and costs
*
          CALL MASS79
	
          CALL COST79
*
C... MNL79 summation of costs
*
          CALL BYTA79
*
       ELSE
*
C... "MNL92 - FORMULAS"
*
          CALL MASS92
          CALL COST92
       ENDIF
*
C--- Return BBEXAC to its original value if BBEXON=.TRUE.
*
       IF (BBEXON) THEN
          BBEXAC=BBDUMM
       END IF
*
       X=PKLOSS*URC(1,1,1)*ZWOULI
       Y=P0LOSS*P00(1)
       CLOSSV=(X+Y)
*
C... VALUE = the value which the optimisation routine LMIN
C... is to minimise
*
       VALUEO=CLOSSV+COST
*
C... Calculation of limits.
*
       CALL PENLTY(G)
*
       HELP=0.
       IF (BBOOVN.AND.VALUEO.GT.VALUEM) HELP=VALUEO-VALUEM
       IF (BBOOVN) VALUEO=COST+HELP*2.
       VALUE=VALUEO/AVALUE
*
C... XA-values must be assigned if FUNK shall be OK
C... outside the optimisation routine
*
       CALL FRFUNK(XA,ACOND,BBCURD,BBDLI,BBFLU,BBHLI,
     &             BLIMB,DCORE,HLIMB,NWILI)
*
       IF (.NOT.BBERR1.OR.BBHELP(10)) GOTO 899
*
C... RETURN1
*
       RETURN1
  899  CONTINUE
*
C... Set up files to produce optimisation graphs
*
C,,, If called for by TRACE=3 --> Bbhelp(3)=.True.
*
       IF (BBHELP(4)) THEN
*
          THOU=1000.
          ISTEG1=ISTEG
          IFILE1=37
          IRANGE=250
          ISTEP=30
          IF (ISTEG1.EQ.1) THEN
             DO 1999 IFILE=IFILE1,IFILE1+7
             IF (NWILI.EQ.2) THEN
                WRITE(IFILE,135)
     &          '   DCORE HLIMB   BLIMB   ACOND1   ACOND2',
     &          '  COST CMANUF GRAPH OF ',(OBJ(I),I=1,6)
             ELSE IF (NWILI.EQ.3) THEN
                WRITE(IFILE,135)
     &          '   DCORE HLIMB   BLIMB   ACOND1   ACOND2   ACOND3',
     &          '  COST CMANUF GRAPH OF ',(OBJ(I),I=1,6)
             ELSE IF (NWILI.EQ.4) THEN
                WRITE(IFILE,135)
     &          '   DCORE HLIMB   BLIMB   ACOND1   ACOND2   ACOND3',
     &          '   ACOND4',
     &          '  COST CMANUF GRAPH OF ',(OBJ(I),I=1,6)
             ELSE IF (NWILI.EQ.5) THEN
                WRITE(IFILE,135)
     &          '   DCORE HLIMB   BLIMB   ACOND1   ACOND2   ACOND3',
     &          '   ACOND4   ACOND5',
     &          '  COST CMANUF GRAPH OF ',(OBJ(I),I=1,6)
             ELSE IF (NWILI.EQ.6) THEN
                WRITE(IFILE,135)
     &          '   DCORE HLIMB   BLIMB   ACOND1   ACOND2   ACOND3',
     &          '   ACOND4   ACOND5   ACOND6',
     &          '  COST CMANUF GRAPH OF ',(OBJ(I),I=1,6)
             END IF
 1999        CONTINUE
          ELSE
             ISTEG2=ISTEG2+1
             IFILE=IFILE1+(ISTEG2/IRANGE)
             CMANUF=RES79(241,3)+RES79(241,5)+RES79(241,6)
             WRITE(IFILE,145) ISTEG2,DCORE,HLIMB,BLIMB,
     &                     (ACOND(IWDG)*THOU,IWDG=1,NWILI),
     &                     COST/1000000.,CMANUF/1000000.
          END IF
       END IF
*
       write(*,*) "PLIMB FUNK",PLIMB
       print*,'going out of first funk'
       RETURN
   10  FORMAT(' ',I4,A,2(F5.0,2A))
  135  FORMAT(' ',15(A))
  145  FORMAT(' ',I4,1X,2(F5.3,1X),F5.3,1X,
     & 6(F8.5,1X),1X,F10.6,1X,F19.6)
       END
