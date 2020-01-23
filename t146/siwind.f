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
*      Subroutine SIWIND
*      -----------------
*
C...   Title:  Convert winding indata to SI-units and
C...           transfer them to internal variables.
C...           Relevant indata checks are performed.
C...           The following variables are assigned values :
C....
C...           KWITYP,BBRR,BBRW,SIGM,RAACON.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-09-11 by B-G Bladh  , SETFO/TS
*       Reduction of max current density based on rated power=
*       self cooled power  (FONAN > 1.)
*      Revised: 91-10-10 by B-G Bladh  , SETFO/TS
*       Check for BOOSTER turns ratio transferred to FABOOS(IWDG)
*       Logical BBOOSW is specified for each winding.
*      Variable SIGMA-Cu depending on temperature is included.
*
************************************************************************
*
       SUBROUTINE SIWIND
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
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ0671(1)    ,  BDUCT  (1)    )
       EQUIVALENCE
     &  (XZ0681(1)    ,  BPART  (1)    ),
     &  (XZ0711(1)    ,  CURDM  (1)    ),
     &  (XZ0721(1)    ,  IPISOL (1)    ),
     &  (XZ0751(1)    ,  DYOKE  (1)    )
       EQUIVALENCE
     &  (XZ0791(1)    ,  FILLF  (1)    ),
     &  (XZ0801(1)    ,  FRACT  (1)    ),
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ0891(1)    ,  KCODE  (1)    ),
     &  (XZ0901(1)    ,  KGROUP (1)    )
       EQUIVALENCE
     &  (XZ0911(1)    ,  KWITYP (1)    ),
     &  (XZ0921(1)    ,  NGROUP (1)    ),
     &  (XZ0971(1)    ,  PRESSM (1)    ),
     &  (XZ0981(1)    ,  RAACON (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    )
       EQUIVALENCE
     &  (XZ1031(1)    ,  SIGM   (1)    ),
     &  (XZ1061(1)    ,  TCOV1  (1)    ),
     &  (XZ1071(1)    ,  TCOV2  (1)    ),
     &  (XZ1081(1)    ,  TENSM  (1)    ),
     &  (XZ1091(1)    ,  TSPIN  (1)    )
       EQUIVALENCE
     &  (XZ1287(1)    ,  USURG  (1)    ),
     &  (XZ1331(1)    ,  BBRR   (1)    ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1445       ,  FONAN         ),
     &  (XZ1486       ,  JFC           )
       EQUIVALENCE
     &  (XZ1341(1)    ,  BBRW   (1)    ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1524       ,  RAAAL         ),
     &  (XZ1525       ,  RAACU         )
       EQUIVALENCE
     &  (XZ1540       ,  SIGMAL        ),
     &  (XZ1541       ,  SIGMCU        ),
     &  (XZ2584(1)    ,  EXTRAR (1)    ),
     &  (XZ2585(1)    ,  HCLAC  (1)    ),
     &  (XZ2592(1)    ,  ZRR    (1)    ),
     &  (XZ2593(1)    ,  ZPART  (1)    ),
     &  (XZ3390(1)    ,  WDGFUN (1)    ),
     &  (XZ2731(1)    ,  FABOOS (1)    ),
     &  (XZ2733(1)    ,  BBOOSW (1)    ),
     &  (XZ2730       ,  TRBOOS        ),
     &  (XZ2734       ,  TELOSS        )
*
       REAL      AARR(200),BARR(100),DARR(100),FRACT(9),CURDM(9),
     &           RRWDG(9),DYOKE(9),HPART(9),BPART(9),SIGM(9),RAACON(9),
     &           USURG(4),TSPIN(9),TENSM(9),PRESSM(9),FILLF(9),RAAAL,
     &           BDUCT(9),EXTRAR(9),HCLAC(9),ZRR(9),ZPART(9),RAACU,
     &           SIGMAL,SIGMCU,TCOV1(9),TCOV2(9),PSPIN,UPGRED,FONAN,
     &           FABOOS(9), TRBOOS, TELOSS
*
       INTEGER   IARR(100),KGROUP(9),KWITYP(9),KCODE(9),NGROUP(9),
     &           IPISOL(9),IWDG,JFC,NG,NWILI
*
       LOGICAL   BBRW(9),BBRR(9),BBERR,BBHELP(10),BBTS,BBOOSW(9)
*
       DIMENSION TARR(100),NCOL(9), WDGFUN(9)
       CHARACTER TARR*8,NCOL*1, WDGFUN*4
*
       DATA NCOL/'A','B','C','D','E','F','G','H','I'/
*
CC*SEBL End of the declarations block.
*
C...  Calculation of reduction factor for current density if
C...     FONAN > 1 (rated power = self cooled power)
      UPGRED = 1.
      FONAN  = AARR(154)
      IF (FONAN.GT.3.) FONAN = FONAN/100.
      IF (FONAN.GT.1.) UPGRED= 1./FONAN
*
C... For each winding
*
       DO 299 IWDG=1,NWILI
       KGROUP(IWDG)=IARR(20+IWDG)
*
C... Illegal reference to terminal
*
       IF (KGROUP(IWDG).LE.0.OR.KGROUP(IWDG).GT.NG)
     &    CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     & 'SIWIND: Winding '//NCOL(IWDG)//' has an invalid terminal node')
*
C... Determine the winding type.
*
       KWITYP(IWDG)=0
       IF (TARR(30+IWDG).EQ.'S       '
     & .OR.TARR(30+IWDG).EQ.'S1      '
     & .OR.TARR(30+IWDG).EQ.'S2      '
     & .OR.TARR(30+IWDG).EQ.'S3      '
     & .OR.TARR(30+IWDG).EQ.'S4      '
     & .OR.TARR(30+IWDG).EQ.'S5      '
     & .OR.TARR(30+IWDG).EQ.'S6      '
     & .OR.TARR(30+IWDG).EQ.'S7      '
     & .OR.TARR(30+IWDG).EQ.'S8      '
     & .OR.TARR(30+IWDG).EQ.'S9      '
     & .OR.TARR(30+IWDG).EQ.'S10     '
     & .OR.TARR(30+IWDG).EQ.'S11     '
     & .OR.TARR(30+IWDG).EQ.'S12     ') KWITYP(IWDG)=1
*
       IF (TARR(30+IWDG).EQ.'L       '
     & .OR.TARR(30+IWDG).EQ.'L1      '
     & .OR.TARR(30+IWDG).EQ.'L2      '
     & .OR.TARR(30+IWDG).EQ.'L3      ') KWITYP(IWDG)=2
*
       IF (TARR(30+IWDG).EQ.'CD      ') KWITYP(IWDG)=3
*
       IF (TARR(30+IWDG).EQ.'SD      ') KWITYP(IWDG)=4
*
       IF (TARR(30+IWDG).EQ.'D2      ') KWITYP(IWDG)=5
*
       IF (TARR(30+IWDG).EQ.'SLS     '
     & .OR.TARR(30+IWDG).EQ.'SLS1    '
     & .OR.TARR(30+IWDG).EQ.'SLS2    '
     & .OR.TARR(30+IWDG).EQ.'SLS3    '
     & .OR.TARR(30+IWDG).EQ.'SLS4    '
     & .OR.TARR(30+IWDG).EQ.'SLS5    '
     & .OR.TARR(30+IWDG).EQ.'SLS6    ') KWITYP(IWDG)=6
*
       IF (TARR(30+IWDG).EQ.'SLL     '
     & .OR.TARR(30+IWDG).EQ.'SLL1    '
     & .OR.TARR(30+IWDG).EQ.'SLL2    ') KWITYP(IWDG)=7
*
C... Winding type illegally stated
*
       IF (KWITYP(IWDG).EQ.0)
     &    CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     & 'SIWIND: Winding '//NCOL(IWDG)//' has an invalid winding type')
*
C... Determine the number of strands in radial direction
*
       ZRR(IWDG)=1
       ZPART(IWDG)=0
       IF (TARR(30+IWDG).EQ.'S       '
     & .OR.TARR(30+IWDG).EQ.'S1      ') ZPART(IWDG)=1
       IF (TARR(30+IWDG).EQ.'S2      ') ZPART(IWDG)=2
       IF (TARR(30+IWDG).EQ.'S3      ') ZPART(IWDG)=3
       IF (TARR(30+IWDG).EQ.'S4      ') ZPART(IWDG)=4
       IF (TARR(30+IWDG).EQ.'S5      ') ZPART(IWDG)=5
       IF (TARR(30+IWDG).EQ.'S6      ') ZPART(IWDG)=6
       IF (TARR(30+IWDG).EQ.'S7      ') ZPART(IWDG)=7
       IF (TARR(30+IWDG).EQ.'S8      ') ZPART(IWDG)=8
       IF (TARR(30+IWDG).EQ.'S9      ') ZPART(IWDG)=9
       IF (TARR(30+IWDG).EQ.'S10     ') ZPART(IWDG)=10
       IF (TARR(30+IWDG).EQ.'S11     ') ZPART(IWDG)=11
       IF (TARR(30+IWDG).EQ.'S12     ') ZPART(IWDG)=12
*
       IF (TARR(30+IWDG).EQ.'L       '
     & .OR.TARR(30+IWDG).EQ.'L1      ') ZPART(IWDG)=1
       IF (TARR(30+IWDG).EQ.'L2      ') ZPART(IWDG)=2
       IF (TARR(30+IWDG).EQ.'L3      ') ZPART(IWDG)=3
*
       IF (TARR(30+IWDG).EQ.'CD      ') ZPART(IWDG)=1
*
       IF (TARR(30+IWDG).EQ.'SD      ') ZPART(IWDG)=1
*
       IF (TARR(30+IWDG).EQ.'D2      ') ZPART(IWDG)=1
*
       IF (TARR(30+IWDG).EQ.'SLS     '
     & .OR.TARR(30+IWDG).EQ.'SLS1    ') ZPART(IWDG)=1
       IF (TARR(30+IWDG).EQ.'SLS2    ') ZPART(IWDG)=2
       IF (TARR(30+IWDG).EQ.'SLS3    ') ZPART(IWDG)=3
       IF (TARR(30+IWDG).EQ.'SLS4    ') ZPART(IWDG)=4
       IF (TARR(30+IWDG).EQ.'SLS5    ') ZPART(IWDG)=5
       IF (TARR(30+IWDG).EQ.'SLS6    ') ZPART(IWDG)=6
*
       IF (TARR(30+IWDG).EQ.'SLL     '
     & .OR.TARR(30+IWDG).EQ.'SLL1    ') ZPART(IWDG)=1
       IF (TARR(30+IWDG).EQ.'SLL2    ') ZPART(IWDG)=2
*
C... Number of strands in the redial direction not given
*
       IF (ZPART(IWDG).EQ.0)
     &    CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &      'SIWIND: Number of radial strands not given'//
     &      ' for winding '//NCOL(IWDG))
*
       IPISOL(IWDG)=0
       IF (TARR(60+IWDG).EQ.'SP    ') IPISOL(IWDG)=1
       IF (TARR(60+IWDG).EQ.'OR    ') IPISOL(IWDG)=2
       IF (TARR(60+IWDG).EQ.'LR    ') IPISOL(IWDG)=3
       IF (TARR(60+IWDG).EQ.'TC    ') IPISOL(IWDG)=1
*
C... Cable type illegally stated
*
       IF (IPISOL(IWDG).EQ.0)
     &    CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     & 'SIWIND: Winding '//NCOL(IWDG)//' has an invalid cable type')
*
C... Function code (1=Main,2=Reg.,3=Coarse step reg w , 4=Tapped main)
*
       KCODE(IWDG)=IARR(30+IWDG)
*
C... Illegal function code
*
       IF (KCODE(IWDG).LE.0.OR.KCODE(IWDG).GT.4)
     &    CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     & 'SIWIND: Winding '//NCOL(IWDG)//' has an invalid function code')
*
       BBRW(IWDG)=.FALSE.
       IF (KCODE(IWDG).EQ.2.OR.KCODE(IWDG).EQ.3) BBRW(IWDG)=.TRUE.
*
       FRACT(IWDG)=AARR(40+IWDG)
       NGROUP(IWDG)=IARR(40+IWDG)
       CURDM(IWDG)=4.5E+6 * UPGRED
       IF (AARR(50+IWDG).LT.0.)
     &    CURDM(IWDG)=1.E+6*ABS(AARR(50+IWDG)) * UPGRED
*
C... ACOND is calculated in 'PREP1'  after 'VOLTAGE'
*
CC*SBBL Start of calculation block
*
       BDUCT(IWDG)=AARR(70+IWDG)/1.E+3
       FILLF(IWDG)=AARR(80+IWDG)
       IF (AARR(90+IWDG).GT.0.) BBRR(IWDG)=.TRUE.
*
CC*SEBL End of calculation block
*
C... Space factor not given
*
       IF (AARR(80+IWDG).LT.1.E-6.AND.AARR(90+IWDG).LT.1.E-6)
     &    CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     & 'SIWIND: Winding '//NCOL(IWDG)//' has no space factor given')
*
       RRWDG(IWDG)=AARR(90+IWDG)/1.E+3
       DYOKE(IWDG)=AARR(100+IWDG)/1.E+3
       BPART(IWDG)=AARR(110+IWDG)/1.E+3
       EXTRAR(IWDG)=DARR(40+IWDG)/1.E+3
       HCLAC(IWDG)=DARR(50+IWDG)/1.E+3
*
C... Conductor material parameters
*
C,,, Aluminium conductors
*
       IF (TARR(40+IWDG)(1:1).EQ.'A') THEN
          SIGM(IWDG)=SIGMAL * (235. + 75.) / (235. + TELOSS)
          RAACON(IWDG)=RAAAL


*
C... Copper conductors
*
       ELSE IF (TARR(40+IWDG)(1:1).EQ.'C') THEN
          SIGM(IWDG)=SIGMCU * (235. + 75.) / (235. + TELOSS)
          RAACON(IWDG)=RAACU
*
C... Conductor material not recognised
*
       ELSE
          CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     & 'SIWIND: Winding '//NCOL(IWDG)//' conductor material is invalid')
       END IF
*
C... Auxiliary constant for calculation of additional losses
*
       TENSM(IWDG)=1.E+6*AARR(120+IWDG)
       PRESSM(IWDG)=-1.E+6*AARR(130+IWDG)
*
C... Default values for conductor height and paper covering
*
C,,, Winding type 1
*
       IF (KWITYP(IWDG).EQ.1) THEN
          HPART(IWDG)=0.0075
          TSPIN(IWDG)=0.0005
*
C... Winding type 2
*
       ELSE IF (KWITYP(IWDG).EQ.2) THEN
          HPART(IWDG)=0.0075
          TSPIN(IWDG)=0.001
*
C... Winding type 3 or 5
*
       ELSE IF ((KWITYP(IWDG).EQ.3).OR.(KWITYP(IWDG).EQ.5)) THEN
          HPART(IWDG)=0.011
          TSPIN(IWDG)=AMAX1((USURG(KGROUP(IWDG))/1000.)**0.7/60000.,
     &                      0.0005)
*
C... Winding type 4
*
       ELSE IF (KWITYP(IWDG).EQ.4) THEN
          HPART(IWDG)=0.011
          TSPIN(IWDG)=AMAX1((USURG(KGROUP(IWDG))/1000.)**0.7/90000.,
     &                      0.0005)
*
C... Winding type 6
*
       ELSE IF (KWITYP(IWDG).EQ.6) THEN
          HPART(IWDG)=0.015
          TSPIN(IWDG)=0.0018
*
C... Winding type 7
*
       ELSE IF (KWITYP(IWDG).EQ.7) THEN
          HPART(IWDG)=0.015
          TSPIN(IWDG)=0.004
       END IF
*
       IF (BARR(10+IWDG).GT.0.1) HPART(IWDG)=BARR(10+IWDG)/1.E+3
       BARR(10+IWDG)=1.E+3*HPART(IWDG)
       IF (BARR(20+IWDG).GE.0.) TSPIN(IWDG)=BARR(20+IWDG)/1.E+3
*
       IF(IPISOL(IWDG).EQ.2) THEN
          PSPIN=0.15E-3
       ELSE IF(IPISOL(IWDG).EQ.3) THEN
          PSPIN=0.18E-3
       ELSE
          PSPIN=0.5E-3
       END IF
*
       TCOV1(IWDG)=PSPIN
       IF (DARR(30+IWDG).GE.0.) TCOV1(IWDG)=DARR(30+IWDG)/1.E+3
*
       IF(IPISOL(IWDG).EQ.2.OR.IPISOL(IWDG).EQ.3) TCOV1(IWDG)=PSPIN
*
       TCOV2(IWDG)=TSPIN(IWDG)-TCOV1(IWDG)
       IF(IPISOL(IWDG).EQ.2.OR.IPISOL(IWDG).EQ.3)
     &                                TCOV2(IWDG)=TSPIN(IWDG)
*
C... Make covering non-negative
*
       IF(TCOV1(IWDG).LT.0.) TCOV1(IWDG)=0.
       IF(TCOV2(IWDG).LT.0.) TCOV2(IWDG)=0.
*
C..    Booster check
       IF(WDGFUN(IWDG) .EQ.'RB ' .OR.
     &    WDGFUN(IWDG) .EQ.'RCB'     ) THEN
             FABOOS(IWDG) = TRBOOS
       ELSE
             FABOOS(IWDG) = 1.
       ENDIF
C..  Booster losses are added to the regulating winding with
*    BBOOSW=.T. . Only one winding shall have that addition.
*    This is the reason why not 'RCB'-winding shall have
*    BBOOS=.T.
       IF(WDGFUN(IWDG) .EQ.'RB ') THEN
             BBOOSW(IWDG) = .TRUE.
       ELSE
             BBOOSW(IWDG) = .FALSE.
       ENDIF
*
  299  CONTINUE
       RETURN
       END
