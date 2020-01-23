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
*      Subroutine PREP1
*      ----------------
*
C...       Title:  Assigns values to certain variables :
C...
C...               BBDLI    = (Free Core diameter)
C...               BBHLI    = (Free Limb height  )
C...               BBFLU    = (Free Flux density )
C...               BBREA    = (Free Short-circuit voltage)
C...               BBCURD(I)= (Free current density for winding(I))
C...               BBOPTI   = (No. of free variables is greater than 1)
C...   Assigns values to XA, XREL & GREL for the optimisation routine.
C...   Assigns start values for the optimisation routine if required.
C...   Assigns No. of turns corresponding to term.1, TURNRA start value
C...   Counts the No. of free variables
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 83-06-15 by H.Westberg , ZKAB
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-09-22 by B-G Bladh  , SETFO/TS
*       FABOOS(9) for boosters have been introduced.
*
************************************************************************
*
       SUBROUTINE PREP1
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
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0701(1)    ,  CURDEN (1)    ),
     &  (XZ0711(1)    ,  CURDM  (1)    )
       EQUIVALENCE
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ0971(1)    ,  PRESSM (1)    ),
     &  (XZ1081(1)    ,  TENSM  (1)    ),
     &  (XZ1243(1)    ,  RIDIMW (1)    ),
     &  (XZ1247(1)    ,  RIDIRW (1)    )
       EQUIVALENCE
     &  (XZ1259(1)    ,  SRATE  (1)    ),
     &  (XZ1267(1)    ,  UDIM   (1)    ),
     &  (XZ1301(1)    ,  BBCURD (1)    ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1341(1)    ,  BBRW   (1)    ),
     &  (XZ1375       ,  BBDLI         )
       EQUIVALENCE
     &  (XZ1382       ,  BBFLU         ),
     &  (XZ1384       ,  BBHLI         ),
     &  (XZ1386       ,  BBOPTI        ),
     &  (XZ1389       ,  BBREA         ),
     &  (XZ1391       ,  BBTS          )
       EQUIVALENCE
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1405       ,  BLIMB         ),
     &  (XZ1406       ,  BLTOPM        ),
     &  (XZ1407       ,  BMAXPU        ),
     &  (XZ1411       ,  BTANKM        )
       EQUIVALENCE
     &  (XZ1424       ,  DCOMAX        ),
     &  (XZ1425       ,  DCOMIN        ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1431       ,  DRV           ),
     &  (XZ1437       ,  EPS           )
       EQUIVALENCE
     &  (XZ1442       ,  FEPS          ),
     &  (XZ1444       ,  AF            ),
     &  (XZ1446       ,  FREQ          ),
     &  (XZ1467       ,  GTRPM         ),
     &  (XZ1472       ,  HLIMB         )
       EQUIVALENCE
     &  (XZ1473       ,  HLMBMA        ),
     &  (XZ1475       ,  HTANKM        ),
     &  (XZ1478       ,  HLMBMI        ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1498       ,  NFREE         ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1516       ,  PLOADM        )
       EQUIVALENCE
     &  (XZ1519       ,  IOPT          ),
     &  (XZ1520       ,  PNOLOM        ),
     &  (XZ1529       ,  RLTNKM        ),
     &  (XZ1539       ,  SEQU2W        ),
     &  (XZ1543       ,  SOUNDM        )
       EQUIVALENCE
     &  (XZ1555       ,  TURNRA        ),
     &  (XZ1561       ,  USHORE        ),
     &  (XZ1743(1)    ,  XA     (1)    ),
     &  (XZ1755(1)    ,  XREL   (1)    ),
     &  (XZ2307(1)    ,  GREL   (1)    )
       EQUIVALENCE
     &  (XZ2346(1)    ,  G      (1)    ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ2731(1)    ,  FABOOS (1)    )
*
       REAL      XA(30),XREL(30),AARR(200),CURDM(9),AF,BLIMB,BLTOPM,
     &           CURDEN(9),ACOND(9),DARR(100),GREL(96),G(96),SRATE(4),
     &           RIDIMW(4),RIDIRW(4),PRESSM(9),TENSM(9),UDIM(4),FEPS,
     &           BMAXPU,BSTART,BTANKM,DCOMAX,DCOMIN,DCORE,DRV,EPS,EX,
     &           FREQ,GTRPM,FACTOR,HLIMB,HLMBMA,HTANKM,PLOADM,PNOLOM,
     &           RIDI,RLTNKM,SEQU2W,SOUNDM,TURNRA,USHORE,HLMBMI,
     &           FABOOS(9)
*
       INTEGER   KGROUP(9),IG,IOPT,IWDG,IX,KGRP,NFREE,NWILI,JFC,
     &           ISTOP
*
       LOGICAL   BBRW(9),BBVFR,BBDLI,BBHLI,BBFLU,BBREA,BBOPTI,
     &           BBCURD(9),BBSTRT,BBTS,BBERR,BBHELP(10)
*
CC*SEBL End of the declarations block.
*
C... Problem too big for time-sharing
*
       IF (BBTS.AND.NWILI.GT.7) THEN
           CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     & 'PREP1:T146 can handle up to 6 windings only: memory limitation')
*....      Backtracking
           IF(ISTOP.EQ.1) RETURN
       ENDIF

       BBSTRT=.TRUE.
*
C... UK - Short-circuit voltage
C... IF BOTH Core diameter AND limb height are given
C... then the REACTANCE ( if given ) is ignored
*
       IF (.NOT.(BBHLI.OR.BBDLI)) BBREA=.TRUE.
       IF (.NOT.BBREA) USHORE=AARR(4)/1.E+2
*
       BBOPTI=.FALSE.
       NFREE=0.
*
C... Set XREL(*) to zero
*
       DO 299 IX=1,30
  299  XREL(IX)=0.
*
C... Core diameter
*
       IF (DCORE.LT.1.E-3) DCORE=ABS(AARR(143))/1.E+3
*
C... Set core diameter
*
C,,, If not given
*
       IF (DCORE.LT.1.E-3) THEN
          BBSTRT=.FALSE.
          DCORE=0.37*SQRT(SQRT(SEQU2W/1.E+3)/FREQ)
       END IF
*
C... Determine core diameter
*
C,,, If necessary
*
       IF (BBDLI) THEN
          NFREE=NFREE+1
          IF (DCORE.LT.DCOMIN) DCORE=DCOMIN
          IF (DCORE.GT.DCOMAX) DCORE=DCOMAX
          XREL(1)=0.5*DCORE
       END IF
       XA(1)=DCORE
*
C... Limb height
*
       IF (HLIMB.LT.0.1) HLIMB=ABS(DARR(20))/1.E+3
       write(*,*) 'prep1 line 190', HLIMB
       IF (ABS(HLIMB-ABS(DARR(20)/1.E+3)).GT.1.E-4)
     &                   HLIMB=ABS(DARR(20))/1.E+3
       write(*,*) 'prep1 line 193', HLIMB
       IF (HLIMB.LT.0.1.AND.BBREA) BBSTRT=.FALSE.
       IF (HLIMB.LT.0.1) HLIMB=2.+2.*SRATE(1)/1.E+9
       write(*,*) 'prep1 line 196', HLIMB
*
C... Determine limb height
*
C,,, If necessary
*
       IF (BBHLI) THEN
          NFREE=NFREE+1
          XREL(2)=1.0*HLIMB
       END IF
       XA(2)=HLIMB
*
C... Flux density
*
       IF (BLIMB.LT.0.1) BLIMB=ABS(AARR(146))
       IF (BBVFR) BLIMB=BLIMB/BMAXPU
       IF (BLIMB.LT.0.1) THEN
          BBSTRT=.FALSE.
          BLIMB=BLTOPM*0.918
       END IF
*
C... Determine flux density
*
C,,, If necessary
*
       IF (BBFLU) THEN
          NFREE=NFREE+1
          XREL(3)=0.5*BLIMB
          BSTART=AMIN1(BLTOPM-0.01,BLIMB)
          IF (BSTART.LT.0.1) BSTART=BLTOPM*0.918
          BLIMB=BSTART
       END IF
       XA(3)=BLIMB
*
C... No. of turns corresponding to terminal 1 at nominal voltage
C... start value
C... Exact calculation in CORE1N
*
CC*SBBL Start of the TURNRA block
*
       TURNRA=UDIM(1)/(3.1*BLIMB*(DCORE**2)*AARR(2))
*
CC*SEBL End of the TURNRA block
*
C... Windings
*
C... Determine whether conductor RD for each winding has been given
*
       DO 1099 IWDG=1,NWILI
*
       BBCURD(IWDG)=.FALSE.
       IF (AARR(60+IWDG).LT.1.E-3.AND.
     &     AARR(90+IWDG).LT.1.E-3) BBCURD(IWDG)=.TRUE.
*
       KGRP=KGROUP(IWDG)
       RIDI=RIDIMW(KGRP)
       IF (BBRW(IWDG)) RIDI=RIDIRW(KGRP)
*
C... Determine conductor area and current density
*
C,,, Winding to be optimised
*
       IF (BBCURD(IWDG)) THEN
          NFREE=NFREE+1
          CURDEN(IWDG)=CURDM(IWDG)*0.6
CCCCCCCCC IF (ACOND(IWDG).LT.1.E-8) ACOND(IWDG)=ABS(AARR(60+IWDG))/1.E+6
          IF (ACOND(IWDG).LT.1.E-8) THEN
             BBSTRT=.FALSE.
             ACOND(IWDG)=RIDI/CURDEN(IWDG)/FABOOS(IWDG)
          END IF
          XREL(3+IWDG)=1.0*ACOND(IWDG)
*
C... Winding defined
*
       ELSE
*
C... Determine Area
*
C,,, Area given
*
          IF (AARR(60+IWDG).GT.1.E-8) THEN
             ACOND(IWDG)=AARR(60+IWDG)/1.E+6
*
C... Area not given
*
          ELSE
             CURDEN(IWDG)=1.E+6*AARR(50+IWDG)
             ACOND(IWDG)=RIDI/CURDEN(IWDG)/FABOOS(IWDG)
          END IF
       END IF
*
       XA(3+IWDG)=ACOND(IWDG)
 1099  CONTINUE
*
       IF (NFREE.GT.0) BBOPTI=.TRUE.
*
C... Initialisation of the GREL-vector
*
       DO 2199 IG=1,96
       G(IG)=0.
 2199  GREL(IG)=0.
*
C... Allocation of values to the GREL-vector
*
CC*SBBL Start of the allocation block
*
       FACTOR=1.E+2
       IF (PLOADM.GT.0) GREL( 1)=PLOADM/FACTOR
       IF (PNOLOM.GT.0) GREL( 2)=PNOLOM/FACTOR
             IF (BBDLI) GREL( 3)=((DCOMIN+DCOMAX)/2.)/FACTOR
                        GREL( 4)=0.
                        GREL( 5)=HTANKM/FACTOR
             IF (BBHLI) GREL( 6)=HLMBMA/FACTOR
                        GREL( 7)=BTANKM/FACTOR
                        GREL( 8)=RLTNKM/FACTOR
                        GREL( 9)=GTRPM/FACTOR
       IF (SOUNDM.GT.0) GREL(10)=SOUNDM/FACTOR
*
C--- Equality needed for fixed reactance
*
       IF (.NOT.BBREA)  GREL(11)=(-1.)*USHORE/FACTOR
                        GREL(12)=BLTOPM/FACTOR
*
CC*SEBL End of the allocation block
*
C... Allocation of the GREL-vector for each winding
*
       DO 2299 IWDG=1,NWILI
*
C... Allocate the GREL-vector
*
C,,, If the conductor RD is given
*
       IF (BBCURD(IWDG)) THEN
                        GREL(13+3*(IWDG-1))=CURDM(IWDG)/FACTOR
                        GREL(14+3*(IWDG-1))=(-1.)*PRESSM(IWDG)/FACTOR
                        GREL(15+3*(IWDG-1))=TENSM(IWDG)/FACTOR
       END IF
 2299  CONTINUE
*
C... Allocation of optimisation parameters to LMIN
*
CC*SBBL Start of the optimisation parameter block
*
       IOPT=1
       IF (BBSTRT) IOPT=2
*
       EX=FLOAT(-IOPT-2)
       IF (IOPT.GT.2) FEPS=0.
C****  IF (IOPT.LE.2) FEPS=10.**(EX)
       FEPS=0.001
*
       EX=FLOAT(1-IOPT)
C****  AF=10.**(EX)
       AF=0.1
*
C****  DRV=AF/1000.
       DRV=0.01
*
       EX=FLOAT(-IOPT)
       IF (EPS.EQ.0) EPS=AMIN1(0.001,10.**(EX))
*
CC*SEBL End of the optimisation parameter block
*
       RETURN
       END
