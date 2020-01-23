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
*      Subroutine PRWIDA
*      -----------------
*
C...   Title:  Prints tables of winding data and
C...           certain measures and masses on page 2.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE PRWIDA
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       REAL           RES79(241,6)
       COMMON/KONSRE/ RES79
*
       EQUIVALENCE
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0671(1)    ,  BDUCT  (1)    )
       EQUIVALENCE
     &  (XZ0681(1)    ,  BPART  (1)    ),
     &  (XZ0731(1)    ,  CURRUT (1)    ),
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ0751(1)    ,  DYOKE  (1)    ),
     &  (XZ0791(1)    ,  FILLF  (1)    ),
     &  (XZ0861(1)    ,  GWINCO (1)    )
       EQUIVALENCE
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ0991(1)    ,  RPART  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    )
       EQUIVALENCE
     &  (XZ1011(1)    ,  RWIND  (1)    ),
     &  (XZ1041(1)    ,  STRWMM (1)    ),
     &  (XZ1051(1)    ,  STRWMP (1)    ),
     &  (XZ1061(1)    ,  TCOV1  (1)    ),
     &  (XZ1071(1)    ,  TCOV2  (1)    )
       EQUIVALENCE
     &  (XZ1101(1)    ,  VOLTUT (1)    ),
     &  (XZ1121(1)    ,  WLOSSM (1)    ),
     &  (XZ1161(1)    ,  ZCODU  (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    )
       EQUIVALENCE
     &  (XZ1397       ,  ACOVER        ),
     &  (XZ1404       ,  BDRAG         ),
     &  (XZ1410       ,  BTANK         ),
     &  (XZ1427       ,  DKSL          ),
     &  (XZ1452       ,  GACTP         )
       EQUIVALENCE
     &  (XZ1453       ,  GCONS         ),
     &  (XZ1454       ,  GCOOL         ),
     &  (XZ1455       ,  GCORLA        ),
     &  (XZ1456       ,  GCORN         ),
     &  (XZ1457       ,  GCOVER        )
       EQUIVALENCE
     &  (XZ1459       ,  GLIMBM        ),
     &  (XZ1460       ,  GLIMBY        ),
     &  (XZ2737       ,  GOIL          ),
     &  (XZ1465       ,  GTANK         ),
     &  (XZ1466       ,  GTRP          )
       EQUIVALENCE
     &  (XZ1470       ,  GYOKE         ),
     &  (XZ1471       ,  HCORE         ),
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1474       ,  HTANK         ),
     &  (XZ1477       ,  HYOKE         )
       EQUIVALENCE
     &  (XZ1483       ,  MNLY          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1513       ,  PI            ),
     &  (XZ1515       ,  PLIMB         )
       EQUIVALENCE
     &  (XZ1518       ,  PLSL          ),
     &  (XZ1526       ,  RDRAG         ),
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1531       ,  RLYOKE        ),
     &  (XZ1551       ,  TDRAG         ),
     &  (XZ1458       ,  GFAN          ),
     &  (XZ1464       ,  GRAD          ),
     &  (XZ1468       ,  GVVAH         ),
     &  (XZ1461       ,  GOFWF         )
       EQUIVALENCE
     &  (XZ1562       ,  VACTP         ),
     &  (XZ1563       ,  VTANK         ),
     &  (XZ2245(1)    ,  WLOSSE (1)    ),
     &  (XZ2254(1)    ,  WLOSSR (1)    ),
     &  (XZ2271(1)    ,  FLUXMD (1)    )
       EQUIVALENCE
     &  (XZ2281(1)    ,  FLUXMW (1)    ),
     &  (XZ2298(1)    ,  NCOL   (1)    ),
     &  (XZ2567       ,  BBLAY         ),
     &  (XZ2586(1)    ,  ZDISC  (1)    ),
     &  (XZ2593(1)    ,  ZPART  (1)    ),
     &  (XZ2594(1)    ,  ZTUDI  (1)    ),
     &  (XZ3390(1)    ,  WDGFUN (1)    )
       EQUIVALENCE
     &  (XZ2749       ,  PNDUCT        ),
     &  (XZ2750       ,  PNWIND        ),
     &  (XZ2751       ,  PNYOKE        )
*
       REAL      TCOV2 (9),RPART(9),FLUXMD(9),FLUXMW(9),ACOND(9),
     &           BDUCT(9),BPART(9),CURDUT(9),CURRUT(9),FILLF(9),
     &           GWINCO(9),HPART(9),HWIND(9),DINN(9),RRWDG(9),RWIND(9),
     &           STRWMM(9),ZWIND(9),STRWMP(9),SWIND(9),VOLTUT(9),
     &           ZCODU(9),WLOSSM(9),DWIND(9),WLOSSE(9),
     &           WLOSSR(9),TCOV1(9),ACOVER,BDRAG,BTANK,DKSL,GACTP,
     &           GCOOL,GCORLA,GCORN,GCOVER,GLIMBM,GLIMBY,
     &           GFAN,GRAD,GVVAH,GOFWF,GCONS,GOIL,
     &           GS,GTANK,GTRP,GWCOT,GXCOOL,GYOKE,HCORE,HLIMB,
     &           HTANK,HYOKE,PI,PL,PLIMB,PLSL,RDRAG,RLTANK,RLYOKE,
     &           TDRAG,VACTP,VTANK,ZTUDI(9),ZDISC(9),ZPART(9),DYOKE(9),
     &           PNDUCT, PNWIND, PNYOKE
*
       INTEGER   KGROUP(9),I,IWDG,JFC,NWILI,NWOULI,MNLY
*
       DIMENSION TARR(100),CUAL(9),TWTYPE(9),NCOL(9),
     &           CBLTYP(9),WTYPE1(9),WDGFUN(9)
       CHARACTER TARR*8,CUAL*4,TWTYPE*4,NCOL*1,
     &           CBLTYP*4,WTYPE1*4,WDGFUN*4
*
       LOGICAL   BBLAY
*
CC*SEBL End of the declarations block.
*
C... Local function definition.
*
CC*SBBL Start of the function definition block.
*
       GS(I)=RES79(I,1)+RES79(I,4)
*
CC*SEBL End of the function definition block.
*
       GWCOT=0.
*
C... Summate conductor masses
*
       DO 299 IWDG=1,NWILI
  299  GWCOT=GWCOT+GWINCO(IWDG)
       GWCOT=GWCOT*FLOAT(NWOULI)
*
       GXCOOL= GFAN + GRAD + GVVAH + GOFWF
*
       WRITE(JFC,80)
*
C... Print the required data
*
       DO 599 IWDG=1,NWILI
       WTYPE1(IWDG)='    '
*
C... Create the required character string
*
       CALL CONCAT(WTYPE1(IWDG),1,NCOL(IWDG),1,1)
       READ(TARR(30+IWDG),70) TWTYPE(IWDG)
       READ(TARR(40+IWDG),70) CUAL(IWDG)
       READ(TARR(60+IWDG),70) CBLTYP(IWDG)
       CURDUT(IWDG)=CURRUT(IWDG)/ACOND(IWDG)
       SWIND(IWDG)=DWIND(IWDG)*PI
  599  DINN(IWDG)=DWIND(IWDG)-RRWDG(IWDG)
*
CC*SOFF Switch OFF structure analyser (Not all calls need be shown)
*
       CALL  PRWT('Winding  ',WTYPE1,JFC,NWILI)
*
CC*SON  Switch ON  structure analyser (Not all calls need be shown)
*
*
C... Print INTEGER data
*
       CALL  PRWI('Terminal ',KGROUP,JFC,NWILI)
*
C... Print CHARACTER data
*
       CALL  PRWT('Wind type',TWTYPE,JFC,NWILI)
       CALL  PRWT('Wind func',WDGFUN,JFC,NWILI)
*
C... Print REAL data (3rd parameter defines No. of decimals)
*
       CALL  PRWA('No. Turns',ZWIND,1,JFC,NWILI)
*
CC*SOFF Switch OFF structure analyser (Not all calls need be shown)
*
       CALL  PRWA('Volts(kV)',VOLTUT,2,JFC,NWILI)
       CALL  PRWA('Current A',CURRUT,0,JFC,NWILI)
*
CC*SON  Switch ON structure analyser (Not all calls need be shown)
*
C... Print REAL data (3rd parameter defines No. of decimals)
C...                 (4th parameter defines a multiplication factor)
*
       CALL  PRWR('Cond area',ACOND,1,1.E+6,JFC,NWILI)
*
CC*SOFF Switch OFF structure analyser (Not all calls need be shown)
*
       CALL  PRWR('A/mm2    ',CURDUT,2,1.E-6,JFC,NWILI)
       CALL  PRWA('Space f  ',FILLF,3,      JFC,NWILI)
       CALL  PRWR('W. height',HWIND,0,1.E+3,JFC,NWILI)
       CALL  PRWR('Yoke dist',DYOKE,0,1.E+3,JFC,NWILI)
       CALL  PRWR('Duct     ',BDUCT,0,1.E+3,JFC,NWILI)
       CALL  PRWR('In. diam ',DINN ,0,1000.,JFC,NWILI)
       CALL  PRWR('Width    ',RRWDG,1,1.E+3,JFC,NWILI)
       CALL  PRWA('Cool duct',ZCODU,1,      JFC,NWILI)
       CALL  PRWR('Mean turn',SWIND,0,1.E+3,JFC,NWILI)
       CALL  PRWA('Ohm/limb ',RWIND,3,      JFC,NWILI)
       CALL  PRWA('Cond mass',GWINCO,0,     JFC,NWILI)
       CALL  PRWT('Material ',CUAL,         JFC,NWILI)
       IF (BBLAY) THEN
          CALL  PRWA('Tot.discs',ZDISC,1,   JFC,NWILI)
          CALL  PRWA('Trns/disc',ZTUDI,2,   JFC,NWILI)
          CALL  PRWA('Parts/trn',ZPART,0,   JFC,NWILI)
       END IF
       CALL  PRWA('Parts RR ',RPART,0,      JFC,NWILI)
       CALL  PRWR('Cond AD  ',HPART,2,1.E+3,JFC,NWILI)
       CALL  PRWR('Cond RD  ',BPART,2,1.E+3,JFC,NWILI)
       CALL  PRWT('PartInsul',CBLTYP,       JFC,NWILI)
       CALL  PRWR('Part Cov ',TCOV1,2,1.E+3,JFC,NWILI)
       CALL  PRWR('Comm.Cov ',TCOV2,2,1.E+3,JFC,NWILI)
       CALL  PRWA('Duct flux',FLUXMD,3,     JFC,NWILI)
       CALL  PRWA('Wind.flux',FLUXMW,3,     JFC,NWILI)
       CALL  PRWR('N/mm2+   ',STRWMP,0,1.E-6,JFC,NWILI)
       CALL  PRWR('N/mm2-   ',STRWMM,0,-1.E-6,JFC,NWILI)
       CALL  PRWR('RII  loss',WLOSSR,1,1.E-3,JFC,NWILI)
       CALL  PRWR('Eddy loss',WLOSSE,1,1.E-3,JFC,NWILI)
       CALL  PRWR('Max. loss',WLOSSM,1,1.E-3,JFC,NWILI)
*
CC*SON  Switch ON structure analyser (Not all calls need be shown)
*
       PL=PLIMB
       IF (NWOULI.EQ.1) PL=0.
       WRITE(JFC,82)
       IF(MNLY.LT.91) THEN
          WRITE(JFC,85) 'Width       ',1.E+3*BTANK,
     &                  'Transp. mass   ',GTRP
       ELSE
          WRITE(JFC,85) 'Width       ',1.E+3*BTANK,
     &                  'Main Components',GTRP
       ENDIF
       WRITE(JFC,84) 'Length      ',1.E+3*RLTANK,
     &               'Conserv+cnsl',GCONS,
     &               'Height      ',1.E+3*HTANK,
     &               'Cooler appr ',GXCOOL,
     &               'Limb height ',1.E+3*HLIMB,
     &               'Oil  106 %  ',GOIL
       WRITE(JFC,86) 'Core height ',1.E+3*HCORE,
     &               'Active part ',VACTP,GACTP
       WRITE(JFC,84) 'Core length ',1.E+3*RLYOKE,
     &               'Conductor   ',GWCOT,
     &               'Yoke height ',1.E+3*HYOKE,
     &               'Core steel  ',GCORLA,
     &               'Limb pitch  ',1.E+3*PL,
     &               '  Limb      ', GLIMBM+GLIMBY,
     &               'Pitch side-l',1.E+3*PLSL,
     &               '  Yoke      ',GYOKE,
     &               'Width side-l',1.E+3*DKSL,
     &               '  Corner    ',GCORN
       WRITE(JFC,88) 'No of fl.pl/limb',NINT(RDRAG),
     &               'Tank        ',VTANK,'m3',GTANK
       WRITE(JFC,89) '  Dimension',1.E+3*BDRAG,1.E+3*TDRAG,
     &               'Cover       ',ACOVER,'m2',GCOVER
       IF (MNLY.GT.91) THEN
          WRITE(JFC, 90) '  PN in ducts.......', PNDUCT
          WRITE(JFC, 90) '  PN in windings....', PNWIND
          WRITE(JFC, 90) '  PN yoke insulation', PNYOKE
          WRITE(JFC, 90) 'TOTAL PN............', PNYOKE+PNWIND+PNDUCT
       ENDIF
*
       RETURN
*
   70  FORMAT(A4)
   80  FORMAT(' ','     W I N D I N G S')
   82  FORMAT(1X/'      D I M E N S I O N S',18X,' M A S S E S ')
   84  FORMAT(6X,A12,F8.0,' mm',14X,A12,F20.0,' kg')
   85  FORMAT(6X,A12,F8.0,' mm',14X,A15,F17.0,' kg')
   86  FORMAT(6X,A12,F8.0,' mm',14X,A12,F8.3,' m3',F9.0,' kg')
   88  FORMAT(6X,A16,I7,14X,A12,F8.3,A3,F9.0,' kg')
   89  FORMAT(6X,A11,F4.0,' *',F3.0,' mm',14X,A12,F8.3,A3,F9.0,' kg')
   90  FORMAT(6X,37X,A20,F12.0,' kg')
       END
