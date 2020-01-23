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
*      Subroutine Mass92
*      -----------------
*
C...   Title:  Calculation of masses, simplified routines
*
*      Written: 92-01-07 by B-G Bladh  , SETFO/TS
*
************************************************************************
*
       SUBROUTINE MASS92
*
C... Declarations
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0861(1)    ,  GWINCO(1)     ),
     &  (XZ0981(1)    ,  RAACON(1)     ),
     &  (XZ1464       ,  GRAD          ),
     &  (XZ2737       ,  GOIL          )
*
       EQUIVALENCE
     &  (XZ0581(1)    ,  UARR   (1)    ),
     &  (XZ1380       ,  BBEXAC        ),
     &  (XZ1385       ,  BBLT10        ),
     &  (XZ1390       ,  BBSLIM        ),
     &  (XZ1397       ,  ACOVER        )
       EQUIVALENCE
     &  (XZ1403       ,  ASHELL        ),
     &  (XZ1410       ,  BTANK         ),
     &  (XZ1423       ,  DCOCOV        ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1428       ,  DOUTW         )
       EQUIVALENCE
     &  (XZ1435       ,  DWITA         ),
     &  (XZ1438       ,  EXTANK        ),
     &  (XZ1439       ,  F1TANK        ),
     &  (XZ1440       ,  F2TANK        ),
     &  (XZ1441       ,  F3TANK        )
c       EQUIVALENCE
c     &  (XZ1464       ,  GRAD          )
       EQUIVALENCE
     &  (XZ1468       ,  GVVAH         ),
     &  (XZ0861(1)    ,  GWINCO(1)     ),
     &  (XZ1471       ,  HCORE         ),
     &  (XZ1472       ,  HLIMB         )
       EQUIVALENCE
     &  (XZ1474       ,  HTANK         ),
     &  (XZ1477       ,  HYOKE         ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1491       ,  KCORE         ),
     &  (XZ1492       ,  KTAN79        ),
     &  (XZ1493       ,  KTANK         )
       EQUIVALENCE
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1515       ,  PLIMB         ),
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1530       ,  RLTRAN        ),
     &  (XZ1531       ,  RLYOKE        )
       EQUIVALENCE
     &  (XZ1542       ,  SNOML         ),
     &  (XZ1547       ,  TANKDI        ),
     &  (XZ1550       ,  TCORE         ),
     &  (XZ1557       ,  TWSUP         )
       EQUIVALENCE
     &  (XZ1558       ,  TYPCOR        ),
     &  (XZ1562       ,  VACTP         ),
     &  (XZ1563       ,  VTANK         ),
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ2744       ,  GCONDU        ),
     &  (XZ1452       ,  GACTP         ),
     &  (XZ1455       ,  GCORLA        ),
     &  (XZ2736       ,  GPBLK         ),
     &  (XZ1466       ,  GTRP          ),
     &  (XZ1465       ,  GTANK         ),
     &  (XZ1457       ,  GCOVER        ),
     &  (XZ1453       ,  GCONS         )
*
       REAL
     &           GWINCO(9),ACOVER,RAACON(9),ASHELL,BTANK,DCOCOV,
     &           DCORE,DOUTW, DWITA,EXTANK,FACTOR,FONAN,F1TANK,
     &           F2TANK,F3TANK,GACTP,GCORLA,GCOVER,GCONDU,GCONS,GRAD,
     &           GPBLK,GOIL,ZWOULI,
     &           GTANK,GTRP,GVVAH,HCORE,HLIMB,HTANK,HYOKE,
     &           PLIMB,RLTANK,RLTRAN,RLYOKE,RRAD,SNOML,
     &           TANKDI,TCORE,TWSUP,TYPCOR,VACTP,VTANK,VVAH, VOLCU
*
       INTEGER   IWDG,JFC,KCORE,KTANK,KTAN79,NWILI
*
       DIMENSION UARR(100)
       CHARACTER UARR*4,OPTTNK*4
*
       LOGICAL   BBPRNT,BBEXAC, BBLT10,BBSLIM
*
       DATA      BBPRNT /.FALSE./,
     &           FACTOR /1.E+3/
*
C... Mass and volume of winding conductors
       GCONDU = 0.
       VOLCU  = 0.
       DO 100 IWDG = 1, NWILI
          VOLCU  = VOLCU  + GWINCO(IWDG)/RAACON(IWDG)*ZWOULI
 100      GCONDU = GCONDU + GWINCO(IWDG)*ZWOULI
*
*
C... Yoke clamps
*
       CALL PBALK(BBPRNT,JFC,ANINT(DCORE*FACTOR),HYOKE*FACTOR,
     &            HLIMB*FACTOR,PLIMB*FACTOR,ANINT(TWSUP*FACTOR),
     &            BBEXAC,SNOML/1.E+6,RLYOKE*FACTOR,
     &            TCORE*FACTOR,KCORE,NINT(TYPCOR),BBSLIM,GPBLK)
*
C... Calculation of tank dimensions
*
       OPTTNK=UARR(20)
*
C... Set tank dimensions
*
C,,, If they are to be optimised
*
       IF (OPTTNK(1:1).EQ.'Y') THEN
          RLTANK=RLTRAN+EXTANK
          BTANK=DOUTW+2.*DWITA
          HTANK=HCORE+DCOCOV
       END IF
*
C... Set tank dimensions to zero
*
C,,, NO TANK
*
       IF(KTANK.EQ.0) THEN
          RLTANK=0.
          BTANK=0.
          HTANK=0.
       END IF
       ACOVER=(F1TANK*RLTANK+F2TANK*BTANK)*BTANK
       ASHELL=(RLTANK+F3TANK*BTANK)*HTANK*2.
       IF(KTANK.EQ.0) ACOVER=0.
       IF(KTANK.EQ.0) ASHELL=0.
       VTANK=ACOVER*HTANK
       VVAH=GVVAH/1250.
       RRAD=GRAD/500.
       IF(KTANK.NE.0)
*
C... Tank mass
*
     & CALL TANK79(BBPRNT,JFC,HTANK,BTANK,RLTANK,VVAH,RRAD,VTANK,
     &             ASHELL,ACOVER,KTAN79,TANKDI,GTANK,BBLT10,BBEXAC)
*
C... Cover
*
       CALL COVMA (ACOVER, GCOVER, KTAN79)
*
C... Volume active part
*
       VACTP = VOLCU + (GCORLA+GPBLK)*0.13E-3
*
C... Free oil:
*      Formula from MNL79 mass calculations
       GOIL  = 960.*(0.98*VTANK - VACTP)
*
C... MASS  Conservator(1st part), pipes to conserv. (2nd part)
*      Formulas from MNL79 mass calculations
*
       GCONS = 0.129*GOIL**0.9  + 1.21E-5*GOIL**1.5
*
       GACTP = GCONDU+GCORLA+GPBLK
       GTRP  = GACTP+GTANK+GCOVER+GCONS
*
       RETURN
       END
