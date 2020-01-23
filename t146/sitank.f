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
*      Subroutine SITANK
*      -----------------
*
C...   Title:  Convert tank data from indata fields to SI-units
C...           and transfer them to internal variables
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE SITANK
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ0301(1)    ,  IARR   (1)    ),
     &  (XZ0581(1)    ,  UARR   (1)    ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1390       ,  BBSLIM        ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1410       ,  BTANK         )
       EQUIVALENCE
     &  (XZ1411       ,  BTANKM        ),
     &  (XZ1423       ,  DCOCOV        ),
     &  (XZ1435       ,  DWITA         ),
     &  (XZ1438       ,  EXTANK        ),
     &  (XZ1474       ,  HTANK         )
       EQUIVALENCE
     &  (XZ1475       ,  HTANKM        ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1493       ,  KTANK         ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1529       ,  RLTNKM        )
*
       REAL      AARR(200),BTANK,BTANKM,DCOCOV,DWITA,EXTANK,EXTTA,
     &           HTANK,HTANKM,RLTANK,RLTNKM,XTANK
*
       INTEGER   IARR(100),JFC,KTANK,ISTOP
*
       DIMENSION UARR(100)
       CHARACTER UARR*4,OPTTNK*4
*
       LOGICAL   BBSLIM,BBTS,BBERR,BBHELP(10)
*
CC*SEBL End of the declarations block.
*
       KTANK=IARR(53)
*
C... Illegal tank type
*
       IF (KTANK.LT.0.OR.KTANK.GE.40) THEN
           CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &               'SITANK:ILLEGAL TANK TYPE')
*....      Backtracking
           IF(ISTOP.EQ.1) RETURN
       ENDIF
*
       DWITA=AARR(151)/1.E+3
       EXTTA=AARR(152)/1.E+3
       DCOCOV=AARR(153)/1.E+3
*
       XTANK=0.5
       IF (KTANK.EQ.2)               XTANK=0.4
       IF (KTANK.EQ.4.OR.KTANK.EQ.5) XTANK=0.45
*
       IF (BBSLIM)      EXTANK=EXTTA+XTANK
       IF (.NOT.BBSLIM) EXTANK=EXTTA+2.*DWITA
*
       OPTTNK=UARR(20)
*
C... When the tank dimensions have been fixed ( OPTTNK = 'NO' )
C... then the additional tank-distances are assumed to be the actual
C... tank-dimensions.
*
C,,, If fixed tank dimensions
*
       IF (OPTTNK(1:1).EQ.'N') THEN
          HTANK=DCOCOV
          BTANK=DWITA
          RLTANK=EXTTA
       END IF
*
       HTANKM=AARR(174)/1.E+3
       BTANKM=AARR(175)/1.E+3
       RLTNKM=AARR(176)/1.E+3
*
       RETURN
       END
