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
*      Subroutine PAGECR
*      -----------------
*
C...   TITLE:  PRINTOUT OF COSTS IN MNL-87
*
*      Written: XX-XX-XX by A.N.Other     , XXXX
*      Revised: 85-10-18 by Ron Bell      , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell      , TRAFO/IK
*      Revised: 89-01-04 by Per Sãderberg , TRAFO/IK
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*             Lay out of report is changed.
************************************************************************
*
       SUBROUTINE PAGECR
*
C... Declarations
*
CC*SBBL Start of the declarations block
*
       include'com1.h'
*
       REAL           RES79 (241,6)
       COMMON/KONSRE/ RES79
*
       EQUIVALENCE
     &  (XZ1283(1)    ,  UNLINE (1)    ),
     &  (XZ1414       ,  CLOSSV        ),
     &  (XZ1415       ,  COST          ),
     &  (XZ1416       ,  COSTTP        ),
     &  (XZ1417       ,  CPUFIX        )
       EQUIVALENCE
     &  (XZ1422       ,  CTROVN        ),
     &  (XZ1483       ,  MNLY          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1509       ,  NWOULI        )
       EQUIVALENCE
     &  (XZ1510       ,  P0LOSS        ),
     &  (XZ1514       ,  PKLOSS        ),
     &  (XZ1539       ,  SEQU2W        )
       EQUIVALENCE
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ1632(1)    ,  RUBCH  (1)    ),
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1689(1,1,1),  URC    (1,1,1))
       EQUIVALENCE
     &  (XZ2266       ,  CSTPSW        ),
     &  (XZ2296       ,  PG            ),
     &  (XZ2297       ,  PK            )
*
       REAL      RK(5,2,3),P00(3),URC(4,4,3),UNLINE(4),CDSCNT,CFIXED,
     &           CLOSSV,CMANUF,COLE,COST,COSTTP,CPUFIX,
     &           CS,CSTPSW,CTROVN,HELP,PG,PK,PKLOSS,P0LOSS,
     &           SEQU2W,X1,X2,ZWOULI
*
       INTEGER   I,JFC,KOD,KODPT,MNLY,NPK,NWOULI
*
       DIMENSION RUBCH(15)
       CHARACTER RUBCH*4,CURNCY*4
*
       CS(I)=RES79(I,3)+RES79(I,5)+RES79(I,6)
*
       CMANUF=CS(241)
       CFIXED=CPUFIX*CMANUF
       COST=CMANUF+CFIXED
*
       CURNCY=RUBCH(14)
*
       WRITE(JFC,814)'C O S T   R E P O R T:',
     &   '- - - - - - - - - - - - - - - - - - - - - - - - -'
       WRITE(JFC,802)
     & 'Cost excluding oil & cooling equipment     ',CTROVN,CURNCY
       WRITE(JFC,800) 'Cost file ident.:     ',(RUBCH(I),I=1,10)
       WRITE(JFC,812) '  '
       WRITE(JFC,812) 'Not included estimated prices'
       COLE=CS(201)+CS(223)+CS(239)
       IF(COLE.GT.1.) WRITE(JFC,802)
     & 'Estimated price for cooling equipment      ',COLE,CURNCY
       IF(CS(236).GT.1.) WRITE(JFC,802)
     & 'Oil absorbed                               ',CS(236),CURNCY
       IF(CS(237).GT.1.) WRITE(JFC,802)
     & 'Oil free in the transformer tank           ',CS(237),CURNCY
       WRITE(JFC,812) '  '
       WRITE(JFC,812)
     &  'Calculation of comparing price'
       WRITE(JFC,802)
     & 'Upgraded costs from the cost routines:     ',CMANUF,CURNCY
       WRITE(JFC,806)
     & 'Fixed price addition ( ',100.*CPUFIX,'%)',   CFIXED,CURNCY
       WRITE(JFC,808) 'Loss evaluation ',P00(1)/1.E+3,'*',1.E+3*P0LOSS,
     &                ' + ',URC(1,1,1)*ZWOULI/1.E+3,'*',1.E+3*PKLOSS,
     &                CLOSSV,CURNCY
       WRITE(JFC,810) 'Comparison price       ',COST+CLOSSV,CURNCY
*
       RETURN
*
  800  FORMAT(9X,A22,10A4)
  802  FORMAT(9X,A43, 7X,                         F13.0,1X,A4)
  804  FORMAT(9X,A5,I3,A2,38X,A3,                 F12.0,1X,A4)
  806  FORMAT(9X,A23,F9.1,A2,17X,                 F12.0,1X,A4)
  808  FORMAT(9X,A16,F7.1,A1,F7.0,A3,F7.1,A1,F7.0,F14.0,1X,A4)
  810  FORMAT(9X,A23,     28X,                    F12.0,1X,A4)
  812  FORMAT(6X,A)
  814  FORMAT(' '/' '/6X,A, A/1X)
*
       END
