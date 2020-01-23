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
*      Subroutine PAGC92
*      -----------------
*
C...   TITLE:  PRINTOUT OF COSTS IN MNL-87
*
*      Written: 92-01-08 by B-G Bladh     , SETFO/TS
*      Revised: ....................................
************************************************************************
*
       SUBROUTINE PAGC92
*
C... Declarations
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0861(1)    ,  GWINCO (1)    ),
     &  (XZ1414       ,  CLOSSV        ),
     &  (XZ1415       ,  COST          ),
     &  (XZ1417       ,  CPUFIX        )
       EQUIVALENCE
     &  (XZ1422       ,  CTROVN        ),
     &  (XZ1455       ,  GCORLA        ),
     &  (XZ2738       ,  CTRCMP        ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1510       ,  P0LOSS        ),
     &  (XZ1514       ,  PKLOSS        ),
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ1587       ,  CHCOST        ),
     &  (XZ1632(1)    ,  RUBCH  (1)    ),
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1689(1,1,1),  URC    (1,1,1))
       EQUIVALENCE
     &  (XZ2737       ,  GOIL          ),
     &  (XZ2739       ,  CUCOS         ),
     &  (XZ2740       ,  CUCOSP        ),
     &  (XZ2741       ,  FECOS         ),
     &  (XZ2742       ,  FECOSP        ),
     &  (XZ2743       ,  OLCOS         ),
     &  (XZ2744       ,  GCONDU        ),
     &  (XZ2745       ,  OLCOSP        )
*
       REAL      P00(3),URC(4,4,3),CLOSSV,COST,CPUFIX,FIXADD,
     &           CTROVN,CTRCMP,GCONDU,GCORLA,GOIL,GWINCO(9),
     &           PKLOSS,P0LOSS,ZWOULI,
     &           CUCOS,CUCOSP,FECOS,FECOSP,OLCOS,OLCOSP
*
       INTEGER   I,JFC,NWILI
*
       DIMENSION RUBCH(15)
       CHARACTER RUBCH*4,CHCOST*6,CURNCY*4
*
       FIXADD = CTRCMP * CPUFIX
*
       CALL CONCAT(CURNCY,0, CHCOST, 0, 4)
*
       WRITE(JFC,809)'C O S T   O P T I M I Z A T I O N   R E P O R T',
     &               ' - - - - - - - - - - - -'
       WRITE(JFC,800) 'Cost file ident:  ',(RUBCH(I),I=1,10)
       WRITE(JFC,801) 'Mass', 'Base value', 'Upgraded value'
       WRITE(JFC,802) '(kg)', CURNCY,CURNCY
       WRITE(JFC,803) 'Winding conductor', GCONDU, CUCOS, CUCOSP
       WRITE(JFC,803) 'Core lamination  ', GCORLA, FECOS, FECOSP
       WRITE(JFC,803) 'Free Oil         ', GOIL  , OLCOS, OLCOSP
*
       WRITE(JFC,804) 'S u m :          ', CTROVN,CTRCMP
*
       WRITE(JFC,805) 'Fixed price add (', 100.*CPUFIX, FIXADD
       WRITE(JFC,806) 'Loss evaluation',CURNCY
       WRITE(JFC,807)
     &  'No load', P00(1)/1000.,1000.*P0LOSS,
     &             P0LOSS*P00(1)
       WRITE(JFC,807)
     &  'Load   ' ,  URC(1,1,1)*ZWOULI/1000.,1000.*PKLOSS,
     &             PKLOSS * URC(1,1,1) * ZWOULI
       WRITE(JFC,808) 'Comparing price',COST+CLOSSV
*
       RETURN
*
  800  FORMAT(6X,A18,10A4)
  801  FORMAT(6X,32X,A4,8X,A10,4X,A14)
  802  FORMAT(6X,32X,A4,2(12X,'(',A4,')'))
  803  FORMAT(6X,A17,1X,3F18.0 )
  804  FORMAT(6X,24X,48('-') / 6X,A17,19X,2F18.0)
  805  FORMAT(6X,A17,F5.1,' % )',28X,F18.0)
  806  FORMAT(6X,A15,17X,'(kW)',9X,'(',A4,'/kW)')
  807  FORMAT(6X,4X, A7,7X, F18.1 ,2F18.0)
  808  FORMAT(6X,24X,48('-') / 6X,A15,39X,F18.0)
  809  FORMAT(' '/' '/6X,A, A/1X)
*
       END
