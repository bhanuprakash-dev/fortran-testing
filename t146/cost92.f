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
*      Subroutine Cost92
*      -----------------
*
C...   Title:  Calculation of costs, simplified routines
*
*      Written: 92-01-07 by B-G Bladh  , SETFO/TS
*
************************************************************************
*
       SUBROUTINE COST92
*
C... Declarations
*
       COMMON/CST92/
     &   CUC92, CUC92P, FEC92, FEC92P, OLC92, OLC92P
       REAL
     &   CUC92(3), CUC92P(3), FEC92(4), FEC92P(4), OLC92, OLC92P
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0861(1)    ,  GWINCO(1)     ),
     &  (XZ1415       ,  COST          ),
     &  (XZ1417       ,  CPUFIX        ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1527       ,  ISTGRD        ),
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ1481(1)    ,  NCONDU(1)     ),
     &  (XZ2738       ,  CTRCMP        ),
     &  (XZ1422       ,  CTROVN        ),
     &  (XZ2739       ,  CUCOS         ),
     &  (XZ2740       ,  CUCOSP        )
       EQUIVALENCE
     &  (XZ2741       ,  FECOS         ),
     &  (XZ2742       ,  FECOSP        ),
     &  (XZ2743       ,  OLCOS         ),
     &  (XZ2745       ,  OLCOSP        ),
     &  (XZ1455       ,  GCORLA        ),
     &  (XZ2737       ,  GOIL          ),
     &  (XZ2744       ,  GCONDU        )
*
       REAL
     &   GCORLA,GCONDU,GOIL,X,COST,CPUFIX,GWINCO(9),ZWOULI,
     &   CTRCMP,CTROVN, CUCOS, CUCOSP, FECOS, FECOSP, OLCOS, OLCOSP
*
       INTEGER  ISTGRD, IWDG,  NCONDU(9),NWILI, J
*
C... Copper costs
*
       CUCOS  = 0.
       CUCOSP = 0.
       DO 10 IWDG = 1, NWILI
          J      = NCONDU(IWDG)
          X      = GWINCO(IWDG)* CUC92(J) * ZWOULI
          CUCOS  = CUCOS  + X
          CUCOSP = CUCOSP + X*CUC92P(J)
 10    CONTINUE

*
C... Core lamination cost
*
       FECOS  = GCORLA * FEC92(ISTGRD)
       FECOSP = FECOS  * FEC92P(ISTGRD)
*
C... Free Oil cost
*
       OLCOS  = GOIL   * OLC92
       OLCOSP = OLCOS  * OLC92P
*
       CTROVN = FECOS  + CUCOS  + OLCOS
       CTRCMP = FECOSP + CUCOSP + OLCOSP
       COST   = CTRCMP * (1.+CPUFIX)
*
*
       RETURN
       END
