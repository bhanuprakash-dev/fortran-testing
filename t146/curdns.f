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
*      Subroutine CURDNS
*      -----------------
*
C...   Title: Calculation of winding currents and voltages for
C...          output on Page 2 in the summarised list of results.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-09-25 by B-G Bladh  , SETFO/TS
*        FABOOS is entered for booster trafos
*
************************************************************************
*
       SUBROUTINE CURDNS
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0731(1)    ,  CURRUT (1)    ),
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ1101(1)    ,  VOLTUT (1)    ),
     &  (XZ1201(1)    ,  ZWINDW (1)    )
       EQUIVALENCE
     &  (XZ1263(1)    ,  SRATEP (1)    ),
     &  (XZ1267(1)    ,  UDIM   (1)    ),
     &  (XZ1271(1,1)  ,  UN     (1,1)  ),
     &  (XZ1341(1)    ,  BBRW   (1)    ),
     &  (XZ1373       ,  BBAUTO        )
       EQUIVALENCE
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1500       ,  NPG           ),
     &  (XZ1504       ,  NSG           ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ2731(1)    ,  FABOOS(1)     )
*
       REAL    SRATEP(4),UN(4,4),VOLTUT(9),CURRUT(9),UDIM(4),ZWINDW(9),
     &         FABOOS(9)
*
       INTEGER IWDG,KGROUP(9),KTML,NPG,NSG,NWILI
*
       LOGICAL BBAUTO,BBRW(9),BBVFR
*
CC*SEBL End of the declarations block.
*
C... Calculate current density and winding voltage
*
       DO 199 IWDG=1,NWILI
       KTML=KGROUP(IWDG)
       CURRUT(IWDG)=SRATEP(KTML)/UN(KTML,1)/FABOOS(IWDG)
       IF( BBAUTO
     &    .AND.KTML.EQ.NPG
     &    .AND..NOT.(BBRW(IWDG).AND..NOT.BBVFR))
     & CURRUT(IWDG)=CURRUT(IWDG)-SRATEP(KTML)/UN(NSG,1)/FABOOS(IWDG)
       VOLTUT(IWDG)=ZWINDW(IWDG)*UDIM(1)*FABOOS(IWDG) /1.E+3
  199  CONTINUE
*
       RETURN
       END
