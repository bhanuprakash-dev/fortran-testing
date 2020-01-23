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
*      Subroutine NPAGE
*      ----------------
*
C...   Title:   Prints a new heading if required.
C...            If BBERR1 =.TRUE. an error message is
C...            printed at the top of each page.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-12-10 by B-G Bladh  , SETFO/TS
*One extra blank line after heading
************************************************************************
*
       SUBROUTINE NPAGE(KRAD)
*
C... Declarations
*
CC*SBBL Start of the declarations block
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1379       ,  BBERR1        ),
     &  (XZ1482       ,  IPAGE         ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1589       ,  DATE          ),
     &  (XZ1604(1)    ,  IDENT  (1)    )
       EQUIVALENCE
     &  (XZ2562       ,  BBFREQ        )
*
       INTEGER   I,IPAGE,IRAD,JFC,KRAD
*
       LOGICAL   BBERR1,BBFREQ
*
       DIMENSION IDENT(18)
       CHARACTER IDENT*4,DATE*26
*
CC*SEBL End of the declarations block
*
       IRAD=0
       IRAD=IRAD+KRAD
*
C... Write heading
*
C,,, If a new page
*
       IF(IRAD.GE.64.OR.KRAD.LE.0) THEN
          IPAGE=IPAGE+1
*
C... Write the heading
*
             WRITE(JFC, 81) (IDENT(I),I=1,9),DATE,IPAGE
*
C... Print error warning if needed
*
          IF (BBERR1.OR.BBFREQ) CALL ERRRUT(BBERR1,BBFREQ,JFC)
          IRAD=KRAD
       END IF
       RETURN
*
   81  FORMAT(1H1,'     ',9A4,A26,' Page',I2/1X)
       END
