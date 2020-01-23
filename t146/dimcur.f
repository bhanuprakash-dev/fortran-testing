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
*      Subroutine DIMCUR
*      -----------------
*
C...   Title:  Calculates dimensioning winding currents
C...           and equivalent 2-winding power ( SEQU2W )
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE DIMCUR
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1243(1)    ,  RIDIMW (1)    ),
     &  (XZ1247(1)    ,  RIDIRW (1)    ),
     &  (XZ1263(1)    ,  SRATEP (1)    ),
     &  (XZ1267(1)    ,  UDIM   (1)    ),
     &  (XZ1271(1,1)  ,  UN     (1,1)  )
       EQUIVALENCE
     &  (XZ1373       ,  BBAUTO        ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1500       ,  NPG           ),
     &  (XZ1504       ,  NSG           )
       EQUIVALENCE
     &  (XZ1539       ,  SEQU2W        ),
     &  (XZ1542       ,  SNOML         )
*
       REAL      UN(4,4),UDIM(4),SRATEP(4),RIDIMW(4),RIDIRW(4),
     &           SEQU2W,SNOML,AUTO
*
       INTEGER   I3,I4,KTML,NG,NPG,NSG
*
       LOGICAL BBAUTO,BBVFR
*
CC*SEBL End of the declarations block.
*
C... Set RIDIMW and RIDIRW
*
       DO 299 KTML=1,NG
       RIDIMW(KTML)=SRATEP(KTML)/UN(KTML,1)
  299  RIDIRW(KTML)=RIDIMW(KTML)
*
C... Adjust RIDIMW and RIDIRW
*
C,,, BBAUTO
*
       IF(BBAUTO) THEN
          RIDIMW(NPG)=RIDIMW(NPG)-RIDIMW(NSG)
          AUTO=1.-UDIM(NPG)/UDIM(NSG)
          IF(BBVFR) RIDIRW(NPG)=RIDIMW(NPG)
*
C... NOT BBAUTO
*
       ELSE
          AUTO=1.
       END IF
*
C... Calculate equivalent 2-winding rating
*
       I1=1
       I2=1
       I3=0
       IF(NG.GE.3) I3=1
       I4=0
       IF(NG.GE.4) I4=1
CCCCC  SEQU2W=SNOML*AUTO+(I3*SRATEP(3)+I4*SRATEP(4))/2.
       SEQU2W=(AUTO*(SRATEP(1)*I1+SRATEP(2)*I2)+
     &               SRATEP(3)*I3+SRATEP(4)*I4)/2.
*
       RETURN
       END
