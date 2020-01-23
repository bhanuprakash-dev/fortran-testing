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
*      Subroutine CONSTR
*      -----------------
*
C...   Title:  Determination of those limits
C...           which have been exceeded
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE CONSTR
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0701(1)    ,  CURDEN (1)    ),
     &  (XZ0711(1)    ,  CURDM  (1)    ),
     &  (XZ0971(1)    ,  PRESSM (1)    ),
     &  (XZ1041(1)    ,  STRWMM (1)    ),
     &  (XZ1051(1)    ,  STRWMP (1)    )
       EQUIVALENCE
     &  (XZ1081(1)    ,  TENSM  (1)    ),
     &  (XZ1410       ,  BTANK         ),
     &  (XZ1411       ,  BTANKM        ),
     &  (XZ1424       ,  DCOMAX        )
       EQUIVALENCE
     &  (XZ1425       ,  DCOMIN        ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1466       ,  GTRP          ),
     &  (XZ1467       ,  GTRPM         ),
     &  (XZ1472       ,  HLIMB         )
       EQUIVALENCE
     &  (XZ1473       ,  HLMBMA        ),
     &  (XZ1474       ,  HTANK         ),
     &  (XZ1475       ,  HTANKM        ),
     &  (XZ1478       ,  HLMBMI        ),
     &  (XZ1495       ,  NCONST        ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1516       ,  PLOADM        ),
     &  (XZ1520       ,  PNOLOM        ),
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1529       ,  RLTNKM        ),
     &  (XZ1543       ,  SOUNDM        )
       EQUIVALENCE
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1686(1)    ,  SOUND0 (1)    ),
     &  (XZ1689(1,1,1),  URC    (1,1,1)),
     &  (XZ2145(1)    ,  SAVE1  (1)    )
*
       REAL      CURDM(9),CURDEN(9),URC(4,4,3),SOUND0(3),PRESSM(9),
     &           TENSM(9),STRWMM(9),STRWMP(9),P00(3),BTANK,BTANKM,
     &           DCOMAX,DCOMIN,DCORE,GTRP,GTRPM,HLIMB,HLMBMA,HTANK,
     &           HTANKM,PLOADM,PNOLOM,RLTANK,RLTNKM,SOUNDM,ZWOULI,
     &           HLMBMI
*
       INTEGER   IWDG,NCONST,NWILI
*
       DIMENSION SAVE1(50)
       CHARACTER SAVE1*7,TEXT1*7,TEXT2*7
*
CC*SEBL End of the declarations block.
*
C... Initialise NCONST = 0
*
CC*SBBL Start of the initialisation block.
*
       NCONST=0
*
CC*SEBL End of the initialisation block.
*
C... Check the limits
*
       IF(PLOADM.GT.0)
     & CALL LIMCHK (URC(1,1,1)*ZWOULI/PLOADM ,' PLOADM','*PLOADM',
     &              NCONST,SAVE1)
*
CC*SOFF Switch off structure analysis (Not all calls need be shown)
*
       IF(PNOLOM.GT.0)
     & CALL LIMCHK (P00(1)/PNOLOM,' PNOLOM','*PNOLOM',NCONST,SAVE1)
       CALL LIMCHK (DCORE/DCOMAX ,' DCOMAX','*DCOMAX',NCONST,SAVE1)
       CALL LIMCHK (DCOMIN/DCORE ,' DCOMIN','*DCOMIN',NCONST,SAVE1)
       CALL LIMCHK (HTANK/HTANKM ,' HTANKM','*HTANKM',NCONST,SAVE1)
       CALL LIMCHK (HLIMB/HLMBMA ,' HLMBMA','*HLMBMA',NCONST,SAVE1)
       CALL LIMCHK (HLMBMI/HLIMB ,' HLMBMI','*HLMBMI',NCONST,SAVE1)
       CALL LIMCHK (BTANK/BTANKM ,' BTANKM','*BTANKM',NCONST,SAVE1)
       CALL LIMCHK (RLTANK/RLTNKM,' LTANKM','*LTANKM',NCONST,SAVE1)
       CALL LIMCHK (GTRP/GTRPM   ,' GTRPM ','*GTRPM ',NCONST,SAVE1)
       IF(SOUNDM.GT.0)
     & CALL LIMCHK (SOUND0(2)/SOUNDM,' SOUNDM','*SOUNDM',NCONST,SAVE1)
*
CC*SON  Switch on  structure analysis (Not all calls need be shown)
*
C...Windings
*
       DO 199 IWDG=1,NWILI
*
C...Prepare the parameters
*
CC*SBBL Start of the preparation block.
*
       WRITE(TEXT1,1) ' CURD ',IWDG
       WRITE(TEXT2,1) '*CURD ',IWDG
*
CC*SEBL End of the preparation block.
*
C...Check the limit
*
       CALL LIMCHK (CURDEN(IWDG)/CURDM(IWDG),TEXT1,TEXT2,NCONST,SAVE1)
*
CC*SOFF Switch OFF structure analysis (Not all calls need be shown)
*
       WRITE(TEXT1,1) ' PRESS',IWDG
       WRITE(TEXT2,1) '*PRESS',IWDG
       CALL LIMCHK
     &  (ABS(STRWMM(IWDG)/PRESSM(IWDG)),TEXT1,TEXT2,NCONST,SAVE1)
       WRITE(TEXT1,1) ' TENS ',IWDG
       WRITE(TEXT2,1) '*TENS ',IWDG
       CALL LIMCHK
     &  (ABS(STRWMP(IWDG)/TENSM(IWDG)),TEXT1,TEXT2,NCONST,SAVE1)
*
CC*SON  Switch ON  structure analysis (Not all calls need be shown)
*
  199  CONTINUE
       RETURN
    1  FORMAT(A6,I1)
       END
