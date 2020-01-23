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
*      Subroutine WDGTYP
*      -----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 87-04-27 by Ron Bell     , TRAFO/IK
*
*  1.  Description
C...       Title:  Convert the winding types
C...               to the same classification of windings as in T31146
C...               from the winding layout routines
C...               KWIND  --->  KWITYP
*
************************************************************************
*
       SUBROUTINE WDGTYP
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ2555(1)    ,  KWIND  (1)    ),
     &  (XZ0911(1)    ,  KWITYP (1)    ),
     &  (XZ2583(1)    ,  ZCOIAR (1)    )
*
       REAL      ZCOIAR(9)
*
       INTEGER   IWDG,NWILI,KWIND(9),KWITYP(9)
*
       DIMENSION TARR(100)
       CHARACTER TARR*8
*
C... Translation to the same classification of windings as in T31146
*
       DO 105 IWDG=1,NWILI
       IF      (KWIND(IWDG).EQ.1  ) THEN
          TARR(30+IWDG)='S1      '
          KWITYP(IWDG)=1
       ELSE IF (KWIND(IWDG).EQ.2  ) THEN
          TARR(30+IWDG)='S2      '
          KWITYP(IWDG)=1
       ELSE IF (KWIND(IWDG).EQ.3  ) THEN
          TARR(30+IWDG)='S3      '
          KWITYP(IWDG)=1
       ELSE IF (KWIND(IWDG).EQ.4  ) THEN
          TARR(30+IWDG)='S4      '
          KWITYP(IWDG)=1
       ELSE IF (KWIND(IWDG).EQ.5  ) THEN
          TARR(30+IWDG)='S5      '
          KWITYP(IWDG)=1
       ELSE IF (KWIND(IWDG).EQ.6  ) THEN
          TARR(30+IWDG)='S6      '
          KWITYP(IWDG)=1
       ELSE IF (KWIND(IWDG).EQ.11 ) THEN
          TARR(30+IWDG)='L       '
       ELSE IF (KWIND(IWDG).EQ.12 ) THEN
          TARR(30+IWDG)='L1      '
       ELSE IF (KWIND(IWDG).EQ.13 ) THEN
          TARR(30+IWDG)='L2      '
       ELSE IF (KWIND(IWDG).EQ.14 ) THEN
          TARR(30+IWDG)='L3      '
       ELSE IF (KWIND(IWDG).EQ.11 ) THEN
          TARR(30+IWDG)='SLS1    '
       ELSE IF (KWIND(IWDG).EQ.12 ) THEN
          TARR(30+IWDG)='SLS2    '
       ELSE IF (KWIND(IWDG).EQ.13 ) THEN
          TARR(30+IWDG)='SLS3    '
       ELSE IF (KWIND(IWDG).EQ.14 ) THEN
          TARR(30+IWDG)='SLS4    '
       ELSE IF (KWIND(IWDG).EQ.15 ) THEN
          TARR(30+IWDG)='SLS5    '
       ELSE IF (KWIND(IWDG).EQ.16 ) THEN
          TARR(30+IWDG)='SLS6    '
       ELSE IF (KWIND(IWDG).EQ.7  ) THEN
          TARR(30+IWDG)='SLL1    '
          KWITYP(IWDG)=7
       ELSE IF (KWIND(IWDG).EQ.10 ) THEN
          TARR(30+IWDG)='SLL2    '
          KWITYP(IWDG)=7
       ELSE IF (KWIND(IWDG).EQ.100 ) THEN
          TARR(30+IWDG)='SD      '
          KWITYP(IWDG)=4
       ELSE IF (KWIND(IWDG).EQ.0 ) THEN
          TARR(30+IWDG)='CD      '
          KWITYP(IWDG)=3
       END IF
  105  CONTINUE
*
  100  DO 299 IWDG=1,NWILI
CCCCC  IF (KWIND(IWDG).EQ.11.OR.KWIND(IWDG).EQ.12) THEN
CCCCC     ZCOIAR(IWDG)=FLOAT(KWIND(IWDG)-10)
CCCCC     KWITYP(IWDG)=9
CCCCC  ELSE IF (KWIND(IWDG).EQ.13.OR.KWIND(IWDG).EQ.14) THEN
CCCCC     ZCOIAR(IWDG)=FLOAT(KWIND(IWDG)-12)
CCCCC     KWITYP(IWDG)=9
CCCCC  ELSE IF (KWIND(IWDG).EQ.15.OR.KWIND(IWDG).EQ.16) THEN
CCCCC     ZCOIAR(IWDG)=FLOAT(KWIND(IWDG)-14)
CCCCC     KWITYP(IWDG)=9
CCCCC  ELSE IF (KWIND(IWDG).GE.21.AND.KWIND(IWDG).LE.29) THEN
CCCCC     ZCOIAR(IWDG)=FLOAT(KWIND(IWDG)-20)
CCCCC     KWITYP(IWDG)=9
CCCCC  ELSE IF (KWIND(IWDG).GE.30.AND.KWIND(IWDG).LE.39) THEN
CCCCC     ZCOIAR(IWDG)=FLOAT(KWIND(IWDG)-30)
CCCCC     KWITYP(IWDG)=8
CCCCC  END IF
*
  299  CONTINUE
*
  199  RETURN
       END
