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
*      Subroutine TYPWDG
*      -----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 87-04-27 by Ron Bell     , TRAFO/IK
*
*  1.  Description
C...       Title:  Convert the winding types
C...               for the winding layout routines
C...               KWITYP --->  KWIND
*
************************************************************************
*
       SUBROUTINE TYPWDG
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1508       ,  NWILI         ),
     &  (XZ2555(1)    ,  KWIND  (1)    ),
     &  (XZ0401(1)    ,  TARR   (1)    )
*
       INTEGER   IWDG,KWIND(9)
*
       DIMENSION NCOL(9),TARR(100)
       CHARACTER NCOL*1,TARR*8
*
       DATA NCOL /'A','B','C','D','E','F','G','H','I'/
*
C... Winding data
*
       DO 105 IWDG=1,NWILI
       IF (    (TARR(30+IWDG).EQ.'S       ').OR.
     &         (TARR(30+IWDG).EQ.'S1      ')) THEN
          KWIND(IWDG)=1
       ELSE IF (TARR(30+IWDG).EQ.'S2      ') THEN
          KWIND(IWDG)=2
       ELSE IF (TARR(30+IWDG).EQ.'S3      ') THEN
          KWIND(IWDG)=3
       ELSE IF (TARR(30+IWDG).EQ.'S4      ') THEN
          KWIND(IWDG)=4
       ELSE IF (TARR(30+IWDG).EQ.'S5      ') THEN
          KWIND(IWDG)=5
       ELSE IF (TARR(30+IWDG).EQ.'S6      ') THEN
          KWIND(IWDG)=6
       ELSE IF (TARR(30+IWDG).EQ.'S7      ') THEN
          KWIND(IWDG)=6
       ELSE IF (TARR(30+IWDG).EQ.'S8      ') THEN
          KWIND(IWDG)=6
       ELSE IF (TARR(30+IWDG).EQ.'S9      ') THEN
          KWIND(IWDG)=6
       ELSE IF (TARR(30+IWDG).EQ.'S10     ') THEN
          KWIND(IWDG)=6
       ELSE IF (TARR(30+IWDG).EQ.'S11     ') THEN
          KWIND(IWDG)=6
       ELSE IF (TARR(30+IWDG).EQ.'S12     ') THEN
          KWIND(IWDG)=6
       ELSE IF (TARR(30+IWDG).EQ.'L       ') THEN
          KWIND(IWDG)=11
       ELSE IF (TARR(30+IWDG).EQ.'L1      ') THEN
          KWIND(IWDG)=12
       ELSE IF (TARR(30+IWDG).EQ.'L2      ') THEN
          KWIND(IWDG)=13
       ELSE IF (TARR(30+IWDG).EQ.'L3      ') THEN
          KWIND(IWDG)=14
       ELSE IF (TARR(30+IWDG).EQ.'CD      ') THEN
          KWIND(IWDG)=0
       ELSE IF (TARR(30+IWDG).EQ.'SD      ') THEN
          KWIND(IWDG)=100
       ELSE IF (TARR(30+IWDG).EQ.'D2      ') THEN
          KWIND(IWDG)=100
       ELSE IF (TARR(30+IWDG).EQ.'SLS     ') THEN
          KWIND(IWDG)=11
       ELSE IF (TARR(30+IWDG).EQ.'SLS1    ') THEN
          KWIND(IWDG)=11
       ELSE IF (TARR(30+IWDG).EQ.'SLS2    ') THEN
          KWIND(IWDG)=12
       ELSE IF (TARR(30+IWDG).EQ.'SLS3    ') THEN
          KWIND(IWDG)=13
       ELSE IF (TARR(30+IWDG).EQ.'SLS4    ') THEN
          KWIND(IWDG)=14
       ELSE IF (TARR(30+IWDG).EQ.'SLS5    ') THEN
          KWIND(IWDG)=15
       ELSE IF (TARR(30+IWDG).EQ.'SLS6    ') THEN
          KWIND(IWDG)=16
       ELSE IF (TARR(30+IWDG).EQ.'SLL     ') THEN
          KWIND(IWDG)=7
       ELSE IF (TARR(30+IWDG).EQ.'SLL1    ') THEN
          KWIND(IWDG)=7
       ELSE IF (TARR(30+IWDG).EQ.'SLL2    ') THEN
          KWIND(IWDG)=10
       END IF
 105   CONTINUE
*
       RETURN
       END
