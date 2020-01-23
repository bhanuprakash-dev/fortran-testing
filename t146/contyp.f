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
*      Subroutine CONTYP
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
       SUBROUTINE CONTYP
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1508       ,  NWILI         ),
     &  (XZ2555(1)    ,  KWIND  (1)    ),
     &  (XZ0911(1)    ,  KWITYP (1)    )
*
       INTEGER   IWDG,KWIND(9),KWITYP(9)
*
       DIMENSION IDIDWI(12),KWIND1(9),NCOL(9)
       CHARACTER IDIDWI*6,KWIND1*6,NCOL*1
*
       DATA NCOL /'A','B','C','D','E','F','G','H','I'/
*
       DATA IDIDWI /'CD    ','S1    ','S2    ','S3    ','S4    ',
     &              'S5    ','S6    ','SLL   ',
     &              'LN    ','LNX   ','BND   ','SD    '/
*
C... Winding data
*
       DO 105 IWDG=1,NWILI
       KW=KWIND(IWDG)
       IF (KW.LT. 7             ) KWIND1(IWDG)=IDIDWI(KW+1)
       IF (KW.GE. 7.AND.KW.LE.10) KWIND1(IWDG)=IDIDWI(8)
       IF (KW.GE.11.AND.KW.LE.19) KWIND1(IWDG)=IDIDWI(9)
       IF (KW.GE.21.AND.KW.LE.29) KWIND1(IWDG)=IDIDWI(10)
       IF (KW.GE.30.AND.KW.LE.39) KWIND1(IWDG)=IDIDWI(11)
       IF (KW.EQ.100            ) KWIND1(IWDG)=IDIDWI(12)
       IF (KWIND1(IWDG).EQ.'S1    ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S2    ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S3    ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S4    ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S5    ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S6    ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S7    ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S8    ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S9    ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S10   ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S11   ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'S12   ') KWITYP(IWDG)=1
       IF (KWIND1(IWDG).EQ.'L     ') KWITYP(IWDG)=2
       IF (KWIND1(IWDG).EQ.'L1    ') KWITYP(IWDG)=2
       IF (KWIND1(IWDG).EQ.'L2    ') KWITYP(IWDG)=2
       IF (KWIND1(IWDG).EQ.'L3    ') KWITYP(IWDG)=2
       IF (KWIND1(IWDG).EQ.'CD    ') KWITYP(IWDG)=3
       IF (KWIND1(IWDG).EQ.'SD    ') KWITYP(IWDG)=4
       IF (KWIND1(IWDG).EQ.'D2    ') KWITYP(IWDG)=5
       IF (KWIND1(IWDG).EQ.'SLS   ') KWITYP(IWDG)=6
       IF (KWIND1(IWDG).EQ.'SLS1  ') KWITYP(IWDG)=6
       IF (KWIND1(IWDG).EQ.'SLS2  ') KWITYP(IWDG)=6
       IF (KWIND1(IWDG).EQ.'SLS3  ') KWITYP(IWDG)=6
       IF (KWIND1(IWDG).EQ.'SLS4  ') KWITYP(IWDG)=6
       IF (KWIND1(IWDG).EQ.'SLS5  ') KWITYP(IWDG)=6
       IF (KWIND1(IWDG).EQ.'SLS6  ') KWITYP(IWDG)=6
       IF (KWIND1(IWDG).EQ.'SLL   ') KWITYP(IWDG)=7
       IF (KWIND1(IWDG).EQ.'SLL1  ') KWITYP(IWDG)=7
       IF (KWIND1(IWDG).EQ.'SLL2  ') KWITYP(IWDG)=7
 105   CONTINUE
*
       RETURN
       END
