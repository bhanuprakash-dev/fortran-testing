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
*      Subroutine PRESET
*      -----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 83-05-04 by H.Westberg   , ZKAB
*      Revised: 87-04-27 by Ron Bell     , TRAFO/IK
*
*  1.  Description
C...       Title:  Examine indata for additional information
*
************************************************************************
*
       SUBROUTINE PRESET
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1508       ,  NWILI         ),
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ0751(1)    ,  DYOKE  (1)    )
       EQUIVALENCE
     &  (XZ1331(1)    ,  BBRR   (1)    ),
     &  (XZ0921(1)    ,  NGROUP (1)    ),
     &  (XZ2555(1)    ,  KWIND  (1)    ),
     &  (XZ2583(1)    ,  ZCOIAR (1)    ),
     &  (XZ2584(1)    ,  EXTRAR (1)    ),
     &  (XZ2585(1)    ,  HCLAC  (1)    )
       EQUIVALENCE
     &  (XZ1091(1)    ,  TSPIN  (1)    ),
     &  (XZ2591(1)    ,  ZAR    (1)    ),
     &  (XZ2592(1)    ,  ZRR    (1)    ),
     &  (XZ2593(1)    ,  ZPART  (1)    ),
     &  (XZ0871(1)    ,  HPART  (1)    )
       EQUIVALENCE
     &  (XZ0681(1)    ,  BPART  (1)    ),
     &  (XZ2586(1)    ,  ZDISC  (1)    ),
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ2595(1)    ,  ZTULO  (1)    ),
     &  (XZ2554(1)    ,  NLORR  (1)    )
       EQUIVALENCE
     &  (XZ0931(1)    ,  NLOOP  (1)    ),
     &  (XZ2594(1)    ,  ZTUDI  (1)    ),
     &  (XZ0891(1)    ,  KCODE  (1)    ),
     &  (XZ2581(1)    ,  ZLAG   (1)    ),
     &  (XZ2582(1)    ,  ZNLAG  (1)    )
       EQUIVALENCE
     &  (XZ2597(1)    ,  HPRTMN (1)    ),
     &  (XZ2598(1)    ,  HPRTMX (1)    ),
     &  (XZ2599(1)    ,  BPRTMN (1)    ),
     &  (XZ2600(1)    ,  BPRTMX (1)    )
*
       REAL      ZWIND(9),DYOKE(9),EXTRAR(9),HCLAC(9),ZCOIAR(9),
     &           TSPIN(9),ZDISC(9),ACOND(9),ZTULO(9),
     &           ZAR(9),ZRR(9),ZPART(9),HPART(9),BPART(9),ZTUDI(9),
     &           HPRTMX(9),BPRTMX(9),HPRTMN(9),BPRTMN(9),
     &           ZNLAG(9),ZLAG(9)
*
       INTEGER   KGROUP(9),NGROUP(9),KWIND(9),NLOOP(9),
     &           KCODE(9),NLORR(9)
*
       LOGICAL   BBRR(9)
*
       DIMENSION TARR(100)
       CHARACTER TARR*8
*
C... Convert the winding types for the winding layout routines
*
       CALL TYPWDG
*
C... Set various winding parameters
*
       DO 4000 IWDG=1,NWILI
       ZDISC(IWDG)=0.
       ZTUDI(IWDG)=0.
       ZTULO(IWDG)=0.
       ZPART(IWDG)=1.
       ZCOIAR(IWDG)=1.
       ZAR(IWDG)=1.
       ZRR(IWDG)=1.
       ZNLAG(IWDG)=0.
       ZLAG(IWDG)=1.
*
       MKWIND=KWIND(IWDG)
*
       IF (MKWIND.GE.11.AND.MKWIND.LE.19.AND.KCODE(IWDG).EQ.2) MKWIND=7
       IF (MKWIND.GE.20.AND.MKWIND.LE.39.AND.KCODE(IWDG).EQ.2) MKWIND=10
*
C... Maximum strand dimensions
*
       IF (TARR(40+IWDG)(1:1).EQ.'C') THEN
          HPRTMX(IWDG)=0.015
          BPRTMX(IWDG)=0.003
          HPRTMN(IWDG)=0.008
          BPRTMN(IWDG)=0.0011
       ELSE
          HPRTMX(IWDG)=0.016
          BPRTMX(IWDG)=0.0035
          HPRTMN(IWDG)=0.009
          BPRTMN(IWDG)=0.0013
       END IF
*
C... Convert insulation thicknesses to compacted values
*
       IF (MKWIND.LE.10.OR.MKWIND.GE.40) THEN
          TSPIN(IWDG)=TSPIN(IWDG)*0.95
          HCLAC(IWDG)=HCLAC(IWDG)*0.95
       END IF
*
C... Number of strands in axial and radial directions
*
       IF (MKWIND.GE.11.AND.MKWIND.LE.29) THEN
          ZAR(IWDG)=ANINT(MKWIND/10.)
          ZRR(IWDG)=AMOD(ANINT(FLOAT(MKWIND)),10.)
       ELSE IF (MKWIND.GE.2.AND.MKWIND.LE.6) THEN
          ZAR(IWDG)=FLOAT(MKWIND)
       ELSE
          ZAR(IWDG)=1.
       END IF
*
       IF (MKWIND.EQ.12.OR.MKWIND.EQ.22) ZRR(IWDG)=2.
*
C... Number of loops in the radial direction
*
       NLORR(IWDG)=1
       IF (MKWIND.GE.7.AND.MKWIND.LE.9)     NLORR(IWDG)=1
       IF (MKWIND.EQ.10)                    NLORR(IWDG)=2
       IF (MKWIND.GE.11.AND.MKWIND.LE.39)   NLORR(IWDG)=1
       IF (MKWIND.EQ.8)                     NLOOP(IWDG)=NLOOP(IWDG)/2
       IF (MKWIND.EQ.9)                     NLOOP(IWDG)=NLOOP(IWDG)/3
       IF (MKWIND.GE.7.AND.MKWIND.NE.100.AND.BBRR(IWDG)) ZPART(IWDG)=1.
       IF (MKWIND.NE.0.OR.MKWIND.NE.100 .OR..NOT.BBRR(IWDG)) GOTO 2600
*
C... Adjust the initial values of strand dimensions for disc windings
*
 2550  IF (HPART(IWDG)*BPART(IWDG).LE.ACOND(IWDG)) GOTO 2600
       HPART(IWDG)=HPART(IWDG)*0.95
       BPART(IWDG)=BPART(IWDG)*0.95
       GOTO 2550
 2600  CONTINUE
*
       IF (.NOT.BBRR(IWDG)) THEN
          ZPART(IWDG)=ZRR(IWDG)*ZAR(IWDG)
          IF (KWIND(IWDG).LE.0.OR.KWIND(IWDG).EQ.100) THEN
*
C... Calculate the number of discs and number of turns per disc if
C... the winding dimensions are fixed
*
             ZDISC(IWDG)=(HLIMB-DYOKE(IWDG)-EXTRAR(IWDG)+HCLAC(IWDG))/
     &                           (HPART(IWDG)+TSPIN(IWDG)+HCLAC(IWDG))
             ZDISC(IWDG)=2.*ANINT(ZDISC(IWDG)/2.)
             ZTUDI(IWDG)=ZWIND(IWDG)*FLOAT(NGROUP(IWDG))/ZDISC(IWDG)
             ZTUDI(IWDG)=ANINT(ZTUDI(IWDG)*2.+1.0)/2.
          END IF
       END IF
*
 4000  CONTINUE
*
       RETURN
       END
