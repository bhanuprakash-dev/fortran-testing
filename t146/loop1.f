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
*      Subroutine LOOP1
*      ----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 83-03-18 by H. Westberg  , ZKAB
*      Revised: 87-04-27 by Ron Bell     , TRAFO/IK
*
*  1.  Description
C...       Title:  Winding layout for SLL-windings
*
************************************************************************
*
       SUBROUTINE LOOP1(IWDG)
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1513       ,  PI            ),
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1001(1)    ,  RRWDG  (1)    )
       EQUIVALENCE
     &  (XZ0671(1)    ,  BDUCT  (1)    ),
     &  (XZ0921(1)    ,  NGROUP (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ0751(1)    ,  DYOKE  (1)    ),
     &  (XZ1091(1)    ,  TSPIN  (1)    )
       EQUIVALENCE
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ0681(1)    ,  BPART  (1)    ),
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0931(1)    ,  NLOOP  (1)    ),
     &  (XZ2298(1)    ,  NCOL   (1)    ),
     &  (XZ2563       ,  BBADJU        )
       EQUIVALENCE
     &  (XZ2581(1)    ,  ZLAG   (1)    ),
     &  (XZ2582(1)    ,  ZNLAG  (1)    ),
     &  (XZ2583(1)    ,  ZCOIAR (1)    ),
     &  (XZ2584(1)    ,  EXTRAR (1)    ),
     &  (XZ2589(1)    ,  SWIND  (1)    )
       EQUIVALENCE
     &  (XZ2590(1)    ,  RINNER (1)    ),
     &  (XZ2593(1)    ,  ZPART  (1)    ),
     &  (XZ2554(1)    ,  NLORR  (1)    ),
     &  (XZ2595(1)    ,  ZTULO  (1)    ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ2555(1)    ,  KWIND  (1)    )
*
       REAL      ZWIND(9),RRWDG(9),BDUCT(9),DYOKE(9),TSPIN(9),HPART(9),
     &           BPART(9),ACOND(9),HWIND(9),ZLAG(9),ZNLAG(9),ZCOIAR(9),
     &           EXTRAR(9),SWIND(9),RINNER(9),ZPART(9),ZTULO(9),DCORE,
     &           DWIND,HELP,HLIMB,PI
*
       INTEGER   NGROUP(9),NLOOP(9),NWARN(5),NLORR(9),KWIND(9),JFC,
     &           IWDG, ISTOP
*
       LOGICAL   BBADJU,BBTS,BBERR,BBHELP(10)
*
       DIMENSION NCOL(9)
       CHARACTER NCOL*1,MESTXT*70
*
       EXTERNAL  CORN
*
       DATA      NWARN/0,0,0,0,0/
*
       HWIND(IWDG)=HLIMB-DYOKE(IWDG)
*
C... No. of turns/loop
*
       ZTULO(IWDG)=ZWIND(IWDG)/FLOAT(NLOOP(IWDG))
*
C... Strand height
*
       HELP=(HWIND(IWDG)-EXTRAR(IWDG))/FLOAT(NGROUP(IWDG))
   10  CONTINUE
       IF (KWIND(IWDG).EQ.7.OR.KWIND(IWDG).EQ.10)
     &    HPART(IWDG)=HELP/(FLOAT(NLOOP(IWDG))/FLOAT(NLORR(IWDG))*
     &    (ZTULO(IWDG)+1.))-TSPIN(IWDG)
*
   20  CONTINUE
       IF (KWIND(IWDG).EQ.8.OR.KWIND(IWDG).EQ.11)
     & HPART(IWDG)=(HELP-FLOAT(NLOOP(IWDG)+1)*0.005)/
     &    ((ZTULO(IWDG)+1.)*
     &    FLOAT(NLOOP(IWDG))/FLOAT(NLORR(IWDG)))-TSPIN(IWDG)
*
       IF (KWIND(IWDG).EQ.9)
     &    HPART(IWDG)=(HELP-FLOAT(NLOOP(IWDG)+1)*0.01)/
     &    (FLOAT(NLOOP(IWDG))*(ZTULO(IWDG)+1.))-TSPIN(IWDG)
*
       IF (HPART(IWDG).LE.0.020.OR.KWIND(IWDG).NE.11)  GOTO 300
       KWIND(IWDG)=7
       NLORR(IWDG)=1
       NWARN(4)=0
       MESTXT='LOOP1:SLL2 changed to SLL1 - Winding '//NCOL(IWDG)//
     &        ' strand height > 20 mm )        '
       CALL WMESS (JFC,MESTXT,NWARN(5))
       GOTO 10
*
  300  IF (HPART(IWDG).LE.0.001) THEN
          IF (HPART(IWDG).LE.0.007) GOTO 350
          KWIND(IWDG)=11
          NLORR(IWDG)=2
          NWARN(5)=0
          MESTXT='LOOP1:SLL1 changed to SLL2 - Winding '//NCOL(IWDG)//
     &           ' strand height <  1 mm )        '
          CALL WMESS (JFC,MESTXT,NWARN(4))
          GOTO 20
  350     CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &   'LOOP1:Strand height of winding '//NCOL(IWDG)//' is too low.')
             STOP
*
C... Strand width
*
       END IF
*
       BPART(IWDG)=(ACOND(IWDG)/ZPART(IWDG)+CORN(BPART(IWDG))*
     &              1.E-6)/HPART(IWDG)/NGROUP(IWDG)
*
C... Calculate winding width and inner diameter
*
       RRWDG(IWDG)=(BPART(IWDG)+TSPIN(IWDG)/0.95)*
     &           FLOAT(NLORR(IWDG))+0.0005
       IF (IWDG.EQ.1) THEN
          RINNER(IWDG)=DCORE/2.+BDUCT(IWDG)
       ELSE
          RINNER(IWDG)=RINNER(IWDG-1)+BDUCT(IWDG)+RRWDG(IWDG-1)
       END IF
*
       SWIND(IWDG)=(2.*RINNER(IWDG)+RRWDG(IWDG))*PI
*
C... Check winding strand width
*
       DWIND = 2.*RINNER(IWDG)+RRWDG(IWDG)
       IF(BBADJU.AND.BPART(IWDG).LT.DWIND/200.) THEN
       MESTXT='LOOP1:Winding '//NCOL(IWDG)//' strand width is less '//
     &        'than DWIND/200 (Series loop)     '
          CALL WMESS (JFC,MESTXT,NWARN(1))
       END IF
       IF(BBADJU.AND.BPART(IWDG).LT.0.004) THEN
       MESTXT='LOOP1:Winding '//NCOL(IWDG)//' strand width is less '//
     &        'than 4 mm      (Series loop)     '
          CALL WMESS (JFC,MESTXT,NWARN(2))
       END IF
       IF(BBADJU.AND.BPART(IWDG).LT.1.5*TSPIN(IWDG)) THEN
       MESTXT='LOOP1:Winding '//NCOL(IWDG)//' strand width is less '//
     &        'than 1.5*TSPIN (Series loop)     '
          CALL WMESS (JFC,MESTXT,NWARN(3))
       END IF
*
C... Instructions for TAC and for temperature calculation
*
       ZLAG(IWDG)=1.
       ZCOIAR(IWDG)=1.
       ZNLAG(IWDG)=ZWIND(IWDG)
*
       RETURN
       END
