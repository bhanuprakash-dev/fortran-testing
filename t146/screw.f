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
*      Subroutine SCREW
*      ----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 83-03-18 by H. Westberg  , ZKAB
*      Revised: 87-04-27 by Ron Bell     , TRAFO/IK
*
*  1.  Description
C...       Title:  Winding layout for screw windings
*
************************************************************************
*
       SUBROUTINE SCREW(IWDG)
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
     &  (XZ1426       ,  DCORE         )
       EQUIVALENCE
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ0671(1)    ,  BDUCT  (1)    ),
     &  (XZ0921(1)    ,  NGROUP (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    )
       EQUIVALENCE
     &  (XZ0751(1)    ,  DYOKE  (1)    ),
     &  (XZ1091(1)    ,  TSPIN  (1)    ),
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ0681(1)    ,  BPART  (1)    ),
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ1161(1)    ,  ZCODU  (1)    ),
     &  (XZ2298(1)    ,  NCOL   (1)    )
       EQUIVALENCE
     &  (XZ2584(1)    ,  EXTRAR (1)    ),
     &  (XZ2585(1)    ,  HCLAC  (1)    ),
     &  (XZ2586(1)    ,  ZDISC  (1)    ),
     &  (XZ2589(1)    ,  SWIND  (1)    ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ2590(1)    ,  RINNER (1)    ),
     &  (XZ2591(1)    ,  ZAR    (1)    ),
     &  (XZ2592(1)    ,  ZRR    (1)    ),
     &  (XZ2593(1)    ,  ZPART  (1)    ),
     &  (XZ2555(1)    ,  KWIND  (1)    )
*
       REAL      ZWIND(9),RRWDG(9),BDUCT(9),HWIND(9),DYOKE(9),
     &           TSPIN(9),HPART(9),BPART(9),ACOND(9),ZCODU(9),
     &           EXTRAR(9),HCLAC(9),ZDISC(9),SWIND(9),RINNER(9),
     &           ZAR(9),ZRR(9),ZPART(9),HTILT
*
       INTEGER   NGROUP(9),KWIND(9),JFC,NWARN,ISTOP
*
       LOGICAL    BBTS,BBERR,BBHELP(10)
*
       DIMENSION NCOL(9)
       CHARACTER NCOL*1
*
 11    KW=KWIND(IWDG)
       ZKW=FLOAT(KW)
       RGROUP=FLOAT(NGROUP(IWDG))
*
       HWIND(IWDG)=HLIMB-DYOKE(IWDG)
*
C... Extra clacks at transpositions
*
       IF (KW.GT.1) THEN
          Q=0.
       ELSE
          Q=1.
       END IF
*
C... Number of discs
*
       ZDISC(IWDG)=ANINT(ZWIND(IWDG)+1.0)*RGROUP*ZKW
*
C... Strand height
*
       HPART(IWDG)=((HWIND(IWDG)-EXTRAR(IWDG))/RGROUP-
     &             ZKW*ANINT(ZWIND(IWDG))*HCLAC(IWDG)-HCLAC(IWDG)*
     &             (ZRR(IWDG)+3.)*Q-(ZKW-1.)*HCLAC(IWDG))/
     &             (ZKW*ANINT(ZWIND(IWDG)+1.0))-TSPIN(IWDG)
       HPART(IWDG)=ANINT(HPART(IWDG)*2.E+4)/2.E+4

       IF (HPART(IWDG).LT.0.020.OR.KW.GT.3) GO TO 210
       KWIND(IWDG)=KW+1
       ZAR(IWDG)=KWIND(IWDG)
       NWARN=0
       CALL WMESS
     & (JFC,' Screw winding '//NCOL(IWDG)//
     &                    ' - No. of strands in AR increased.',NWARN)
       GO TO 11
 210   IF (HPART(IWDG).GT.0.001) GO TO 100

       IF (KW.EQ.1) THEN
          CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &  'SCREW:Strand height of winding '//NCOL(IWDG)//' is too low.')
*....    Backtracking
         IF(ISTOP.EQ.1) RETURN
       ENDIF

       KWIND(IWDG)=KW-1
       ZAR(IWDG)=KWIND(IWDG)
       NWARN=0
       CALL WMESS
     & (JFC,' Screw winding '//NCOL(IWDG)//
     &                    ' - No. of strands in AR decreased.',NWARN)
       GO TO 11
*
C... Number of strands
*
  100  ZPART(IWDG)=ACOND(IWDG)/RGROUP/
     &          (HPART(IWDG)*BPART(IWDG)-CORN(BPART(IWDG))*1.E-6)
       IF (ZPART(IWDG).LE.0.) ZPART(IWDG)=15.
*
C... Number of strands in RR : 0.35 limits the width upwards
*
       ZRR(IWDG)=ANINT(ZPART(IWDG)/ZKW+0.35)
 300   ZPART(IWDG)=ZRR(IWDG)*ZKW
       BPART(IWDG)=(ACOND(IWDG)/RGROUP/ZPART(IWDG)+
     &           CORN(BPART(IWDG))*1.E-6)/HPART(IWDG)
       BPART(IWDG)=ANINT(BPART(IWDG)*2.E+4)/2.E+4
*
C... Check the tilting force
*
       HTILT=3.E+3*BPART(IWDG)*BPART(IWDG)
       IF (HPART(IWDG).LT.HTILT.OR.ZRR(IWDG).LT.2.) GO TO 400
       ZRR(IWDG)=ZRR(IWDG)-1.
       GO TO 300
*
C... Calculate winding width and inner diameter
*
 400   RRWDG(IWDG)=(BPART(IWDG)+TSPIN(IWDG)/0.95)*ZRR(IWDG)+
     &          ZCODU(IWDG)*0.005
       RRWDG(IWDG)=ANINT(RRWDG(IWDG)*1.E+5)/1.E+5
       IF (IWDG.EQ.1) THEN
          RINNER(IWDG)=DCORE/2.+BDUCT(IWDG)
       ELSE
          RINNER(IWDG)=RINNER(IWDG-1)+BDUCT(IWDG)+RRWDG(IWDG-1)
       END IF
       SWIND(IWDG)=(2.*RINNER(IWDG)+RRWDG(IWDG))*PI
       RETURN
       END
