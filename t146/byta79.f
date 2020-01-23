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
*      Subroutine BYTA79
*      -----------------
*
C...   Title:  Calculation of comparison price in MNL79
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 83-03-18 by H.Westberg , ZKAB
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 88-11-03 by Per S�derberg, TRAFO/IK
*
************************************************************************
*
       SUBROUTINE BYTA79
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       REAL            RES79 (241,6)
       COMMON /KONSRE/ RES79
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ1380       ,  BBEXAC        ),
     &  (XZ1415       ,  COST          ),
     &  (XZ1416       ,  COSTTP        ),
     &  (XZ1417       ,  CPUFIX        )
       EQUIVALENCE
     &  (XZ1420       ,  CPUPT         ),
     &  (XZ1422       ,  CTROVN        ),
     &  (XZ1436       ,  PKLPRM        ),
     &  (XZ1452       ,  GACTP         ),
     &  (XZ1453       ,  GCONS         )
       EQUIVALENCE
     &  (XZ1457       ,  GCOVER        ),
     &  (XZ1465       ,  GTANK         ),
     &  (XZ1466       ,  GTRP          ),
     &  (XZ1483       ,  MNLY          ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1570       ,  ZTRANS        ),
     &  (XZ2263       ,  KODL          ),
     &  (XZ2264       ,  KODP          )
*
       REAL      AARR(200),GTRP,COST,COSTTP,CPUFIX,CPUPT,CS,
     &           CMANUF,CSTPSW,CTROVN,GACTP,GCONS,GCOVER,GS,GTANK,
     &           HELP,PKLPRM,SUMWIN,ZTRANS
*
       INTEGER   I,IWDG,KODL,KODP,KSAVE,MNLY,NWILI
*
       LOGICAL   BBEXAC
*
CC*SEBL End of the declarations block.
*
C... Local Functions
*
CC*SBBL Start of the functions block.
*
       GS(I)=RES79(I,1)+RES79(I,4)
       CS(I)=RES79(I,3)+RES79(I,5)+RES79(I,6)
*
CC*SEBL End of the functions block.
*
*
C... MNL79 preparation of costs
*
*
C... Windings
*
CC*SBBL Start of the windings block.
*
       SUMWIN=0.
*
C... Summations for all windings
*
       DO 299 IWDG=1,NWILI
  299  SUMWIN=SUMWIN+GS(IWDG*19)

*
CC*SEBL End of the windings block.
*
C... Calculations
*
CC*SBBL Start of the calculations block.
*
       GACTP=SUMWIN+GS(167)+GS(179)+GS(196)+GS(236)
*
C... (167) = Core Subtotal
C... (179) = Cleats & leads Subtotal
C... (196) = Active part others Subtotal
C... (236) = Oil absorbed
*
C... Transport mass calculation changed 88-02-04
*
       GTRP=GACTP+GS(200)+GS(213)
*
C... (200) = Tank manuf. + field screen
C... (213) = Cover subtotal
*
C... (203) = Tank Subtotal
C... (207) = Cooling equipment Subtotal
C... (222) = Pipe connections
C... (226) = Control cabling
C... (230) = Sound screens
C... (233) = Final Assembly excluding oil
C... (201) = Radiators
C... (223) = Fans
*
CCCCC  GTRP=GACTP+GS(203)+GS(213)+GS(207)+GS(222)+
CCCCC&           GS(226)+GS(230)+GS(233)-GS(201)-GS(223)
*
       GTANK=GS(200)
       GCOVER=GS(211)
       GCONS=GS(217)
       write(*,*) "CS(234)=",CS(235)
       write(*,*) "CS(200)=",CS(201)
       write(*,*) "CS(222)=",CS(223)
       CTROVN=CS(235)-CS(201)-CS(223)
*
C--- CMANUF = Manufacturing cost in comparison price
*
       RES79(241,3)=RES79(241,3)
       CMANUF=CS(241)
*
       COST=CMANUF
       COST=COST+CPUPT*CTROVN
*
*
C... MNL87 modification :
C... Packing costs CPACK = 0 : they are now included in 'Dismantling'
C... as per Memo of 88-06-06 Seppo Ylikoski COMP/K
*
*
       COST=(COST)*(1.+CPUFIX)
*
CC*SEBL End of the functions block.
*
       RETURN
       END
