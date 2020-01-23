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
*      Subroutine PAGE79
*      -----------------
*
C...   Title:  Cost printout routine
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE PAGE79
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       REAL               RES79(241,6)
       COMMON   /KONSRE/  RES79
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ0301(1)    ,  IARR   (1)    ),
     &  (XZ1422       ,  CTROVN        )
       EQUIVALENCE
     &  (XZ1466       ,  GTRP          ),
     &  (XZ1483       ,  MNLY          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1587       ,  CHCOST        ),
     &  (XZ1632(1)    ,  RUBCH  (1)    )
*
       REAL      AARR(200),CMANUF,COLE,CS,
     &           CSH,CTROVN,CWDG,GS,GSH,GTRP,GWDG
*
       INTEGER   IARR(100),I,IL1,IL2,IL3,IPOS1,IPOS2,IPOS3,IRAD,JFC,
     &           MNLY,NWILI
*
       DIMENSION RUBCH(15),KOL1(58),KOL2(58)
       CHARACTER LINE*9,VALUE*9,CHCOST*6,
     &           RUBCH*4,KOL1*33,KOL2*33
*
CC*SEBL End of the declarations block.
*
       DATA      IPOS1,IL1,IPOS2,IL2,IPOS3,IL3 /1,15,16,9,25,9/,
     &           LINE /'   ------'/
*
       GS(I)=RES79(I,1)+RES79(I,4)
       CS(I)=RES79(I,3)+RES79(I,5)+RES79(I,6)
*
       GWDG=0.
       CWDG=0.
*
C... Abbreviate some variables
*
       DO 100 I=1,NWILI
       GWDG=GWDG+GS(I*19)
  100  CWDG=CWDG+CS(I*19)
*
C... Screening of tank is separated from statement 200
*
CC*SBBL Start of the tank screening block.
*
       GSH=GS(200)-GS(197)-GS(198)-GS(199)
       CSH=CS(200)-CS(197)-CS(198)-CS(199)
*
CC*SEBL End of the tank screening block.
*
C... Set columns to blanks
*
       DO 1 I=1,58
       KOL1(I)=' '
    1  KOL2(I)=' '
*
C... New page & heading
*
       CALL NPAGE(0)
       WRITE(JFC,50)
       WRITE(JFC,90)  (RUBCH(I),I=1,13)
       WRITE(JFC,10) 'OVNV','kg',CHCOST,'m3 (OVNF)','KG',CHCOST
*
C... Costs for index calculation
*
CC*SBBL Start of the cost index block.
*
       CMANUF=CS(241)
       COLE=CS(201)+CS(223)+CS(239)
*
CC*SEBL End of the cost index block.
*
C... Give values to the left side of the first page
*
CC*SBBL Start of the left side block.
*
       IRAD=0
       IRAD=1+IRAD
*
C... Create the required character string
*
       CALL CONCAT(KOL1(IRAD),IPOS1,'Windings       ',1,IL1)
*
CC*SEBL End of the left side block.
*
CC*SOFF Switch off the structure analysis (Not all calls need be shown)
*
       WRITE(VALUE,11) GWDG
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CWDG
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Transposed cond',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Glued conductor',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Core           ',1,IL1)
       WRITE(VALUE,11) GS(167)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(167)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Cleats & Leads.',1,IL1)
       WRITE(VALUE,11) GS(179)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(179)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Active part Oth',1,IL1)
       WRITE(VALUE,11) GS(196)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(196)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Tank           ',1,IL1)
       WRITE(VALUE,11) GS(203)-GS(201)-GSH
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(203)-CS(201)-CSH
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Turret for bush',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Tank for tap-ch',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Wheel/Bogie/Ski',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Tank shield    ',1,IL1)
       WRITE(VALUE,11) GSH
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CSH
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Radiators      ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Fans           ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Assy.Cooling.eq',1,IL1)
       WRITE(VALUE,11) GS(207)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(207)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Cover          ',1,IL1)
       WRITE(VALUE,11) GS(213)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(213)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Turret for bush',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Stainless steel',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Conservator    ',1,IL1)
       WRITE(VALUE,11) GS(217)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(217)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Rubber air cell',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Pipe connec.   ',1,IL1)
       WRITE(VALUE,11) GS(222)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(222)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Control cables ',1,IL1)
       WRITE(VALUE,11) GS(226)-GS(223)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(226)-CS(223)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Conduits       ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Sound screens  ',1,IL1)
       WRITE(VALUE,11) GS(230)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(230)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Final assembly ',1,IL1)
       WRITE(VALUE,11) GS(233)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(233)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Disassy+Packing',1,IL1)
       WRITE(VALUE,11) GS(234)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(234)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Short circ.dev.',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Accessories    ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS2,LINE             ,1,IL2)
       CALL CONCAT(KOL1(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'SUM OVNV (COMP)',1,IL1)
       WRITE(VALUE,11) GS(235)-GS(201)-GS(223)
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(235)-CS(201)-CS(223)
       CALL CONCAT(KOL1(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'SUM Extras     ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS2,LINE             ,1,IL2)
       CALL CONCAT(KOL1(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'SUM OVNV       ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),4,'MASS   CALCULATIONS (kg)',1,24)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),4,'------------------------',1,24)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'SUM OVNV       ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'SUM M3         ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS2,LINE             ,1,IL2)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Tot.mass(E.oil)',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Transport mass ',1,IL1)
       WRITE(VALUE,11) GTRP
       CALL CONCAT(KOL1(IRAD),IPOS2,VALUE            ,1,IL2)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'Additions      ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS2,LINE             ,1,IL2)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL1(IRAD),IPOS1,'SUM Transp.mass',1,IL1)
*
CC*SON  Switch on the structure analysis (Not all calls need be shown)
*
C... Give values to the right side of the first page
*
CC*SBBL Start of the right side block.
*
       IRAD=0
*
       IRAD=1+IRAD
*
C... Create the required character string
*
       CALL CONCAT(KOL2(IRAD),IPOS1,'Bushings HV    ',1,IL1)
*
CC*SEBL End of the right side block.
*
CC*SOFF Switch off the structure analysis (Not all calls need be shown)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Bushings ZERO  ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Bushings LV    ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Bushings TV    ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Current transf.',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Tap-changer    ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Tap-ch.contr.eq',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Off-cir.tap-ch.',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Coolers        ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Control cabinet',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Impregnated oil',1,IL1)
       WRITE(VALUE,11) GS(236)
       CALL CONCAT(KOL2(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(236)
       CALL CONCAT(KOL2(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS2,LINE             ,1,IL2)
       CALL CONCAT(KOL2(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'SUM M3(Exc.oil)',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'M3 Corr. factor',1,IL1)
       CALL CONCAT(KOL2(IRAD),IPOS2,' :       '      ,1,IL2)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'(PG 0.1 of ZKI ',1,IL1)
       CALL CONCAT(KOL2(IRAD),IPOS2,'4575-111)'      ,1,IL2)
       CALL CONCAT(KOL2(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Corrected M3(Ex',1,IL1)
       CALL CONCAT(KOL2(IRAD),IPOS2,'c.oil)   '      ,1,IL2)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),6,'PRICE CALCULATION',1,17)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),6,'-----------------',1,17)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),27,CHCOST,1,4)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'SUM OVNV       ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Tender error % ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Routine test   ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Processing     ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'SUM Tests      ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'TOTAL OVNV     ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'PT             ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'               ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'Transport cost ',1,IL1)
CCC    WRITE(VALUE,11) CSTPSW
CCC    CALL CONCAT(KOL2(IRAD),IPOS2,VALUE            ,1,IL2)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'SUM M3(Exc.oil)',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'SUM Oil        ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'SUM Design(pu) ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'TOTAL OVNF     ',1,IL1)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       WRITE(KOL2(IRAD),102)
CCCCC&      1.E+2*CMANUF/(CTROVN+COLE+CS(236)+CS(237))
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       WRITE(KOL2(IRAD),103)
*
       IRAD=1+IRAD
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'  Price/kg     ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'  --------     ',1,IL1)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'OVNF Price/Transp.mass',1,22)
*
       IRAD=1+IRAD
       CALL CONCAT(KOL2(IRAD),IPOS1,'(         /         )=',1,22)
*
CC*SON  Switch on the structure analysis (Not all calls need be shown)
*
C... Preparing for the printout of Page 6
*
       DO 2 I=1,58
       WRITE(JFC,12) KOL1(I),KOL2(I)
    2  CONTINUE
*
C... Printout of Page 6
*
       CALL PAG79X (KOL1,KOL2,JFC,CHCOST,
     &              IPOS1,IPOS2,IPOS3,IL1,IL2,IL3,
     &              LINE,VALUE)
*
C... Printout of the second costs page
*
       DO 6 I=1,58
       WRITE(JFC,12) KOL1(I),KOL2(I)
    6  CONTINUE
*
       RETURN
   10  FORMAT(11X,A4,10X,A4,6X,A6,8X,A9,5X,A4,4X,A6)
   11  FORMAT(F9.0)
   12  FORMAT(6X,A33,6X,A33)
   50  FORMAT(1X,'     P R E C A L C U L A T I O N  **')
   90  FORMAT(6X,13A4)
  101  FORMAT(F8.4,1X)
  102  FORMAT('                          ')
  103  FORMAT('                          ')
       END
