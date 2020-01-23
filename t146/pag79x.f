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
*      Subroutine PAG79X
*      -----------------
*
C...   Title:  Auxiliary printout of cost calculations
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE PAG79X(KOL1,KOL2,JFC,CHCOST,
     &                   IPOS1,IPOS2,IPOS3,IL1,IL2,IL3,
     &                   LINE,VALUE)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL            RES79(241,6)
       COMMON /KONSRE/ RES79
*
       REAL      CS,GS
*
       INTEGER   I,IL1,IL2,IL3,IPOS1,IPOS2,IPOS3,IRAD,JFC
*
       DIMENSION KOL1(58),KOL2(58)
       CHARACTER KOL1*33,KOL2*33,
     &           LINE*9,VALUE*9,CHCOST*6
*
CC*SEBL End of the declarations block.
*
C... Functions
*
CC*SBBL Start of the functions block
*
       GS(I)=RES79(I,1)+RES79(I,4)
       CS(I)=RES79(I,3)+RES79(I,5)+RES79(I,6)
*
CC*SEBL End of the functions block
*
C... Set columns to blanks on the second costs page
*
       DO 5 I=1,58
       KOL1(I)=' '
    5  KOL2(I)=' '
*
C... New page & heading
*
       CALL NPAGE(0)
       WRITE(JFC,51)
       WRITE(JFC,30) 'DESIGN'   , 'OIL',
     &               '------'   , '---',
     &               'HOURS RATE',CHCOST,           'kg', CHCOST
*
C... Give values to the left side of the second page
*
CC*SBBL Start of the 'Left' Block
*
       IRAD=0
*
       IRAD=IRAD+1
*
C... Set up the string required.
*
       CALL CONCAT(KOL1(IRAD),IPOS1,'Design         ',1,IL1)
*
CC*SOFF Switch off structure analyser (Not all calls need be shown)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'Drawings       ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'Preparat.      ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'Control.Equip. ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'Tap-changer    ',1,IL1)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS2,LINE            ,1,IL2)
       CALL CONCAT(KOL1(IRAD),IPOS3,LINE            ,1,IL3)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'SUM Design     ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'SUM Design(pu) ',1,IL1)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'  Tender error ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'  ------------ ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'F1=      /(   *        )=',1,25)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'P1=      %     ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'P2=      %     ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'P3=      %     ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'P4=      %     ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL1(IRAD),IPOS1,'  ------       ',1,IL1)
*
CC*SON  Switch on structure analyser (Not all calls need be shown)
*
CC*SEBL End of the 'Left' Block
*
C... Give values to the right side of the second page
*
CC*SBBL Start of the 'Right' Block
*
       IRAD=0
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
*
C... Set up the string required.
*
       CALL CONCAT(KOL2(IRAD),IPOS1,'Oil in tank 98%',1,IL1)
*
CC*SOFF Switch off structure analyser (Not all calls need be shown)
*
       WRITE(VALUE,11) GS(237)*0.925
       CALL CONCAT(KOL2(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(237)*0.925
       CALL CONCAT(KOL2(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'Oil in cons. 8%',1,IL1)
       WRITE(VALUE,11) GS(237)*0.075
       CALL CONCAT(KOL2(IRAD),IPOS2,VALUE            ,1,IL2)
       WRITE(VALUE,11) CS(237)*0.075
       CALL CONCAT(KOL2(IRAD),IPOS3,VALUE            ,1,IL3)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'Oil in coolers ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'Oil in tap-chgr',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'Oil in turrets ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS2,LINE             ,1,IL2)
       CALL CONCAT(KOL2(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'SUM Oil        ',1,IL1)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'     TEST      ',1,IL1)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'     ----      ',1,IL1)
       CALL CONCAT(KOL2(IRAD),29,CHCOST,1,4)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'Applied test   ',1,IL1)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'Impulse test   ',1,IL1)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'Induced test   ',1,IL1)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'PD-measurement ',1,IL1)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'SUB-TOTAL(pu)  ',1,IL1)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'Heat test(pu)  ',1,IL1)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'Sound test (pu)',1,IL1)
*
       IRAD=IRAD+1
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS3,LINE             ,1,IL3)
*
       IRAD=IRAD+1
       CALL CONCAT(KOL2(IRAD),IPOS1,'SUM Tests      ',1,IL1)
*
CC*SON  Switch on structure analyser (Not all calls need be shown)
*
CC*SEBL End of the 'Right' Block
*
       RETURN
*
   11  FORMAT(F9.0)
   30  FORMAT(11X,A6,33X,A3/11X,A6,33X,A3/
     &        21X,A12,3X,A6,25X,A2,5X,A6)
   51  FORMAT(1X,'     PRECALCULATION  **')
*
       END
