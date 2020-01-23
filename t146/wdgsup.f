*********************************************************************
*                                                                   *
*      The copyright to the computer program herein is  the         *
*      property of ABB  TRANSFORMERS, Sweden.  The  program         *
*      may be used or copied only with the written  permis-         *
*      sion of ABB  TRANSFORMERS or in accordance with  the         *
*      terms of agreement under which the program has  been         *
*      supplied.                                                    *
*                                                                   *
*      In no event shall ABB  TRANSFORMERS  be  liable  for         *
*      incidental or consequential damages arising from use         *
*      of this program.                                             *
*                                                                   *
*********************************************************************

************************************************************************
*
*      Subroutine WDGSUP
*      -----------------
*
*  0.  Written: XX-XX-XX by S-E Jansson  , ZKB
*      Revised: 86-12-09 by J. Akesson   , Z1KDC
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the dimensions of the winding supports
*
************************************************************************
*
       SUBROUTINE WDGSUP(RK,RIADON,KTEST,DYL1,DCOR,H01,H11,H21,
     &                   B,TWSUPC,BBSLIM,TANSL1,WMARG,TMARG)
*
       REAL       AREA,B,BSUP,BMARG,BTEST,DCORI,DCOR,DYL,DYL1,
     &            FI1,FI2,FI3,FI4,FACT,H,H01,H1,H11,H2,H21,HWSUPC,
     &            TWSUPC,HMARG,RK,RR,TANSL,TANSL1,TMARG,WMARG,RIADON
*
       INTEGER    IHELP,KTEST

       LOGICAL BBSLIM

       include'consts.h'
*
C... Start
*
       HWSUPC= (TWSUPC-6)/1000.
       H= H01/1000.
       H1= H11/1000.
       H2= H21/1000.
       DYL =DYL1/1000.
       DCORI= DCOR/1000.
       RR= DYL-DCORI
       FI1= RIADON*1.E-6
       FI2= B*0.25*(PI/4.*(DYL+DCORI)*RR-DCORI*RR)
       KTEST= 0
       IHELP= 0
*
C... Height of the Winding Support
*
       IF (TWSUPC.EQ.40.OR.TWSUPC.EQ.60.OR.TWSUPC.EQ.80) THEN
*
C... Height of the Winding Support GIVEN
C... Width of the Winding Support
*
          BSUP= (FI1+FI2)/(0.42*RR*HWSUPC)
          HMARG= 1.5/BSUP
          IF (BSUP.GT.1.5) THEN
*
C... Calculate a new height & width
*
             HWSUPC= 1.6/RR*(FI1+FI2)
             TWSUPC= IFIX(HWSUPC*1000.)
             IF (HWSUPC.LT.0.074) TWSUPC= 80
             IF (HWSUPC.LT.0.054) TWSUPC= 60
             IF (HWSUPC.LT.0.034) TWSUPC= 40
             HMARG= (TWSUPC-6)/HWSUPC/1000.
             HWSUPC= (TWSUPC-6)/1000.
          END IF
*
C... Calculate the Height of the Winding Support
*
       ELSE
          HWSUPC= 1.6/RR*(FI1+FI2)
          TWSUPC= IFIX(HWSUPC*1000.)
          IF (HWSUPC.LT.0.074) TWSUPC= 80
          IF (HWSUPC.LT.0.054) TWSUPC= 60
          IF (HWSUPC.LT.0.034) TWSUPC= 40
          HMARG= (TWSUPC-6)/HWSUPC/1000.
          HWSUPC= (TWSUPC-6)/1000.
       END IF
*
       AREA= (H1+H2)/2.*(RK*HWSUPC-.015)
       FACT= H+RK*HWSUPC-.015
       FI4= B*0.5*RR*FACT
*
C... Outer limb?
*
 400   IF (BBSLIM) THEN
*
C... Yes
*
          BTEST= (FI2+FI4)/AREA
*
C... Connection Plate
*
       ELSE
          FI3= B*0.5*RR*(FACT+HWSUPC)
          TANSL= 0.67*(FI1+FI2+FI3)/FACT
          TANSL1= TANSL*1000.
          TANSL1= INT(TANSL1/10.)*10.+10.
          IF (TANSL1.LT.120.)  TANSL1=120.
          IF (TANSL1.GT.300.)  IHELP= 1
          TMARG= TANSL1/TANSL/1000.
          BTEST= 2.*(FI2+FI4)/AREA
       END IF
*
C... B in Yoke <=1.5 ?
*
 500   IF (BTEST.GT.1.5.OR.IHELP.EQ.1) THEN
*
C... New RK
*
          KTEST= 1
*
C... B is OK
*
       ELSE
          BMARG= 1.5/BTEST
          WMARG= (MIN(HMARG,BMARG)-1.)*100.
          TMARG= (TMARG-1.)*100.
       END IF
*
       RETURN
       END
