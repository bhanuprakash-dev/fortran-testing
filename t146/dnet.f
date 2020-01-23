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
*      Subroutine DNET
*      ---------------
*
*  0.  Written: XX-XX-XX by S-E Jansson  , ZKB
*      Revised: 86-12-09 by J Akesson    , Z1KDC
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the Net Core Diameter
*
************************************************************************
*
       SUBROUTINE DNET(KSERIE,KOD,HLIMB,ACCTR,DCOR,DCORN,T1C,T2C)
*
       REAL      ACCTR,RLCC,RLKRMC,RMC,RLCO,RLKRMO,RMO,T1O,T2O,
     &           TAS,T,TKR,TBU,TPA,DCOR,DCORN,HLIMB,
     &           T1C,T2C,TOL,TTK,TBAND,TISOL
*
       INTEGER   N1O,N2O,N1C,N2C,NBAND,KSERIE,KOD
*
       TISOL=2.
       TBAND=1.
*
C... Tolerance for ovality and crookedness
*
       TOL= AINT(0.001667*DCOR+5)
*
C... KOD=0 for GLUED core
C... KOD=1 for ASECOND-BANDED core
C... KOD=2 for STEEL-BANDED core
*
       IF (KOD.EQ.0) THEN
          TTK=1.5
          IF (DCOR.GT.400.)TTK=2.
          IF (DCOR.GT.600.)TTK=3.5
          TOL=0.01*DCOR
          IF (TOL.LT.3.)TOL=3.
          DCORN=AINT(DCOR-TTK-TOL)
*
       ELSE IF (KOD.EQ.1) THEN
*
          CALL ASEC(DCOR,HLIMB,ACCTR,
     &              N1C,N2C,T1C,T2C,RLCC,RLKRMC,RMC,
     &              N1O,N2O,T1O,T2O,RLCO,RLKRMO,RMO)
*
          IF (KSERIE.EQ.4) THEN
             TTK = 2.
             IF (DCOR.GT.600.) TTK = 3.
             IF (DCOR.GT.1000.) TTK = 4.
             TAS = 1.5
             TBU = 2.
             TPA = 1.5
             TKR = 2.
             DCORN  = DCOR-2.*(T2C+0.7)-2.*ABS(TKR)-
     &             SQRT(TTK**2+TAS**2+TBU**2+TPA**2)
          ELSE
             T=T2C+0.3+0.4
             DCORN=DCOR-ANINT(2.*T+TOL)
          END IF
*
       ELSE IF (KOD.EQ.2) THEN
*
          IF (DCOR.GT.1000.) THEN
             NBAND=2
          ELSE
             NBAND=1
          END IF
*
          DCORN=DCOR-ANINT(2.*(TISOL+TBAND*NBAND)+TOL)
       END IF
*
       RETURN
       END
