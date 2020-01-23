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
*      Function SNOLO
*      --------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the magnetising power
*
************************************************************************
*
       REAL FUNCTION SNOLO(TYP,BLIMBP,FREQ,OKF,SBF,ISTGRD,
     &                     GB1,GB2,GO,GH,BLADN)
*
       REAL      RK2,RK3,TYP,BLADN,BLIMBP,FREQ,GB1,GB2,GH,GO,OKF,POLY,
     &           SB1,SB2,SBF,SH,SO
*
       REAL      SSP1(3),SSP2(5),SSP3(3),
     &           SSPH11(3),SSPH12(5),SSPH13(3),
     &           SSPH21(3),SSPH22(5),SSPH23(3)
*
       INTEGER   ISTGRD
*
       LOGICAL   BBSL
*
C... Coefficients for specific magnetising losses (Limb and Yoke)
C... Constant term first
*
       DATA      SSP1(1)/ 0.88778    /
       DATA      SSP1(2)/-0.92500007 /
       DATA      SSP1(3)/ 0.75000003 /
*
       DATA      SSP2(1)/ 355.98739  /
       DATA      SSP2(2)/-975.50703  /
       DATA      SSP2(3)/1004.1630   /
       DATA      SSP2(4)/-459.88116  /
       DATA      SSP2(5)/  79.285748 /
*
       DATA      SSP3(1)/ 1085.6455  /
       DATA      SSP3(2)/-1202.1996  /
       DATA      SSP3(3)/  333.99989 /
*
C... Corner losses for 1-phase cores and the T-core
*
       DATA      SSPH11(1)/ 0.0626    /
       DATA      SSPH11(2)/ 1.5333336 /
       DATA      SSPH11(3)/-0.22222236/
*
       DATA      SSPH12(1)/  463.08928/
       DATA      SSPH12(2)/-1252.4051 /
       DATA      SSPH12(3)/ 1302.5710 /
       DATA      SSPH12(4)/ -618.75868/
       DATA      SSPH12(5)/  113.71649/
*
       DATA      SSPH13(1)/ 7077.4378 /
       DATA      SSPH13(2)/-7797.5977 /
       DATA      SSPH13(3)/ 2151.9994 /
*
C... Corner losses for the TY-3 Core
*
       DATA      SSPH21(1)/0.         /
       DATA      SSPH21(2)/1.95938    /
       DATA      SSPH21(3)/0.         /
*
       DATA      SSPH22(1)/  5640.5044/
       DATA      SSPH22(2)/-15359.5830/
       DATA      SSPH22(3)/ 15684.7110/
       DATA      SSPH22(4)/ -7123.4939/
       DATA      SSPH22(5)/  1215.8733/
*
       DATA      SSPH23(1)/ 18535.248 /
       DATA      SSPH23(2)/-20449.994 /
       DATA      SSPH23(3)/  5649.9984/
*
       DATA      RK2      /1.13       /
*
       SB1=0.
       SB2=0.
       SO=0.
       SH=0.
*
       IF (BLADN.EQ.4.) THEN
          RK3=1.2
       ELSE
          RK3=1.
       END IF
*
       BBSL=(TYP.GE.7.)
*
       IF (BLIMBP.GT.1.8) THEN
          SB1=GB1*POLY(SSP3,BLIMBP,3)
          SO=GO*POLY(SSP3,BLIMBP/OKF,3)
          IF (BBSL) SB2=GB2*POLY(SSP3,BLIMBP/SBF,3)
*
          IF (TYP.EQ.10.) THEN
             SH=GH*POLY(SSPH23,BLIMBP,3)
          ELSE
             SH=GH*POLY(SSPH13,BLIMBP,3)
          END IF
       ELSE IF (BLIMBP.GE.1.3) THEN
          SB1=GB1*POLY(SSP2,BLIMBP,5)
          SO=GO*POLY(SSP2,BLIMBP/OKF,5)
          IF (BBSL) SB2=GB2*POLY(SSP2,BLIMBP/SBF,5)
*
          IF (TYP.EQ.10.) THEN
             SH=GH*POLY(SSPH22,BLIMBP,5)
          ELSE
             SH=GH*POLY(SSPH12,BLIMBP,5)
          END IF
       ELSE
          SB1=GB1*POLY(SSP1,BLIMBP,3)
          SO=GO*POLY(SSP1,BLIMBP/OKF,3)
          IF (BBSL) SB2=GB2*POLY(SSP1,BLIMBP/SBF,3)
*
          IF (TYP.EQ.10.) THEN
             SH=GH*POLY(SSPH21,BLIMBP,3)
          ELSE
             SH=GH*POLY(SSPH11,BLIMBP,3)
          END IF
       END IF
*
       SO=SO*RK2
       SH=SH*RK3
       SNOLO=(SB1+SB2+SO+SH)
*
       IF (FREQ.GT.55.) THEN
          SNOLO=SNOLO*1.25
       ELSE
          SNOLO=SNOLO*1.04
       ENDIF
*
C... Correction for HI-B, ZDKH, PLJT and ARMS
*
       IF ((ISTGRD.EQ.2).OR.
     &     (ISTGRD.EQ.3).OR.
     &     (ISTGRD.EQ.4).OR.
     &     (ISTGRD.EQ.5)) SNOLO=SNOLO*0.7
*
       RETURN
       END
