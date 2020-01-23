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
*      Function PNOLO
*      --------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates No-load losses
C...               All parameters are REAL and in SI units
*
************************************************************************
*
       REAL FUNCTION PNOLO(TYP,BLIMB,FREQ,OKF,SBF,
     &                     ISTGRD,GB1,GB2,GO,GH,BLA)
*
       REAL      RK2,RK3,TYP,POLY,BLA,BLIMB,FREQ,GB1,GB2,GH,GO,
     &           OKF,PB1,PB2,PH,PO,RED,SBF
*
       REAL      PSP1(3),  PSP2(5),  PSP3(3),
     &           PSPH11(3),PSPH12(5),PSPH13(3),
     &           PSPHD1(3),PSPHD2(5),PSPHD3(3),
     &           PSPH31(3),PSPH32(5),PSPH33(3)
*
       INTEGER    ISTGRD

       LOGICAL    ENFAS,BBSL
*
C... Coefficients for spec. iron losses. Constant term first
*
       DATA PSP1/0.12712500, -0.16000032, 0.42500013/
       DATA PSP2/-27.298521, 68.690477, -63.260419, 25.514539,
     &           -3.6788161/
       DATA PSP3/-2.2174400, 1.2000081, 0.49999783/
*
C... Corner losses for Single phase cores
*
       DATA PSPHD1/0.0316053,-0.21666705,0.50000017/
       DATA PSPHD2/20.530720,-60.188503,66.224117,-32.081477,
     & 5.9078875/
       DATA PSPHD3/91.316943,-103.79994,29.999984/
*
C... Corner losses for T-cores
*
       DATA PSPH11/-0.22683000, 0.50000002, 0.25000000/
       DATA PSPH12/76.056176, -207.05085, 212.08404, -96.271337,
     &             16.470603/
       DATA PSPH13/96.64430, -108.39995, 30.999986/
*
C... Corner losses for TY-3 phase cores
*
       DATA PSPH31/0.13507766,-0.25555532,0.88888878/
       DATA PSPH32/-256.38768,669.21973,-650.81657,
     &              280.18065,-44.805779/
       DATA PSPH33/-38.897559,34.200064,-6.0000171/
*
       DATA RK2/1.13/
*
       PB1=0.
       PB2=0.
       PO=0.
       PH=0.
*
       IF (BLA.EQ.4.) THEN
          RK3=1.16
       ELSE
          RK3=1.
       END IF
*
       ENFAS=(NINT(TYP).EQ.1.OR.
     &        NINT(TYP).EQ.7.OR.
     &        NINT(TYP).EQ.8.OR.
     &        NINT(TYP).EQ.9)

       BBSL=(NINT(TYP).GE.7)
*
       IF (BLIMB.GT.1.8) THEN
          PB1=GB1* POLY(PSP3,BLIMB,3)
          PO = GO* POLY(PSP3,BLIMB/OKF,3)
          IF (BBSL) PB2=GB2* POLY(PSP3,BLIMB/SBF,3)
*
          IF (NINT(TYP).EQ.10) THEN
             PH=GH* POLY(PSPH33,BLIMB,3)
          ELSE IF (ENFAS) THEN
             PH=GH* POLY(PSPHD3,BLIMB,3)
          ELSE
             PH=GH* POLY(PSPH13,BLIMB,3)
          END IF
       ELSE IF (BLIMB.GE.1.3) THEN
          PB1=GB1* POLY(PSP2,BLIMB,5)
          PO =GO * POLY(PSP2,BLIMB/OKF,5)
          IF (BBSL) PB2=GB2* POLY(PSP2,BLIMB/SBF,5)
*
          IF (NINT(TYP).EQ.10) THEN
             PH=GH* POLY(PSPH32,BLIMB,5)
          ELSE IF (ENFAS) THEN
             PH=GH* POLY(PSPHD2,BLIMB,5)
          ELSE
             PH=GH* POLY(PSPH12,BLIMB,5)
          END IF
       ELSE
          PB1=GB1* POLY(PSP1,BLIMB,3)
          PO =GO * POLY(PSP1,BLIMB/OKF,3)
          IF (BBSL) PB2=GB2* POLY(PSP1,BLIMB/SBF,3)
*
          IF (NINT(TYP).EQ.10) THEN
             PH=GH* POLY(PSPH31,BLIMB,3)
          ELSE IF (ENFAS) THEN
             PH=GH* POLY(PSPHD1,BLIMB,3)
          ELSE
             PH=GH* POLY(PSPH11,BLIMB,3)
          END IF
       END IF
*
       PO=PO*RK2
       PH=PH*RK3
*
       PNOLO=(PB1+PB2+PO+PH)*1.02
*
 1100  IF (ISTGRD.LE.1) GOTO 1199
*
       GOTO(1101,1199,1103,1199,1199,1199,1107,1108,1109,1110),NINT(TYP)
*
 1101  CONTINUE
C... Calculate the Reduction for D - Core *
*
       IF (BLIMB.GE.1.85) THEN
          RED =  0.4/1.5*BLIMB + 0.38
       ELSE IF (BLIMB.GE.1.75) THEN
          RED =  0*BLIMB + 0.86
       ELSE IF (BLIMB.GE.1.4) THEN
          RED = -1./3.5*BLIMB + 1.36
       ELSE IF (BLIMB.GE.1.2) THEN
          RED = -0.25/2.*BLIMB + 1.135
       ELSE IF (BLIMB.GE.0.8) THEN
          RED =  0.35/4.*BLIMB + 0.88
       ELSE
          RED = -0.5/8.*BLIMB + 1
       END IF
*
       GOTO 1198
*
 1103  CONTINUE
*
C... Calculate the Reduction for T - Core *
*
       IF (BLIMB.GE.1.80) THEN
          RED =  0.1*BLIMB + 0.705
       ELSE IF (BLIMB.GE.1.75) THEN
          RED =  0*BLIMB + 0.885
       ELSE IF (BLIMB.GE.1.5) THEN
          RED = -1.1/5*BLIMB + 1.27
       ELSE IF (BLIMB.GE.1.3) THEN
          RED =  0*BLIMB + 0.94
       ELSE IF (BLIMB.GE.0.8) THEN
          RED =  0.7/5*BLIMB + 0.758
       ELSE
          RED = -1.3/8*BLIMB + 1
       END IF
*
       GOTO 1198
*
 1107  CONTINUE
C... Calculate the Reduction for EY - Core *
*
       IF (BLIMB.GE.1.75) THEN
          RED = -0.1*BLIMB + 0.985
       ELSE IF (BLIMB.GE.1.6) THEN
          RED = -1./3*BLIMB + 4.18/3
       ELSE IF (BLIMB.GE.1.4) THEN
          RED = -0.1*BLIMB + 1.02
       ELSE IF (BLIMB.GE.0.8) THEN
          RED =  0.5/6*BLIMB + 2.29/3
       ELSE
          RED = -1.7/8*BLIMB + 1
       END IF
*
       GOTO 1198
*
 1108  CONTINUE
C... Calculate the Reduction for DY - Core *
*
       IF (BLIMB.GE.1.75) THEN
          RED = -0.1*BLIMB + 0.985
       ELSE IF (BLIMB.GE.1.6) THEN
          RED = -1./3*BLIMB + 4.18/3
       ELSE IF (BLIMB.GE.1.4) THEN
          RED = -0.1*BLIMB + 1.02
       ELSE IF (BLIMB.GE.0.8) THEN
          RED =  0.5/6*BLIMB + 2.29/3
       ELSE
          RED = -1.7/8*BLIMB + 1
       END IF
*
       GOTO 1198
*
 1109  CONTINUE
C... Calculate the Reduction for  TY-1  Core *
*
       IF (BLIMB.GE.1.7) THEN
          RED =  0.87
       ELSE IF (BLIMB.GE.1.2) THEN
          RED =  -1.1/5*BLIMB + 6.22/5
       ELSE
          RED =  1
       END IF
*
       GOTO 1198
*
 1110  CONTINUE
C... Calculate the Reduction for  TY-3  Core *
*
       IF (BLIMB.GE.1.7) THEN
          RED =  0.9
       ELSE IF (BLIMB.GE.1.2) THEN
          RED =  -0.8/5*BLIMB + 5.86/5
       ELSE
          RED =  1
       END IF
*
       GOTO 1198
*
 1198  PNOLO  =  PNOLO * RED
*
 1199  CONTINUE
*
C... Quality 4 (Plasmajet) inserted 90.01.11
*
       IF (ISTGRD.EQ.3.OR.ISTGRD.EQ.4) THEN
*
       IF (BLIMB.LT.1.4) THEN
          PNOLO=PNOLO*(0.08713*BLIMB+0.6189848)*0.96
       ELSE
          PNOLO=PNOLO*(3.7922*BLIMB**4-25.7589*BLIMB**3
     &          +65.05881*BLIMB**2-72.41348*BLIMB+30.71891)*0.96
       END IF
*
C... Change after Memo 870924 from ZKDA  C. Bengtsson
C... 'Justering av f„rlustv¢rdena f„r 0.23 mm ZDKH och 0.30 mm M5'
*
       PNOLO = PNOLO * 0.995
*
C... Change after Memo 891005 from TDA  C. Bengtsson
C... 'JUSTERING F„R KVALITET PLJT'
*
       IF (ISTGRD.EQ.4) PNOLO = PNOLO * 1.13
*
       END IF
*
C... M5 - Core Plate Quality
*
C... Change after Memo 870924 from ZKDA  C. Bengtsson
C... 'Justering av f„rlustv¢rdena f„r 0.23 mm ZDKH och 0.30 mm M5'
*
       IF (ISTGRD.EQ.1) PNOLO = PNOLO * 0.965
*
C... ARLS ARMCO LASER SCRIBED FOR UAW, 89 % OF HIBI
*
       IF (ISTGRD.EQ.5) PNOLO = 0.89*PNOLO

       IF (ISTGRD.GE.11) THEN
C... MORE REDUCTION FOR M5L-QUAL.
*
       IF (BLIMB.GE.1.5) THEN
          RED = -0.1/2*BLIMB + 1.045
       ELSE IF (BLIMB.GE.0.8) THEN
          RED =  0*BLIMB + 0.97
       ELSE
          RED = -0.3/8*BLIMB + 1
       END IF
*
       PNOLO = PNOLO * RED
*
       END IF
*
       IF (FREQ.GT.55.) PNOLO=PNOLO*1.33/1.02
*
 199   CONTINUE
       RETURN
       END
