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
*      Function NCDTAD
*      ---------------
*
*  0.  Written: 89-10-02 by B-G Bladh     , SETFO/K1
*      Revised: 90-02-12 by Per Sãderberg , SETFO/IK
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*      Revised: 92-04-09 by B-G Bladh     , SETFO/TS;
*               BBZDKH and BBPLJT replaced by ISTGRD.
*               ISTGRD = 1 :  M5   Core lamination quality
*               ISTGRD = 2 :  HI-B Core lamination quality
*               ISTGRD = 3 :  ZDKH Core lamination quality
*               ISTGRD = 4 :  PLJT Core lamination quality
*
*  1.  Description
C...   Title:  Calculate number of cooling ducts in TAD and TAA cores.
*
C...   Technical reference:  Report K 89-63  Kurt Gramm  1989-08-21.
C...   This report is extended by K Gramm also for TAA, and TAD cores.
*
************************************************************************
*
       INTEGER FUNCTION NCDTAD(DCOR,TOPOIL,BLIMB,FREQ,
     &                         BBSTLP,ISTGRD)
*
C... DCOR    Core Diameter  (mm)
C... TOPOIL  Top oil temperature
C... BLIMB   Maximum flux density (T)
C... FREQ    Frequency  (Hz)
C... BBSTLP  step-lap core   (.T.  OR .N.)
*
       REAL     F,X1,X2,P1000,PDK,TOPOIL,DCOR,FREQ,BLIMB
*
       INTEGER  NCD, ISTGRD
*
       LOGICAL  BBSTLP
*
C... Evaluation of straight line between
C... BLIMB = 1,5 T and BLIMB = 1,95 T
C... End point values are X1, AND X2.
*
       F(X1,X2) =  1.95 - 0.45*(PDK-X1)/(X2-X1)
*
C... Top oil parameter PDK.
C... A new fictitious diameter is calculated which
C... is less than the original one, but with 90 degrees top oil.
C... (This will give the same number of cooling ducts)
*
       P1000=(2.**(0.1*(TOPOIL-50.))-1.)*25.+625.
       PDK = (DCOR*(P1000*0.01-1.)+1000.-P1000)/9.
*
C... ZDKH or PLJT core plate quality
*
       IF (ISTGRD.EQ.3 .OR. ISTGRD.EQ.4) THEN
*
C... 50 Hz
*
          IF (FREQ.LT.55.) THEN
*
C... Step-lap, 50 Hz, ZDKH
*
             IF (BBSTLP) THEN
                NCD=0
                IF (BLIMB.GT.F(425.,605.))   NCD=1
                IF (BLIMB.GT.F(610.,855.))   NCD=2
                IF (BLIMB.GT.F(790.,1150.))  NCD=3
             ELSE
*
C... No Step-lap, 50 Hz, ZDKH
*
                NCD=0
                IF (BLIMB.GT.F(410.,585.))   NCD=1
                IF (BLIMB.GT.F(585.,825.))   NCD=2
                IF (BLIMB.GT.F(755.,1110.))  NCD=3
             END IF
*
          ELSE
*
C... Step-lap, 60 Hz,  ZDKH
*
             IF (BBSTLP) THEN
                NCD=0
                IF (BLIMB.GT.F(475.,525.))   NCD=1
                IF (BLIMB.GT.F(640.,720.))   NCD=2
                IF (BLIMB.GT.F(785.,965.))   NCD=3
             ELSE
*
C... No Step-lap, 60 Hz, ZDKH
*
                NCD=0
                IF (BLIMB.GT.F(460.,510.))   NCD=1
                IF (BLIMB.GT.F(620.,695.))   NCD=2
                IF (BLIMB.GT.F(765.,930.))   NCD=3
             END IF
          END IF
*
C... M5  core plate quality
*
       ELSE
*
C... 50 Hz
*
          IF (FREQ.LT.55.) THEN
*
C... Step-lap, 50 Hz, M5
*
             IF (BBSTLP) THEN
                NCD=0
                IF (BLIMB.GT.F(340.,510.))   NCD=1
                IF (BLIMB.GT.F(500.,725.))   NCD=2
                IF (BLIMB.GT.F(625.,955.))   NCD=3
             ELSE
*
C... No Step-lap, 50 Hz, M5
*
                NCD=0
                IF (BLIMB.GT.F(320.,475.))   NCD=1
                IF (BLIMB.GT.F(475.,700.))   NCD=2
                IF (BLIMB.GT.F(575.,920.))   NCD=3
             END IF
*
          ELSE
*
C... 60 Hz
*
C... Step-lap, 60 Hz,  M5
*
             IF (BBSTLP) THEN
                NCD=0
                IF (BLIMB.GT.F(300.,445.))   NCD=1
                IF (BLIMB.GT.F(445.,620.))   NCD=2
                IF (BLIMB.GT.F(540.,815.))   NCD=3
             ELSE
*
C... No Step-lap, 60 Hz, M5
*
                NCD=0
                IF (BLIMB.GT.F(280.,425.))   NCD=1
                IF (BLIMB.GT.F(425.,600.))   NCD=2
                IF (BLIMB.GT.F(510.,785.))   NCD=3
             END IF
          END IF
       END IF
*
       NCDTAD=NCD
*
       RETURN
       END
