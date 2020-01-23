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
*
************************************************************************
*
*      Function NCDTBA
*      ---------------
*
C...   Title:  Calculate number of cooling ducts in TBA cores.
*
C...   Technical reference:  Report K 89-63  Kurt Gramm  1989-08-21
*
*      Written: 89-10-02 by B-G Bladh     , SETFO/K1
*      Revised: 90-02-12 by Per Sãderberg , SETFO/IK
*      Revised: 92-04-09 by B-G Bladh     , SETFO/TS;
*               BBZDKH and BBPLJT replaced by ISTGRD.
*               ISTGRD = 1 :  M5   Core lamination quality
*               ISTGRD = 2 :  HI-B Core lamination quality
*               ISTGRD = 3 :  ZDKH Core lamination quality
*               ISTGRD = 4 :  PLJT Core lamination quality
*
*
************************************************************************
*
       INTEGER FUNCTION NCDTBA(DCOR,TOPOIL,BLIMB,FREQ,
     &                         BBSTLP,ISTGRD)
*
C...   DCOR    Core Diameter  (mm)
C...   TOPOIL  Top oil temperature
C...   BLIMB   Maximum flux density (T)
C...   FREQ    frequency  (HZ)
C...   BBSTLP  step-lap core   (.T.  OR .N.)
*
       REAL     DCOR,TOPOIL,BLIMB,FREQ,F,E,PDK,P1000,X1,X2
*
       INTEGER  NCD, ISTGRD
*
       LOGICAL  BBSTLP
*
C...  Evaluation of straight line between
C...  BLIMB = 1,5 T and BLIMB = 1,95 T
C...  end point values are X1, AND X2.
*
       F(X1,X2) =  1.95 - 0.45*(PDK-X1)/(X2-X1)
*
C...  Top oil parameter PDK.
C...  A new fictive diameter is calculated wich
C...  is less than the original one, but with 90 degrees top oil.
C...  (which will give the same number of cooling ducts)
*
       E=0.1*(TOPOIL-50.)
       P1000=(2.**E-1.)*25.+625.
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
C... STEP-LAP, 50 Hz, ZDKH
*
             IF (BBSTLP) THEN
                NCD=0
                IF (BLIMB.GT.F(495.,725.))   NCD=1
                IF (BLIMB.GT.F(655.,935.))   NCD=2
                IF (BLIMB.GT.F(815.,1165.))  NCD=3
                IF (BLIMB.GT.F(980.,1415.))  NCD=4
             ELSE
*
C... NO STEPLAP, 50 Hz, ZDKH
*
                NCD=0
                IF (BLIMB.GT.F(475.,700.))   NCD=1
                IF (BLIMB.GT.F(625.,905.))   NCD=2
                IF (BLIMB.GT.F(785.,1135.))  NCD=3
                IF (BLIMB.GT.F(940.,1380.))  NCD=4
             END IF
*
          ELSE
*
C... 60 Hz
*
C... STEP-LAP, 60 Hz,  ZDKH
*
             IF (BBSTLP) THEN
                NCD=0
                IF (BLIMB.GT.F(430.,635.))   NCD=1
                IF (BLIMB.GT.F(570.,820.))   NCD=2
                IF (BLIMB.GT.F(705.,1015.))  NCD=3
                IF (BLIMB.GT.F(840.,1235.))  NCD=4
                IF (BLIMB.GT.F(955.,1445.))  NCD=5
             ELSE
*
C... NO STEPLAP, 60 Hz, ZDKH
*
                NCD=0
                IF (BLIMB.GT.F(410.,610.))   NCD=1
                IF (BLIMB.GT.F(540.,795.))   NCD=2
                IF (BLIMB.GT.F(675.,985.))   NCD=3
                IF (BLIMB.GT.F(805.,1190.))  NCD=4
                IF (BLIMB.GT.F(915.,1400.))  NCD=5
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
C... STEP-LAP, 50 Hz, M5
*
             IF (BBSTLP) THEN
                NCD=0
                IF (BLIMB.GT.F(375.,600.))   NCD=1
                IF (BLIMB.GT.F(510.,790.))   NCD=2
                IF (BLIMB.GT.F(630.,980.))   NCD=3
                IF (BLIMB.GT.F(750.,1190.))  NCD=4
                IF (BLIMB.GT.F(845.,1385.))  NCD=5
             ELSE
*
C... NO STEPLAP, 50 Hz, M5
*
                NCD=0
                IF (BLIMB.GT.F(350.,580.))   NCD=1
                IF (BLIMB.GT.F(475.,760.))   NCD=2
                IF (BLIMB.GT.F(585.,950.))   NCD=3
                IF (BLIMB.GT.F(700.,1150.))  NCD=4
                IF (BLIMB.GT.F(785.,1335.))  NCD=5
             END IF
          ELSE
*
C... 60 Hz
*
C... STEP-LAP, 60 Hz,  M5
*
             IF (BBSTLP) THEN
                NCD=0
                IF (BLIMB.GT.F(315.,525.))   NCD=1
                IF (BLIMB.GT.F(430.,690.))   NCD=2
                IF (BLIMB.GT.F(540.,860.))   NCD=3
                IF (BLIMB.GT.F(630.,1030.))  NCD=4
                IF (BLIMB.GT.F(695.,1200.))  NCD=5
             ELSE
*
C... NO STEPLAP, 60 Hz, M5
*
                NCD=0
                IF (BLIMB.GT.F(295.,505.))   NCD=1
                IF (BLIMB.GT.F(410.,665.))   NCD=2
                IF (BLIMB.GT.F(510.,825.))   NCD=3
                IF (BLIMB.GT.F(595.,995.))   NCD=4
                IF (BLIMB.GT.F(655.,1150.))  NCD=5
             END IF
          END IF
       END IF
*
       NCDTBA=NCD
*
       RETURN
       END
