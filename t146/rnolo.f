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

**********************************************************************
*
*      FUNCTION RNOLO
*      --------------
*
C... TITLE: CALCULATION OF NO LOAD LOSSES IN CORE LAMINATION
C...        BASED ON SPECIFIC LOSS DATA READ FROM AN EXTERNAL FILE.
C...        THE EXTERNAL DATA ARE READ FROM ROUTINE 'NLOSSR'
C...
C...        NOTE: THE SAME FUNCTION IS USED FOR BOTH ACTIVE AND
C...              APPARENT POWER LOSSES.
*
*
*           WRITTEN  1991-09-05  by:  B-G BLADH,        SETFO/TS
*           REVISED  ...
*
**********************************************************************
*
C
       FUNCTION RNOLO  ( TYP, BLIMB, FREKV, OKF, SBF, CORR,
     &                   GB1, GB2, GO, GH,
     &                   PSPL1,  PSPL2,  PSPL3,
     &                   PSPHD1, PSPHD2, PSPHD3,
     &                   PSPHT1, PSPHT2, PSPHT3,
     &                   PSPTY1, PSPTY2, PSPTY3 )
C
C---------------------------------------------------------------------
         REAL      PSPL1(3),   PSPL2(5),   PSPL3(3),
     &             PSPHD1(3),  PSPHD2(5),  PSPHD3(3),
     &             PSPHT1(3),  PSPHT2(5),  PSPHT3(3),
     &             PSPTY1(3),  PSPTY2(5),  PSPTY3(3)
C
         REAL      CORR
C---------------------------------------------------------------------
C      FLUX DENSITY....     < 1.3      1.3-1.8      > 1.8
C
C      LIMB YOKE           PSPL1(3),   PSPL2(5),   PSPL3(3),
C      CORNER 1 PHASE      PSPHD1(3),  PSPHD2(5),  PSPHD3(3),
C      CORNER T-CORE       PSPHT1(3),  PSPHT2(5),  PSPHT3(3),
C      CORNER TY-3 CORE    PSPTY1(3),  PSPTY2(5),  PSPTY3(3)
C
C---------------------------------------------------------------------
*
       LOGICAL SINGPH,SIDELI
C
C
 100   CONTINUE
C
       ITYP   = INT(TYP+0.1)
C
       PB1=0.
       PB2=0.
       PO=0.
       PH=0.
       write(*,*)"BLIMB-RNOLO=",BLIMB
       write(*,*)"FREKV-RNOLO=",FREKV
C
       SINGPH=(ITYP.EQ.1 .OR. ITYP.EQ.7 .OR. ITYP.EQ.8 .OR. ITYP.EQ.9)
       SIDELI=(ITYP.GE.7)
C
 300   IF(BLIMB.LE.1.8) GOTO 302
C
C*** BLIMB > 1.8 Tesla ***********************************************
C
 301        PB1 = GB1 * POLY(PSPL3,  BLIMB,    3)
            PO  = GO  * POLY(PSPL3,  BLIMB/OKF,3)
            IF(SIDELI)
     &      PB2 = GB2 * POLY(PSPL3,  BLIMB/SBF,3)
C
 500        IF(SINGPH.OR.ITYP.EQ.10) GOTO 502
 501        PH  = GH  * POLY(PSPHT3, BLIMB,    3)
            GOTO 599
 502        IF(ITYP.EQ.10) GOTO 503
            PH  = GH  * POLY(PSPHD3, BLIMB,    3)
            GOTO 599
 503        PH  = GH  * POLY(PSPTY3, BLIMB,    3)
 599        CONTINUE
             
       GOTO 399
C
 302   IF(BLIMB.LT.1.3) GOTO 303
C
C*** BLIMB 1.3 - 1.8 Tesla ********************************************
C
            PB1 = GB1 * POLY(PSPL2,  BLIMB,    5)
            PO  = GO  * POLY(PSPL2,  BLIMB/OKF,5)
            IF(SIDELI)
     &      PB2 = GB2 * POLY(PSPL2,  BLIMB/SBF,5)
C
 700        IF(SINGPH.OR.ITYP.EQ.10) GOTO 702
 701        PH  = GH  * POLY(PSPHT2, BLIMB,    5)
            GOTO 799
 702        IF(ITYP.EQ.10) GOTO 703
            PH  = GH  * POLY(PSPHD2, BLIMB,    5)
            GOTO 799
 703        PH  = GH  * POLY(PSPTY2, BLIMB,    5)
 799        CONTINUE
            write(*,*)"PB1=",PB1  
            write(*,*)"PO=",PO
            write(*,*)"PB2=",PB2
            write(*,*)"PH=",PH   
       GOTO 399
C
C*** BLIMB < 1.3  Tesla **********************************************
C
 303        PB1 = GB1 * POLY(PSPL1,  BLIMB,    3)
            PO  = GO  * POLY(PSPL1,  BLIMB/OKF,3)
C
            IF(SIDELI)
     &      PB2 = GB2 * POLY(PSPL1,  BLIMB/SBF,3)
C
 900        IF(SINGPH.OR.ITYP.EQ.10) GOTO 902
 901        PH  = GH  * POLY(PSPHT1, BLIMB,    3)
            GOTO 999
 902        IF(ITYP.EQ.10) GOTO 903
            PH  = GH  * POLY(PSPHD1, BLIMB,    3)
            GOTO 999
 903        PH  = GH  * POLY(PSPTY1,BLIMB,    3)
 999        CONTINUE
 399   CONTINUE
C***********************************************************************
C
       RNOLO = ( PB1 + PB2 + PO + PH )
C
C
C.. Adjustment due to 50 or 60 Hz net frequency
       IF(FREKV.GT.55.) RNOLO = RNOLO * CORR

C
       RETURN
       END
