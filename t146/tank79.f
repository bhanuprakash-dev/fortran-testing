C*..* TANK79
CTANK79       *******TANK79 - TANK MASS
       SUBROUTINE TANK79(BBPRNT,JFC,HTANK,BTANK,RLTANK,VVAH,RRAD,VTANK,
     *                   ASHELL,ACOVER,KTAN79,TANKDI,GTANK,BBLT10,
     *                   BBEXAC)
*
*      TANK MASS
*      ***********
*
*      BBPRNT   = PRINTOUT OF INPUT/OUTPUT DATA IF TRUE
*      JFC      = FILECODE FOR PRINTOUT
*      HTANK    = TANKHEIGHT  (M)
*      BTANK    = TANKWIDTH   (M)
*      RLTANK   = TANKLENGTH  (M)
*
*      VVAH     = NUMBER OF HEAT EXCHANGERS (VVAH)
*      RRAD     = NUMBER OF RADIATORS
*      VTANK    = TANKVOLUME (M3)
*      ASHELL   = AREA OF TANK WALLS (M2)
*      ACOVER   = COVER AREA (M2)
*      KTAN79   = CODE FOR TANK TYPE
*                 1 = OVAL TANK
*                 2 = RECTANGULAR TANK
*                 3 = TAA - VACUUM-PROOF
*                 4 = TAA - NOT VACUUM-PROOF
*                 5 = TBA
*                 6 = CURVED TANK  AND OTHER TYPES
*
*      TANKDI   = OUTPUT DATA FOR COST CALCULATION
*      GTANK    = MASS OF THE TANK (KG)
*      BBLT10   = LOGICAL VARIABLE = TRUE IF GTANK IS LESS THAN 10000.
*                 BBLT10 MUST BE SAVED IN THE MAIN LINK
*      BBEXAC   = LOGICAL VARIABLE = TRUE   DURING EXACT CALCULATIONS
*                                    AND  FALSE    DURING OPTIMIZING .
*
       LOGICAL BBLT10,BBEXAC,BBPRNT
*
*....................................................................
*      FIXED VALUES ARE SAVED DURING OPTIMIZING IN THE MAIN LINK:
*      IF ROUTINE TANK79 IS LINKED OUT COMMON BLOCK /STANK7/ MUST
*      BE SAVED IN THE MAIN LINK
       COMMON /STANK7/ XTANKS,YTANKS,ZTANKS,TTANKS
*....................................................................
       IF(.NOT.BBPRNT) GO TO 101
       CALL       PTNK79(BBPRNT,JFC,HTANK,BTANK,RLTANK,VVAH,RRAD,VTANK,
     *                   ASHELL,ACOVER,KTAN79,TANKDI,GTANK,BBLT10,
     *                   BBEXAC)
 101   CONTINUE
*
       IF(KTAN79.GT.2) BBLT10 = .FALSE.
*
       GO TO (100,200,300,400,500,600), KTAN79
*
*...   OVAL TANK ......................................................
 100   CONTINUE
       RM    =  200. * HTANK*(BTANK+RLTANK)
       GTANK =  4.11 * RM** 0.914
       TANKDI=  VTANK
       IF(BBEXAC)  BBLT10= (GTANK.LT.1.E4)
       IF(BBLT10)  TANKDI = RM*1.E-2
       IF(BBLT10)  GTANK  = 1.67*(HTANK*BTANK*RLTANK*1.E3) **0.816
       GO TO  999
*
*...   REKTANGULAR TANK ...............................................
 200   CONTINUE
       RM = 2.E2*HTANK*(BTANK+RLTANK)
       GTANK= 1.462E-4* RM** 2.054
       IF(BBEXAC)  BBLT10 = (GTANK.LT.1.E4)
       TANKDI =  2.*(BTANK+RLTANK)
       IF(BBLT10) GTANK= 3.41* RM**0.907
       GO TO 999
*
*...   TAA-TANK  -  VACUUM-PROOF.......................................
 300   CONTINUE
       IF(.NOT.BBEXAC) GO TO 310
       XTANKS = 0.05
       IF(ASHELL.GT.14.) XTANKS= 0.06
       IF(ASHELL.GT.18.) XTANKS= 0.07
       YTANKS = 0.10
       IF(ASHELL.GT.20.) YTANKS= 0.12
       ZTANKS  = 0.12
       IF(VTANK.GT.6.4)  ZTANKS = 0.16
       IF(VTANK.GT.12.5) ZTANKS = 0.20
 310   GTANK = 8.E+2*(ASHELL*(0.9*XTANKS+0.45*YTANKS ) +
     *                ACOVER*(1.42*ZTANKS + 0.0492))
     *              + 22.6*RRAD + 125.4*VVAH +92.
       TANKDI= ASHELL
       GO TO 999
*
*...   TAA-TANK  -  NOT VACUUM-PROOF ..................................
 400   CONTINUE
       IF(.NOT.BBEXAC) GO TO 410
       XTANKS = 0.05
       IF(ASHELL.GT.28.) XTANKS= 0.06
       YTANKS = 0.08
       IF(ASHELL.GT.28.) YTANKS= 0.10
       ZTANKS  = 0.12
       IF(VTANK.GT.6.6)  ZTANKS = 0.16
       IF(VTANK.GT.11.5) ZTANKS = 0.20
 410   GTANK = 8.E+2*(ASHELL*(0.9*XTANKS+0.45*YTANKS)
     *              + ACOVER*(1.42*ZTANKS + 0.0492))
     *              + 22.6*RRAD + 125.4*VVAH +92.
       TANKDI= ASHELL
       GO TO 999
*
*...   TBA-TANK .......................................................
 500   CONTINUE
       IF(.NOT.BBEXAC) GO TO 510
       TTANKS  = 0.225
       IF(VTANK.GT.19.) TTANKS = 0.275
 510   GTANK=(ACOVER*(0.44+10.4*TTANKS )+ 0.25*ASHELL)*100. +
     *        94.4*VTANK + 100.*HTANK + 10.4
       TANKDI = 2.*(BTANK+RLTANK)
       GO TO 999
*
*...   CURVED TANK AND OTHER TYPES ....................................
 600   CONTINUE
       GTANK=  0.136*(ASHELL*100.)** 1.28
       TANKDI = VTANK
*
 999   RETURN
       END
