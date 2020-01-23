C*..* MASS03
CMASS03   *******MASS03 - CLEATS AND LEADS
       SUBROUTINE  MASS03(BBPRNT,JFC,J,KCON,UPHAS,RIPHAS,BH,NWOLI,
     *                    NCON,RLCON,BOOST)
*
*      CLEATS AND LEADS
*      ****************
*      BBPRNT    IF TRUE PRINTOUT OF INPUT/OUTPUT DATA
*      JFC       FILECODE FOR PRINTOUT
*      J =       TERMINAL NUMBER (1,2,3 OR 4)
*      KCON =    CODE FOR CLEATS AND LEADS
*                1 = 1-PHASE  1 WOUND LIMB
*                2 = 1-PHASE  2 WOUND LIMBS
*                3 = 1-PHASE  3 WOUND LIMBS
*                4 = 1-PHASE  4 WOUND LIMBS
*                5 = 3-PHASE  Y CONNECTED
*                6 = 3-PHASE  Y/AUTO  CONNECTED
*                7 = 3-PHASE  Z CONNECTED
*                8 = 3-PHASE  D CONNECTED
*      UPHAS =   PHASE VOLTAGE (=MAIN VOLTAGE/SQRT(3) )      (V)
*      RIPHAS =  MAX WINDING CURRENT                         (A)
*      BH    =   LIMB HEIGHT                                 (MM)
*      NWOLI =   NUMBER OF WOUND LIMBS
*      NCON =    NUMBER OF REGULATING LOOPS.
*                (=0 FOR NOT REGULATED TERMINALS)
*      RLCON =   MEAN DISTANCE FROM REG. WINDING TO TAP CHANGER  (MM)
*      BOOST =   BOOSTER FACTOR. THE CURRENT IN THE REG.WINDING IS
*                REDUCED WITH THE BOOST.FACTOR.(NORMALLY  BOOST = 1)
*-------------------------------------------------------------------
*      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
*
       COMMON /V79/
     * GSN11(9),    GSN23,       GSN25,        GSN31(4),    GSN32(4),
     * GSN41(9),    GSN42,       GSN45,        GSN46,
     * GSN47,       GSN48,
     * GSN51,       GSN52,       GSN53,        GSN054,
     * GSN61,       GSN71,       GSN72,        GSN81,
     * GSN91,       GSN92,       GSN102,       GSN111,      GSN112,
     * GSN121(9),   GSN122(9),   GSN131(9),    GSN132(9),
     * GSN133(9),   GSN134(9),   GN1351(9),    GN1352(9),
     * GSN136(9),
     * GSN141,      GSN142,      GSN143,       GSN211,      GSN212,
     * GSN221,      GSN222,      GSN241,       GSN242,      GN1210
       COMMON /VG79/
     * GG21,        GG22,        GG23,         GG24,        GG32(4),
     * GG49,        GG51,        GG52,         GG53,        GG054,
     * GG54,        GG55,        GG61,         GG63,        GG71,
     * GG72,        GG74,        GG81,         GG83,
     * GG91,        GG92,        GG94,         GG101,       GG102,
     * GG111,       GG1210 ,     GG151,        GG152
*-------------------------------------------------------------------
       DIMENSION  X31(8)
*
       LOGICAL BBPRNT
*
       DATA X31/82.E-7,123.E-7,164.E-7,205.E-7,
     *          205.E-7,164.E-7,246.E-7,271.E-7/
*

*-------------------------------------------------------------------
 100   IF(.NOT.BBPRNT) GO TO 101
       CALL        PMASS3(BBPRNT,JFC,J,KCON,UPHAS,RIPHAS,BH,NWOLI,
     *                    NCON,RLCON,BOOST)
 101   CONTINUE
       IF(BOOST.LE.0.)  BOOST = 1.
*
****   CONDUCTORS  IN CLEATS AND LEADS
       GSN31(J) = X31(KCON)*RIPHAS*BH*(1+0.0056*UPHAS**0.3)
     *          + 10.E-8*NWOLI*NCON**1.2*RLCON*(RIPHAS/BOOST)**1.485
     *           *(1+0.0056*UPHAS**0.3)
*
*
****   MOUNTING, SUPPORTING AND INSULATION DETAILS
       GSN32(J) = 5.3 * GSN31(J)**0.7
       GG32(J) = 0.05 *(GSN31(J) +GSN32(J) )
*
       RETURN
       END
