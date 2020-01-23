ccC*..* MASS05
ccCMASS05  *******MASS05 - TANK,COVER,COOLING EQUIPMENT,SOUND SCREENS
       SUBROUTINE MASS05(BBPRNT,JFC,BB51,GTANK,VTANK,ACOV,ATAWA,BB07,
     &                   GCOVER,KCOOL1,PLOSS,AFIELD,GRAD,OTANK,KTAN79)
*
*      TANK, COVER, COOLING EQUIPMENT, SOUND SCREENS
*      *********************************************
*
*      BBPRNT     TRUE IF PRINTOUT OF INPUT/OUTPUT DATA
*      JFC        FILECODE FOR PRINTOUT
*      BB51 =     SPARE
*      GTANK =    NET MASS TRANSFORMER TANK, CALCULATED IN THE
*                 TECHNICAL ROUTINES       (KG)
*      VTANK =                             (DM3)
*      ACOV =     COVER AREA               (M2)
*      ATAWA =    AREA OF THE TANK SIDES   (M2)
*      BB07 =     LOGICAL VARIABLE = TRUE IF THE COVER MASS IS
*                 ALREADY CALULATED (=GCOVER)
*      GCOVER =   COVER MASS
*
*      KCOOL1 =   CODE FOR THE COOLING EQUIPMENT
*                 0 = NO COOLING EQUIPMENT
*                 1 = BUILT-ON RADIATORS
*                 2 = BUILT-ON RADIATOR BATTERIES
*                 3 = SEPARATE RADIATOR BATTERIES
*                 4 = BUILT-ON HEAT EXCHANGERS (OFAF)
*                 5 = SEPARATE HEAT EXCHANGERS (OFAF)
*                 6 = BUILT-ON HEAT EXCHANGERS (OFWF)
*
*      PLOSS =    MAX TRANSFORMER LOSSES AT ANY TAP         (W)
*      AFIELD =   AREA OF FIELD SHIELDS AGAINST TANK WALL   (M2)
*      GRAD =     MASS OF RADIATORS (FROM TECHNICAL ROUTINES)  (KG)
*      OTANK =    CIRCUMFERENCE OF THE TANK                 (MM)
*
*      KTAN79 =   TANK CODE
*                 1 = OVAL TANK         TMY
*                 2 = RECTANGULAR TANK  TMY
*                 3 = TAA, VACUUM-PROOF
*                 4 = TAA, NOT VAKUUM-PROOF
*                 5 = TBA
*                 6 = OTHER TYPES , TMY
*
*------------------------------------------------------------------
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
*------------------------------------------------------------------
       DIMENSION CA(6),CB(6)
*
       LOGICAL BB51,BB07,BBPRNT
*
       DATA CA/172.6, 121.4, 160.9, 59.71, 110.85, 96.99 / ,
     *      CB/1.040, 1.148, 1.172, 1.394, 1.200, 1.230 /
*

*------------------------------------------------------------------
       IF(.NOT.BBPRNT) GO TO 101
       CALL       PMASS5(BBPRNT,JFC,BB51,GTANK,VTANK,ACOV,ATAWA,BB07,
     *                   GCOVER,KCOOL1,PLOSS,AFIELD,GRAD,OTANK,KTAN79)
 101   CONTINUE
*
****   TANK MANUFACURED
       GSN51 = GTANK
       GG51 = 0.01 *GSN51
*
****   TANK OTHERS
       GSN52 = 0.04 *GSN51
       GG52 = 0.02 *GSN51
*
****   RADIATORS
       GG54 =GRAD
*
****   FIELD SHIELDS AGAINST TANK WALL
       GSN53 =  9.0 *AFIELD
       GG53 = 0.
*
****   DETAILS FOR SOUND SCREEN ASSEMBLY
       GSN054=0.012*OTANK
       GG054=0.05*GSN054
*
****   GOODS, TANK ASSEMBLY
       GG55= 0.01*GSN51
*
****   COOLING EQUIPMENT
       GSN61 = 0.
       GG61 = 0.
*------------------------------------------------------------------
       IF (KCOOL1.LE.0) GO TO 6099
       GO TO (6099,6002,6003,6004,6005,6006),KCOOL1
*
 6002  CONTINUE
****   BUILT-ON RADIATORS
       GSN61=0.0289*PLOSS**0.9
       GG61  = 0.02 *GSN61
       GO TO  6099
*
 6003  CONTINUE
****   SEPARATE RADIATOR BATTERIES
       GSN61=0.0323*PLOSS**0.9
       GG61  = 0.02 *GSN61
       GO TO 6099
*
 6006  CONTINUE
 6004  CONTINUE
****   BUILT-ON HEAT EXCHANGERS (OFAF, OFWF)
       GSN61=0.0036*PLOSS**0.9
       GG61  = 0.02 *GSN61
       GO TO 6099
*
 6005  CONTINUE
****   SEPARATE HEAT EXCHANGERS
       GSN61=0.012*PLOSS**0.9
       GG61  = 0.02 *GSN61
*
 6099  CONTINUE
*------------------------------------------------------------------
****   GOODS, COOLING EQUIPMENT ASSEMBLY
       GG63= 0.01*GSN51
*
****   COVER
       IF(BB07) GSN71 = GCOVER
       IF(.NOT.BB07) GSN71  = CA(KTAN79) * ACOV**CB(KTAN79)
       GG71  = 0.02 *GSN71
*
****   MANUFACORING DETAILS EXCLUDING HEAVY PART
       GSN72= 0.04 * GSN71
       GG72 = 0.02 * GSN72
*
****   GOODS, COVER ASSEMBLY
       GG74 = 0.01 * GSN72
*
       RETURN
       END
