C*..* MASS06
CMASS06  *******MASS06 - CONSERVATOR,PIPES,CONTROL CABLING
       SUBROUTINE MASS06(BBPRNT,JFC,BB08,KCOOL1,PLOSS,KCOOL2,GFAN)
*
*      CONSERVATOR, PIPES, CONTROL CABLING
*      ***********************************
*
*      BBPRNT     PRINTOUT OF INPUT/OUTPUT IF TRUE
*      JFC        FILECODE FOR PRINTOUT
*      BB08 =     LOGICAL VARIABLE = TRUE IF THE CONSERVATOR IS
*                                    MOUNTED ON THE TANK
*      KCOOL1 =   CODE FOR COOLING EQUIPMENT
*                 1 = BUILT-ON RADIATORS
*                 2 = BUILT-ON RADIATOR BATTERIES
*                 3 = SEPARATE RADIATOR BATTERIES
*                 4 = BUILT-ON HEAT EXCHANGERS (OFAF)
*                 5 = SEPARATE HEAT EXCHANGERS (OFAF)
*                 6 = BUILT-ON HEAT EXCHANGERS (OFWF)
*
*      PLOSS =    MAX TRANSFORMER LOSSES AT ANY TAP  (W)
*
*      KCOOL2 =   CODE FOR COOLING EQUIPMENT CONTROL CABLING
*                 1 = SELF-COOLED TRANSFORMER OR NO COOLING EQUIPMENT.
*                 2 = ONAF - COOLED TRANSFORMERS
*                 3 = OFAF OR OFWF - COOLED TRANSFORMERS
*
*       GFAN =     FAN MASS ESTIMATED IN THE TECHNICAL ROUTINES (KG)
*
*
*---------------------------------------------------------------------
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
*
       LOGICAL BB08,BBPRNT
*

*---------------------------------------------------------------------
       IF(.NOT.BBPRNT) GO TO 101
       CALL       PMASS6(BBPRNT,JFC,BB08,KCOOL1,PLOSS,KCOOL2,GFAN)
 101   CONTINUE
*
****   CONSERVATOR
****   BB08  = .TRUE. IF THE CONSERVATOR IS MOUNTED ON THE TRANSFORMER
       IF(BB08)  GSN81 = 0.123 * GSN142**0.9
       IF(.NOT.BB08)  GSN81 = 0.07 * GSN142
       GG81  = 0.05 * GSN81
*
****   GOODS, CONSERVATOR ASSEMBLY
       GG83 = 0.01 * GSN81
*
****   PIPES
****   PIPES  TO THE CONSERVATOR
       GSN91  = 1.1E-5 * GSN142**1.5
       GG91  = 0.1 *GSN91

*---------------------------------------------------------------------
****   PIPES TO THE COOLING EQUIPMENT
       IF(KCOOL1.GT.0)
     * GOTO (1101,1102,1103,1104,1105,1106), KCOOL1
*
 1101  CONTINUE
****   BUILT-ON RADIATORS
       GSN92 = 0.
       GOTO 1199
*
 1102  CONTINUE
****   BUILT-ON RADIATOR BATTERIES
       GSN92 =  3.01E-5 *PLOSS**1.2
       GOTO 1199
*
 1103  CONTINUE
****   SEPARATE RADIATOR BATTERIES
       GSN92 = 0.146  *PLOSS**0.7
       GOTO 1199
*
 1104  CONTINUE
 1106  CONTINUE
****   BUILT-ON HEAT EXCHANGERS (OFAF,OFWF)
       GSN92 = 3.01E-5 *PLOSS**1.2
       GOTO 1199
*
 1105  CONTINUE
       GSN92 = 0.088 *PLOSS**0.7
*
 1199  CONTINUE
****   COMMON PART - PIPE CONNECTIONS TO THE COOLING EQUIPMENT
       GG92 = 0.1 *GSN92
*---------------------------------------------------------------------
****   MASS OF FANS
       GG101= GFAN
*
****   GOODS, COOLING EQUIPMENT ASSEMBLY
       GG94= 0.01* (GSN92 + GSN91)
*---------------------------------------------------------------------
****   CONTROL CABLING
       IF(KCOOL2.GT.0)
     * GOTO (1001,1002,1003), KCOOL2
       GSN102  = 0.
       GOTO 1099
 1001  CONTINUE
*
****   SELF-COOLED TRANSFORMERS
       GSN102  = 9.13E-3 *PLOSS**0.7
       GOTO  1099
 1002  CONTINUE
 1003  CONTINUE
*
****   ONAF, OFAF, OFWF - COOLING
       GSN102  = 0.0135  *PLOSS**0.7
*
 1099  CONTINUE
*
****   GOODS, CONTROL CABLING ASSEMBLY
       GG102= 0.30 *GSN102
*
       RETURN
       END
