C*..* MASS07
CMASS07    *******MASS07 - OIL ABSORBED,VOLUME ACTIVE PART,FINAL ASSEMBLY ET
       SUBROUTINE MASS07(BBPRNT,JFC,NCLA,NWILI,NG,VTANK,RLTANK,BTANK,
     *                   HTANK,BB132,RAACON,VACTP,GVVAH,GOFWF)
*
*      OIL ABSORBED , VOLUME OF ACTIVE PART, FINAL ASSEMBLY DETAILS,
*      SOUND SCREENS, HEAT EXCHANGERS.
*      *********************************************************
*
*      BBPRNT     PRINTOUT OF INPUT/OUTPUT DATA IF TRUE
*      JFC        FILECODE FOR PRINTOUT
*      NCLA =     CORE CLAMP CODE
*                 1 = WOOD          CORE CLAMPS
*                 2 = STEEL PROFILE CORE CLAMPS
*                 3 = STEEL PLATE   CORE CLAMPS
*
*      NWILI =    NUMBER OF WINDINGS PER LIMB
*      NG =       NUMBER OF TERMINALS ON THE TRANSFORMER
*      VTANK =    VOLUME OF TRANSFORMER TANK                (DM3)
*      RLTANK =   TANK LENGTH              (MM)
*      BTANK =    TANK WIDTH               (MM)
*      HTANK =    TANK HEIGHT              (MM)
*      BB132 =    LOGICAL VARIABLE = TRUE IF SOUND SCREENS
*                 SHALL BE INCLUDED.
*      RAACON =   DENSITY OF WINDING CONDUCTORS       (KG/DM3)
*      VACTP =    OUTPUT DATA : VOLUME OF ACTIVE PART (DM3)
*      GVVAH =    MASS OF HEAT EXCHANGERS OFAF  (ESTIMATED)
*      GOFWF =    MASS OF HEAT EXCHANGERS OFWF  (ESTIMATED)
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
       DIMENSION RAACON(9)
*
       LOGICAL BB132,BBPRNT
*

*---------------------------------------------------------------------
 100   IF(.NOT.BBPRNT) GO TO 101
       CALL       PMASS7(BBPRNT,JFC,NCLA,NWILI,NG,VTANK,RLTANK,BTANK,
     *                   HTANK,BB132,RAACON,VACTP,GVVAH,GOFWF)
 101   CONTINUE
*
****   NET MASS OF OIL ABSORBED
*
*      AUXILIARY VARIABLE FOR CORE CLAMPS
       RK10= 0.09*(GSN241+GSN242)
       IF(NCLA.GE.2) RK10=0.021 *(GSN241+GSN242)**0.825
*
*      OIL IN THE WINDINGS:
       SHELP1=0.
       DO 10  I=1,NWILI
       SHELP1=SHELP1 + 0.15*(GSN131(I)+GSN132(I)+GSN133(I)+
     *        GSN134(I)+GN1351(I)+GN1352(I) ) +
     *           0.33*(GSN121(I)+GSN122(I))  + 0.3*GSN41(I)
 10    CONTINUE
*
*      OIL IN CLEATS AND LEADS
       SHELP2=0.
       DO 15 I=1,NG
       SHELP2=SHELP2 + 0.1*GSN31(I) + 0.3*GSN32(I)
 15    CONTINUE
*
       GSN141 = SHELP1 + SHELP2 +
     *          0.15*GSN46 + 0.3*(GSN42+GSN45+GSN212+RK10) +
     *          0.02*VTANK
*---------------------------------------------------------------------
*
****   VOLUME OF ACTIVE PART
*
       SHELP1=0.
       DO 20 I=1,NWILI
       SHELP1=SHELP1 + 0.9*(GSN121(I)+GSN122(I)) +0.95*GSN41(I) +
     *        0.8*(GSN131(I)+GSN132(I)+GSN133(I)+
     *        GSN134(I)+GN1351(I)+GN1352(I)+GSN136(I)) +
     *        GSN11(I)/RAACON(I)
 20    CONTINUE
*
       SHELP2=0.
       DO 25 I=1,NG
       SHELP2=SHELP2+ 0.9*GSN32(I) + 0.2*GSN31(I)
 25    CONTINUE
*
       write(*,*) "GSN121",GSN121
       write(*,*) "GSN122",GSN122
       write(*,*) "GSN41",GSN41
       write(*,*) "GSN131",GSN131
       write(*,*) "GSN132",GSN132
       write(*,*) "GSN133",GSN133
       write(*,*) "GSN134",GSN134
       write(*,*) "GSN1351",GSN1351
       write(*,*) "GSN1352",GSN1352
       write(*,*) "GSN136",GSN136
       write(*,*) "GSN11",GSN11
       write(*,*) "RAACON",RAACON
       write(*,*) "SHELP1",SHELP1
       write(*,*) "SHELP2",SHELP2
       VACTP = SHELP1 + SHELP2 +
     *         0.15*GSN211 + 0.95*(GSN212+GSN42+GSN45)  +
     *         1.04*GSN25 + 0.2*GG49 + 0.7*GSN46 + 0.35*GSN53 +
     *         0.13*(GSN241 +GSN242 +GSN47)
*---------------------------------------------------------------------
*
****   DETAILS, FINAL ASSEMBLY
*
****   NORMAL FINAL ASSEMBLY DETAILS
       GN1210= 0.
       GG1210= 0.15 *(GSN241+GSN242)**0.75
       GSN142=0.
       write(*,*) "VTANK=",VTANK
       write(*,*) "VACTP=",VACTP
       IF(VTANK.GT.0.)
     * GSN142 = 0.96 *(0.98*VTANK -VACTP)
*
****   SOUND SCREENS
       GSN112  = 0.
       IF(BB132)  GSN112 =30.E-6 *(RLTANK+BTANK+1300)*(HTANK-200)
       GSN111  = 3.5 *GSN112
       GG111   = 0.05 *(GSN111 + GSN112)
*
****   HEAT EXCHANGERS
*
       GG151 = GVVAH
       GG152 = GOFWF
*
       RETURN
       END
