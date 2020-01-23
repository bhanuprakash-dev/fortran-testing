C*..* MASS01
CMASS01   *******MASS01 - CORE DETAILS
       SUBROUTINE MASS01(BBPRNT,JFC,NCOR,NLAM,NCLA,BH,DCORE,OH,OL,
     *                   COCAN,CORLAN,CORLAX,GASEC )
*
*      CORE
*      ****
*      BBPRNT    IF TRUE PRINTOUT OF INPUT/OUTPUT DATA
*      JFC       FILECODE FOR PRINTOUT
*      NCOR =    CORE-CODE
*                1 = D     1-PHASE WITH 2 WOUND LIMBS
*                3 = T     3-PHASE WITH 3 WOUND LIMBS
*                7 = EY    1-PHASE WITH 1 WOUND LIMB,  SIDELIMBS
*                8 = DY    1-PHASE WITH 2 WOUND LIMBS, SIDELIMBS
*                9 = TY-1  1-PHASE WITH 3 WOUND LIMBS, SIDELIMBS
*                10= TY-3  3-PHASE WITH 3 WOUND LIMBS, SIDELIMBS
*
*      NLAM =    CORE LAMINATION QUALITY CODE:  1= M5,  2= HI-B
*
*      NCLA =    CORE CLAMP CODE
*                1 = CORE CLAMPS OF WOOD
*                2 = CORE CLAMPS OF STEEL PROFILE
*                3 = CORE CLAMPS OF STEEL PLATE
*
*      BH =      LIMB HEIGHT                   (MM)
*      DCORE =   CORE DIAMETER                 (MM)
*      OH =      YOKE HEIGHT                   (MM)
*      OL =      YOKE LENGTH                   (MM)
*      COCAN =   NUMBER OF COOLING DUCTS IN THE CORE
*      CORLAN =  MASS OF CORE LAMINATION WITHOUT EXTRA VARNISH (KG)
*      CORLAX =  MASS OF CORE LAMINATION WITH    EXTRA VARNISH (KG)
*                CORLAN AND CORLAX IS CALCULATED IN THE TECHNICAL
*                CALCULATIONS
*      GASEC =   MASS OF ASECOND TAPE FOR LIMB BANDAGE CALCULATED
*                IN THE TECHNICAL ROUTINES
*
*--------------------------------------------------------------
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
       LOGICAL BBPRNT
*

*
*--------------------------------------------------------------
       IF(.NOT.BBPRNT) GO TO 101
       CALL       PMASS1(BBPRNT,JFC,NCOR,NLAM,NCLA,BH,DCORE,OH,OL,
     *                   COCAN,CORLAN,CORLAX,GASEC )
 101   CONTINUE
*
****   STANDARD VALUE: 0.666 KG STEATITE SPACERS / M2.
       STEK = 0.666
*
****   CORE LAMINATION
       GSN241=CORLAN
       GSN242=CORLAX
*
*
****   NET MASS FOR CORE CLAMPS, DETAILS OF STEEL  (GSN211),
*      IS CALCULATED SEPARATELY IN ROUTINE "PBALK"
*
****   NET MASS CORE CLAMPS, DETAILS OF NON-STEEL
       WRITE(*,*) 'MASS01 GSN241 = ', GSN241
       WRITE(*,*) 'MASS01 GSN242 = ', GSN242
       GSN212= 0.021*(GSN241+GSN242)**0.825
       WRITE(*,*) 'MASS01 GSN212 = ', GSN212
*      
****   NET MASS CORE CLAMP GOODS.
       GG21 = 0.1* GSN211
*
****   NET MASS STEATITE SPACERS  (FOR COOLING DUCTS IN THE CORE)
                                  IHLP= 2
       IF(NCOR.EQ.3.OR.NCOR.EQ.8) IHLP=3
       IF(NCOR.GE.9)              IHLP=4
       GG24    = STEK*1.E-6 *COCAN* (IHLP *BH *DCORE +2.*OH*OL )
*
****   MASS OF ASECOND TAPE
       GSN25= GASEC
*
*
       RETURN
       END
