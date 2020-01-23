C*..* MASS04
*******MASS04 - ACTIVE PART
       SUBROUTINE MASS04(BBPRNT,JFC,DBW,DWINDY,DUMMY,RJH,NWOLI,NCLA,
     *                   CORLAN,CORLAX,NCOR,DCORE,DWOL,BH,TWSUP,BB048)
*
*      ACTIVE PART
*      ***********
*
*      BBPRNT =   IF TRUE PRINTOUT OF INPUT/OUTPUT DATA
*      JFC        FILECODE FOR PRINTOUT
*      DBW =      FOR EY-CORES: DISTANCE BETWEEN WINDING AND SIDELIMB.
*                 FOR OTHER CORE TYPES:  DISTANCE BETWEEN PHASES   (MM)
*      DWINDY =   OUTER DIAMETER OF OUTER WINDING           (MM)
*      RJH =      ADJUSTMENT HEIGHT OF THE WINDINGS         (MM)
*      NWOLI =    NUMBER OF WOUND LIMBS
*
*
*      NCLA =     CORE CLAMP CODE
*                 1 = CORE CLAMPS OF WOOD
*                 2 = CORE CLAMPS OF STEEL PROFILE
*                 3 = CORE CLAMPS OF STEEL PLATE
*
*      CORLAN =   MASS OF CORE LAMINATION WITHOUT EXTRA VARNISH (KG)
*      CORLAX =   MASS OF CORE LAMINATION WITH    EXTRA VARNISH (KG)
*
*      NCOR =     CORE-CODE
*                 1 = D     1-PHASE WITH 2 WOUND LIMBS
*                 3 = T     3-PHASE WITH 3 WOUND LIMBS
*                 7 = EY    1-PHASE WITH 1 WOUND LIMB,  SIDELIMBS
*                 8 = DY    1-PHASE WITH 2 WOUND LIMBS, SIDELIMBS
*                 9 = TY-1  1-PHASE WITH 3 WOUND LIMBS, SIDELIMBS
*                 10= TY-3  3-PHASE WITH 3 WOUND LIMBS, SIDELIMBS
*
*      DCORE =    CORE DIAMETER      (MM)
*      DWOL =     DISTANCE FROM WINDING TO SIDE LIMB IF APPLICABLE,
*                 ELSE =0.
*      BH =       LIMBHEIGHT         (MM)
*      TWSUP =    THICKNESS OF WINDING SUPPORT   (MM)
*      BB48 =     LOGICAL VARIABLE = TRUE IF ELECTROSTATIC SHIELDS ON
*                                    REACTOR CORE LIMBS.
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
*---------------------------------------------------------------------
       LOGICAL BB048,BBPRNT
*

*---------------------------------------------------------------------
*
****   INSULATION DETAILS IN THE MAIN DUCT (GSN41) IS
*      CALCULATED IN ROUTINE 'MASS02'.
*---------------------------------------------------------------------
       IF(.NOT.BBPRNT) GO TO 101
       CALL       PMASS4(BBPRNT,JFC,DBW,DWINDY,DUMMY,RJH,NWOLI,NCLA,
     *                   CORLAN,CORLAX,NCOR,DCORE,DWOL,BH,TWSUP,BB048)
 101   CONTINUE
*
*
****   INSULATION DETAILS OUTSIDE THE OUTER WINDING
*
****   STANDARD VALUES:
*      WIDTH OF THE RIBS (MM):
           RIB = 20.
*      WITH OF DUCT INSIDE SPLIT CYLINDERS, OUTSIDE OUTER WINDING
           CANALY = DBW - 45.
           IF(CANALY.LT.0.)  CANALY = 0.
*      NUMBER OF CLACK ROWS IN OUTER WINDING
           PCLACY = DWINDY* 0.02474
*      NUMBER OF SPLIT CYLINDERS OUTSIDE OUTER WINDING:
           RCYLY  = CANALY/14.
       GSN42  = 1.22E-6*RJH*NWOLI* (RIB*PCLACY*(CANALY-RCYLY) +
     *          3.1415*(RCYLY*(DWINDY+CANALY) ) )
*---------------------------------------------------------------------
*
****   INSULATION AND SUPPORTING DETAILS AGAINST THE YOKES
       HELP= 0.68
       IF(NCLA.EQ.1) HELP= 0.38
       GSN46 = HELP* (CORLAN + CORLAX)** 0.63
*---------------------------------------------------------------------
****   INSULATION OF SIDE LIMBS
       GSN45=0.
       IF(NCOR.GT.3) GSN45=2.E-7*(DCORE+400.)*DWOL*BH
*---------------------------------------------------------------------
****   WINDING SUPPORTS
       GSN47= 237.E-7*NWOLI*TWSUP*(DWINDY-DCORE)**2
*---------------------------------------------------------------------
****   ELECTROSTATIC SHIELDING OF REACOR CORE LIMBS
       IF(BB048) GSN48 = 1.9E-5 *DCORE*BH*NWOLI
*---------------------------------------------------------------------
****   NET MASS OF GOODS FOR ACTIVE PART
       GG49=  0.047*(CORLAN + CORLAX)** 0.7
*
       RETURN
       END
