C*..* MASS02
cCMASS02   *******MASS02 - CALCULATE MASS OF WINDING PROPERTIES
       SUBROUTINE MASS02(BBPRNT,JFC,I,DCORE,HLIMB,RLH,RJH,KWITYP,PLIMB,
     *                   ACOND,RAACON,NCOND,NWOLI,HPART,BPART,RPRTAR,
     *                   RPRTRR,
     *                   TCOV1,TCOV2,ZLOOP,ZWIND,DWIND,WWIND,DUCTI,ISAM,
     *                   TCLT,RGUID,RNCOOL,PCSNED,HSPOL,HCLAC,ZLAG,
     *                   ZCOIAR,NGROUP,IPISOL)
*
*      LIST OF OUTPUT GSN-VALUES
*      GSN11,GSN41,GSN121,GSN122,GSN131,GSN132,GSN133,GSN134,GSN136,
*      GN1351,GN1352
*
*      WINDINGS
*      ********
*      BBPRNT =    .TRUE. IF PRINTOUT OF INPUT/OUTPUT IS REQUESTED
*      JFC    =    FILECODE FOR PRINTOUT
*      I      =    WINDING NUMBER
*      DCORE  =    CORE DIAMETER            (MM)
*      HLIMB  =    LIMB HEIGHT              (MM)
*      RLH    =    WINDING HEIGHT           (MM)
*      RJH    =    ADJUSTMENT HEIGHT OF THE WINDING     (MM)
*      KWITYP =    WINDING TYPE CODE:
*                  1 = S     SCREW WINDING
*                  2 = L     LAYER WINDING
*                  3 = CD    CONT. DISC WINDING
*                  4 = SD    STAB. DISC WINDING
*                  5 = D2    DISC-TWO WINDING
*                  6 = SLS   SCREW WINDING
*                  7 = SLL   SERIES-LOOP WINDING
*                  8 = BND   FOIL WINDING
*                  9 = LNX   MULTI-LAYER WINDING
*      PLIMB  =    LIMB PITCH               (MM)
*      ACOND  =    TOTAL CONDUCTOR AREA     (MM2)
*      RAACON =    CONDUCTOR DENSITY        (G/CM3)
*      NCOND  =    CONDUCTOR TYPE:
*                  1 = CU
*                  2 = AL
*                  3 = TRANSPOSED
*      NWOLI  =    NUMBER OF WOUND LIMBS
*      HPART  =    STRAND HEIGHT (AXIALLY)  (MM)
*      BPART  =    STRAND WIDTH (RADIALLY)  (MM)
*      RPRTAR =    NUMBER OF STRANDS AXIALLY  IN THE SAME CABLE
*      RPRTRR =    NUMBER OF STRANDS RADIALLY IN THE SAME CABLE
*      TCOV1  =    DOUBLE SIDED PAPER COVERING OF THE STRAND  (MM)
*                  FOR FOIL WIND: INSULATION BETWEEN TURNS(MM)
*                  STRIP THICKNESS FOR GLUE- AND MYLAR STRIPS
*      TCOV2  =    DOUBLE SIDED PAPER COVERING OF THE CABLE   (MM)
*                  FOR LNX WIND:TOTAL DOUBLE SIDED PAPER COVERING (MM)
*      ZLOOP  =    NUMBER OF LOOPS IN A SLL-WINDING
*      ZWIND  =    NUMBER OF TURNS IN THE WINDING
*      DWIND  =    MEAN DIAMETER OF THE WINDING              (MM)
*      WWIND  =    TOTAL WINDING WIDTH                       (MM)
*      DUCTI  =    TOTAL WIDTH OF INSIDE MAIN DUCT           (MM)
*      ISAM   =    CODE FOR COMBINED WINDING PRODUCTION
*                  1 =  SEPARATE WINDING PRODUCTION
*                  2 =  COMBINED WINDING PRODUCTION
*      TCLT   =    TOTAL HEIGHT OF ALL CLACKS IN ONE CLACK ROW (MM)
*      RGUID  =    NUMBER OF OIL-GUIDING RINGS
*      RNCOOL =    NUMBER OF INNER VERTICAL COOLING DUCTS
*      HSPOL  =    HEIGHET OF EACH COIL (MM), LNX, BND - WINDING
*      HCLAC  =    INSULATION THICKNESS BETWEEN EACH LAYER (MM)
*      ZLAG   =    NUMBER OF LAYERS IN EACH COIL, LNX.
*      ZCOIAR =    NUMBER OF COILS AXIALLY IN EACH WINDING.
*      NGROUP =    NUMBER OF PARALLEL GROUPS
*      IPISOL =    PART ISOLATION
*                  1 = PAPER
*                  2 = MYLAR PRESSPAHN
*                  3 = GLUED STRIP
*-----------------------------------------------------------------------
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
       DATA    PI/3.1415927/
*

*-----------------------------------------------------------------------
       IF(.NOT.BBPRNT) GO TO 101
       CALL       PMASS2(BBPRNT,06,I,DCORE,HLIMB,RLH,RJH,KWITYP,PLIMB,
     *                   ACOND,RAACON,NCOND,NWOLI,HPART,BPART,RPRTAR,
     *                   RPRTRR,
     *                   TCOV1,TCOV2,ZLOOP,ZWIND,DWIND,WWIND,DUCTI,ISAM,
     *                   TCLT,RGUID,RNCOOL,PCSNED,HSPOL,HCLAC,ZLAG,
     *                   ZCOIAR,NGROUP,IPISOL)
 101   CONTINUE
*-----------------------------------------------------------------------
       ZLOOPX= 1.
       IF(ZLOOP.GT.0.) ZLOOPX= ZLOOP
*-----------------------------------------------------------------------
****   CONDUCTOR NET MASS INCLUDING ADDITIONS FOR START AND
****   FINISH OF THE WINDING.
*      ADD  1.5 % OF THE TOTAL LENGTH, BUT NEVER LESS THAN 2500 MM.
*      ADD FOR DISC-TWO WINDINGS: (LIMB PITCH)/4 + 200   MM.
*
       GSN11(I)= PI*1.E-6*DWIND * ZWIND *ACOND*RAACON*NWOLI
       PLUS1 = 0.015 * GSN11(I)
       PLUS2 = 2500.
       IF (KWITYP.EQ.5)  PLUS2= PLIMB/4. + 200.
       PLUS2= PLUS2*(1.E-6 * ACOND * RAACON * NWOLI * ZLOOPX)
       GSN11(I)= GSN11(I) + AMAX1(PLUS1,PLUS2)
*
*-----------------------------------------------------------------------
****   PAPER COVERING
*
       GSN121(I) = 0.
       GSN122(I) = 0.
*
       IF(NCOND.EQ.3) GOTO 39
*
*      CALCULATE TOTAL CABLE LENGTH
       CBLLNG = GSN11(I)/HPART/BPART/RPRTAR/RPRTRR/RAACON
*
*      ASSIGN DENSITIES FOR DIFFERENT COVERING (KG/DM3)
CC*KOM
CC*    RAASP #NDR FR .375 T .75 (VERKLIGT V#RDE)
CC*    TIDIGARE MULT. MED 2 I MASS BER#KN TAS BORT
CC*
       RAASP = 0.750
       RAAOR = 1.140
       RAALR = 1.167
*
       GO TO (31,31,31,31,31,31,31,32,33),KWITYP
*
*      NOT FOIL OR LAYER WINDINGS
 31    GO TO (311,312,313),IPISOL
*      PAPER ISOLATION
 311   GSN121(I) = (HPART+BPART+TCOV1)*RPRTAR*RPRTRR*CBLLNG*
     *             TCOV1*RAASP
       GSN122(I) = (RPRTAR*(HPART+TCOV1)+RPRTRR*(BPART+TCOV1)+TCOV2)*
     *             CBLLNG*TCOV2*RAASP
       write(*,*) "RPRTAR",RPRTAR
       write(*,*) "HPART",HPART
       write(*,*) "TCOV1",TCOV1
       write(*,*) "BPART",BPART
       write(*,*) "TCOV2",TCOV2
       write(*,*) "CBLLNG",CBLLNG
       write(*,*) "RAASP",RAASP
       write(*,*) "kwityp",KWYTYP
       GO TO 399
*      MYLAR PRESSPAHN
 312   GSN121(I) = HPART*RPRTAR*(RPRTRR-1.)*CBLLNG*TCOV1*RAAOR
       GSN122(I) = (HPART*RPRTAR+BPART*RPRTRR+(RPRTRR-1.)*TCOV1+TCOV2)*
     *             CBLLNG*TCOV2*RAASP
       GO TO 399
*      GLUED STRIP
 313   GSN121(I) = HPART*RPRTAR*(RPRTRR-1.)*CBLLNG*TCOV1*RAALR
       GSN122(I) = (HPART*RPRTAR+BPART*RPRTRR+(RPRTRR-1.)*TCOV1+TCOV2)*
     *             CBLLNG*TCOV2*RAASP
 399   CONTINUE
       GO TO 39
*
*      FOIL WINDING
 32    GSN121(I)= ((HPART+30.)*TCOV1 + BPART*30.)*ZWIND*DWIND*PI*
     *             NGROUP*NWOLI*1.1E-6
*                 .... BPART*30. IS CROSS SECTION AREA OF EDGE FILLING...
        GSN122(I) = 0.
       GO TO 39
*
*      MULTI LAYER WINDING
*      INSULATION STRIPS BETWEEN STRANDS
 33    GSN121(I) = (HPART+1.)*0.3*DWIND*PI*ZWIND*NGROUP*
     *             (RPRTRR-1.)*1.4E-6 * NWOLI
*      PAPER COVERING CABLE (STRAND IF(RPRTRR = 1))
              HLPA= HPART
              HLPR= RPRTRR*(BPART+0.3) - 0.3
       GSN122(I) = RAASP/RAACON*(TCOV2+HLPA+HLPR)*TCOV2/HLPA/HLPR*
     *             GSN11(I)
 39    CONTINUE
*-----------------------------------------------------------------------
****   STANDARD VALUES USED BELOW:
*      NUMBER OF CLACK ROWS:
              PCLACK = DWIND * 0.024737
*             THIS WILL GIVE 30 % CLACK COVERAGE WITH CLACK WIDTH 38 MM
*      CLACK WIDTH (MM):
              WCLACK = 38.
*      PRESSPAHN DENSITY (KG/MM3):
              RAAPN=1.22E-6
*      DUCT INSIDE WINDING AGAINST THE  FORMER CYLINDER (MM):
              WDUCT= 11.
*      CLACK PITCH FOR INNER COOLING DUCTS (MM):
              PCLBD= 50.
*      WIDTH OF RIBS IN MAIN DUCT (MM):
              RIBB = 20.
*      THICKNESS OF FORMER CYLINDER (MM):
              TFOCY= 3.
*      MEAN CIRCUMFERENCE OF FORMER CYLINDER (MM):
              WFOCY=(DWIND-WWIND-WDUCT*2.-TFOCY)*PI
*      NUMBER OF SPLIT CYLINDERS INSIDE THE WINDING:
              RCYL= (DUCTI-WDUCT)/14. -1.
              IF(RCYL.LT.0.) RCYL = 0.
*      HEIGHT OF THE FORMER CYLINDER (MM):
              HFOCY= RJH - 6.
*-----------------------------------------------------------------------
****   INSULATION DETAILS IN THE MAIN DUCT
****   SEPARATE WINDING PRODUCTION
       GO TO (41,41,41,41,41,41,41, 43,42), KWITYP
*
 41    GSN131(I)=RAAPN*HFOCY*(WDUCT*PCLACK*RIBB+TFOCY*WFOCY)
     *            *NWOLI
*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
****   MASS OF INSULATION DETAILS IN THE MAIN DUCT FOR "ACTIVE PART"
****   -OR- IN CASE COMBINED WINDING PRODUCTION, ADD TO INSULATION
****   DETAILS IN MAIN DUCT.
       HELP1= RAAPN*NWOLI*HFOCY
       HELP2= HELP1*RIBB *(DUCTI-TFOCY-WDUCT-RCYL)*PCLACK
       IF(HELP2.LT.0.)  HELP2 = 0.
       HELP3= HELP1*PI*RCYL *(DWIND-DUCTI)
*
       GSN41(I)= 0.
       IF(ISAM.EQ.1) GSN41(I)= HELP2 + HELP3
       IF(ISAM.EQ.2) GSN131(I)= GSN131(I) + HELP2 + HELP3
       GO TO 49
*
****   MULTI LAYER WINDING
 42    GSN41(I) = 0.
       GSN131(I)=PI*RAAPN*0.4*DUCTI*RLH*NWOLI*
     *           (DWIND-WWIND-DUCTI)
*                 .... SPACE FACTOR = 0.4 ........
       GO TO 49
*
****   FOIL WINDING: PLASTIC TUBE AND EPOXY
*.... X1 = TUBE LENGTH .....
 43    X1 = (DWIND-WWIND-DUCTI)*PI*RLH/200.
       GSN131(I) =(( X1*DUCTI*50. + 3.E6)*1.65E-6 + (X1+2000.)*0.37E-3)*
     *            NWOLI
*
*     FORMER CYLINDER FOR FOIL WINDING
      GN1352(I) = RAAPN*HFOCY*TFOCY*WFOCY*NWOLI
 49    CONTINUE
*-----------------------------------------------------------------------
*
****   INSULATION DETAILS AGAINST THE YOKES:
       GO TO (60,70,50,50,50,70,70,50,50,50),KWITYP
*
*...   CONT.DISC, STAB. DISC, DISC-TWO, FOIL OR  MULTI-LAYER
 50    GSN132(I)= 102.E-8*NWOLI*WDUCT**0.2*WWIND*DWIND*(RJH-RLH)
       GO TO 80
*
*...   SCREW:
 60    GSN132(I)= 95.E-7*NWOLI*WDUCT**(-0.3)*WWIND*DWIND*
     *                  (RJH-RLH  +RLH/(ZWIND+1.))
       GO TO 80
*
*...   LAYER, SERIES-LOOP  OR SERIES-LOOP SCREW WINDING
 70    GSN132(I)=395.E-8*NWOLI*WWIND*DWIND*
     *                  (RJH-RLH +RLH/(ZWIND+1.))
*
 80    CONTINUE
*-----------------------------------------------------------------------
*
****   OIL-GUIDING RINGS
       GSN133(I)=0.
       IF(RGUID.GT.1..AND.KWITYP.LE.7)
     * GSN133(I)=RAAPN* PI*WDUCT*DWIND*(HPART+TCOV1+TCOV2)*RGUID
     *           * NWOLI
*-----------------------------------------------------------------------
*
****   CLACK BAND FOR INNER COOLING DUCTS
       IF(KWITYP.LE.7)
     * GSN134(I)=NWOLI*DWIND*PI*RNCOOL*(RLH-TCLT)*
     *         (1.10E-4/PCLBD + 2.5E-7)
*
*...   FOIL AND MULTI-LAYER WINDINGS
       IF(KWITYP.GE.8)
     * GSN134(I)= NWOLI*DWIND*PI*RNCOOL*RLH*NWOLI*1.2*RAAPN
*       .... SPACEFACTOR 0.4  * THICKNESS 3 MM   GIVES 1.2 .....
*----------------------------------------------------------------------
       IF(KWITYP.EQ.9)
****   LAYER INSULATION IN LNX-WINDINGS:
     * GN1351(I) = DWIND*PI*(HSPOL+50.)*HCLAC*1.11*(ZLAG+1)*
     *             ZCOIAR*1.E-6*NWOLI
*      .... DENSITY 1.01 G/CM3, OVER-LAP 1.10  GIVES 1.11 .....
*-----------------------------------------------------------------------
       IF(KWITYP.GT.7) GO TO 86
*
****   CLACKS
       GSN135= RAAPN*WCLACK*TCLT*NWOLI*WWIND*PCLACK
       GN1352(I) = PCSNED/100.* GSN135
       GN1351(I) = GSN135 - GN1352(I)
*-----------------------------------------------------------------------
 86    CONTINUE
*
****   LOCKING CYLINDERS
       GSN136(I) =0.
       IF(WWIND.LE.9.5.AND. KWITYP.LE.7) GO TO 98
       GO TO (98, 95,98,98,98,95,95,98,97), KWITYP
*...   LAYER, SERIES-LOOP SCREW  OR  SERIES-LOOP  WINDINGS
 95    GSN136(I) = 1.95E-5*NWOLI*(DWIND+WWIND+5.)*(RJH-RLH+3.*RLH/
     * (ZWIND+1.))
       GO TO 99
*...   MULTI LAYER WINDING: EDGE SUPPORT
 97    GSN136(I)= ZLAG*DWIND*PI*1.2E-6*(26.+ ACOND/BPART/RPRTRR)*
     *            ZCOIAR*NWOLI
       GO TO 99
*...   OTHER WINDING TYPES
 98    GSN136(I) = 0.
*
 99    CONTINUE
       RETURN
       END
