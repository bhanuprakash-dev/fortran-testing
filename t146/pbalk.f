C*..* PBALK
CPBALK    *******PBALK - MASS OF CORE CLAMPS
       SUBROUTINE PBALK (BBPRNT,JFC,DCORE,OH,BH,BD,TLIT,BBEXAC,SMVA,
     *                   OL,TK,KCORE,NCOR,BBSLIM,GSN211)
*
*      MASS OF CORE CLAMPS
*      *********************
*
*
*      BBPRNT  PRINTOUT OF INPUT/OUTPUT DATA IF TRUE
*      JFC     FILECODE FOR PRINTOUT
*      KCORE = CORE SERIES CODE
*              1 = TAA
*              2 = TBA
*              3 = -
*              4 = TCA (WITH AND WITHOUT SIDELIMBS)
*              5 = LTB
*
*      NCOR  = CORE TYPE CODES:
*              1     3     7     8      9      10
*              D     T     EY    DY     TY-1   TY-3
*
*      DCORE = CORE DIAMETER    (MM)
*      OH    = YOKE HEIGHT      (MM)
*      BH    = LIMB HEIGHT      (MM)
*      BD    = LIMB PITCH       (MM)
*      TLIT  = THICKNESS OF WINDING SUPPORT (MM)
*      SMVA  = RATED POWER      (MVA)
*      OL    = YOKE LENGTH      (MM)
*      TK    = CORE THICKNESS   (MM)
*      BBSLIM= .TRUE.  IF SIDE LIMB CORE
*              .FALSE. IF NOT SIDE LIMB CORE
*      BBEXAC= .TRUE.  DURING  EXACT CALCULATION
*              .FALSE. DURING OPTIMIZING
*.......................................................................
*      FIXED VALUES ARE SAVED DURING OPTIMIZING IN THE MAIN LINK
*      IF ROUTINE PBALK IS LINKED OUT COMMON AREA /SPBALK/ MUST BE
*      SAVED IN THE MAIN LINK
       COMMON /SPBALK/ IPBALK,SBALK1,SBALK2,SBALK3,SBALK4,TBALK
*
       LOGICAL BBEXAC,BBSLIM,BBPRNT
*
*      TAA -DATA
       DIMENSION B(3),G(3),B85(3),G1(3),G2(3),H86(3),G3(3)
       DATA
     * B   /  120.,       200.,      260.     / ,
     * G   /  43.6E-3,    59.3E-3,   75.0E-3  / ,
     * B85 /  100.,       150.,      200.     / ,
     * G1  /  10.9E-3,    22.3E-3,   43.6E-3  / ,
     * G2  /  2.47E-3,    5.55E-3,   7.99E-3  / ,
     * H86 /  200.,       250.,      300.     / ,
     * G3  /  1.58E-3,    2.47E-3,   3.55E-3  /
*

*.......................................................................
       IF(.NOT.BBPRNT) GO TO 101
       WRITE(JFC,*) '===== INPUT DATA TO PBALK ====='
       WRITE(JFC,*) DCORE,OH,BH,BD
       WRITE(JFC,*) TLIT,BBEXAC,SMVA,OL
       WRITE(JFC,*) TK,KCORE,NCOR,BBSLIM
 101   CONTINUE
*
*
       IF(BBSLIM)     GO TO 80
       IF(KCORE.EQ.1) GO TO 30
                      GO TO 50
*
*.......................................................................
*
*      TAA
 30    CONTINUE
       IF(.NOT.BBEXAC)  GO TO 40
       IPBALK=1
       IF(DCORE.GT.315) IPBALK = 2
       IF(DCORE.GT.430) IPBALK = 3
 40    RL851 = 2.* B85(IPBALK) + TK + 130.
       Q851 = 8.*  G3(IPBALK)*RL851
       RL850 = OH + BH + H86(IPBALK) + 160.
       Q850 = 8. * G2(IPBALK)*RL850
       Q033 = G1(IPBALK) * TK * 4.
       Q032 = (2.*B85(IPBALK) + TK +8.)* 0.1536
       GSN211 =
     * (G(IPBALK)*4.*(3.*BD+B(IPBALK))+Q032+Q033+Q850+Q851)*1.06
*CC    4.24 = 4*1.06
       GO TO 999
*.......................................................................
*
*...   TBA - CORE
 50    CONTINUE
       Q4= 0.99
       IF(.NOT.BBEXAC) GO TO 70
*
       IF(TLIT.GT.0.1) GO TO 60
*    .... NO WINDING SUPPORT ....
       SBALK1 = 1.082
*
       HELP=  (BD-DCORE)/2.
                         SBALK2 = 188.
       IF(HELP.GT.285.)  SBALK2 = 240.
       IF(HELP.GT.350.)  SBALK2 = 370.
*
                         TBALK = 20.
       IF(DCORE.GE.580.) TBALK = 30.
       IF(BD.GE.1000.)   TBALK = 30.
       IF(DCORE.GE.800.) TBALK = 40.
       IF(BD.GE.1400.)   TBALK = 40.
*
                         SBALK3 = 421.
       IF(SMVA.GT.40.)   SBALK3 = 615.
*
       GO TO 70
*
*    .... WINDING SUPPORT ....
 60    CONTINUE
       SBALK1 = 1.013
*
                                  SBALK2 =  530.
       IF( (BD-DCORE)/2..GT.350.) SBALK2 = 857.
*
                          SBALK3 =  875.
       IF( DCORE.GT.800)  SBALK3 = 1605
*
       TBALK =  30.
*
 70    CONTINUE
       GSN211 =(OH*OL*TBALK*SBALK1*32.E-6 + SBALK2 + SBALK3 )* Q4
       GO TO 999
*.......................................................................
*
*...   LTB - CORE
 80    CONTINUE
       QK1 =  0.93
       QK2 =  1900.
       QK3 =  1975.
*
       IF (NCOR.EQ.10) GO TO 90
       QK1 =  1.0
       QK2 =  805.
       QK3 =  1045.
*
 90    QK4 =  0.9917
       TBALK = 40.
*
       GSN211 =QK4* (OH*OL*TBALK* QK1 * 32.E-6 + QK2 + QK3)
*......................................................................
 999   CONTINUE
*
       RETURN
       END
