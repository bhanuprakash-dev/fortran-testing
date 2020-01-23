C*..* LMIN
CLMIN      SUBRUTIN  T18F07 - LMIN.
C
CC*   MODIFIERAD TILL F77  820301  HEDERSTR@M  ADSAC
C
C
      SUBROUTINE LMIN (N,NB,X,F,XREL,GREL,EPSI,FEPSI,DELTA,
     &                 MFIRST,NFMX,NCYK,NF,ITR,TRYCK,FUNK,
     &                 HV,PARAM,ICONV)
C
      DIMENSION  X(*),XREL(*),HV(*),GREL(*)
      DIMENSION  PARAM(*)
      INTEGER    SIGLIM,STPLIM,PCONT,BNHV
      REAL       MFIRST
      EXTERNAL   TRYCK,FUNK
      print*,"START OF LMIN"
C
C.... VID ANROPET #R :
C     NB     = M
C     EPSI   = EPS
C     FEPSI  = FEPS
C     DELTA  = DRV
C     MFIRST = AF
C.... OCH I PARAM-VEKTORN #R
C     PARAM( 4)= NLIM = SIGLIM
C     PARAM( 5)= NDR  = PCONT
C     PARAM( 6)= DEPS = DEPSI
C     PARAM( 7)= AMAX = AMMAX
C     PARAM( 8)= REPS = ERRM
C     PARAM(10)= NORC = STPLIM
C     PARAM(11)= ORF  = GAMMA
C     PARAM(12)= BETA
C     PARAM(13)= UTSKR= IUT3
C
      print *,'u are in lmin'
      TD     = 1.E-5
      ALFA   = 1.6
      SIGLIM = 2

      PCONT  = 6
      DEPSI  = 0.005

      AMMAX  =100.*MFIRST
      ERRM   = 0.

      NOR    = 0
      STPLIM = 6
      GAMMA  = 0.1
      BETA   = 6
      NLB    = 0
      MNRES  = 3
      ICONV  = 0

      IUT1   = 0
      IUT2   = 1
      IUT3   = 0
      IUT4   = 1
      IUT5   = 0
      IUT6   = 0
      IUT7   = 0
      IUT8   = 0

C.... SWITCHAR F@R UTSKRIFTER : 1 GER UTSKRIFT.
C     IUT1 = DERIVATABER.
C     IUT2 = UTSKRIFT AV AM
C     IUT3 = UTSKRIFT AV F,X OCH G-V#RDEN VID VARJE FUNKTIONSANROP.
C     IUT4 = ENKLA EL. DUBBLA DIFFBER.
C     IUT5 = UTSKRIFT AV FDELTA OCH RES.
C     IUT6 = UTSKRIFT AV AMAX.
C     IUT7 = UTSKRIFT AV SLUTRES. 5 SISTA CYKLERNA
C     IUT8 = TEST AV LP-L@SNING
C
      IF(PARAM(1).EQ.0.) GOTO 20
      IF(PARAM(2).NE.0.) TD     = PARAM(2)
      IF(PARAM(3).NE.0.) ALFA   = PARAM(3)
      IF(PARAM(4).NE.0.) SIGLIM = PARAM(4)
      IF(PARAM(5).NE.0.) PCONT  = PARAM(5)
      IF(PARAM(6).NE.0.) DEPSI  = PARAM(6)
      IF(PARAM(7).NE.0.) AMMAX  = PARAM(7)
      IF(PARAM(8).NE.0.) ERRM   = PARAM(8)
      IF(PARAM(9).NE.0.) NOR    = PARAM(9)
      IF(PARAM(10).NE.0.)STPLIM = PARAM(10)
      IF(PARAM(11).NE.0.)GAMMA  = PARAM(11)
      IF(PARAM(12).NE.0.)BETA   = PARAM(12)
      IF(PARAM(13).NE.0.)IUT3   = PARAM(13)
      IF(PARAM(1).EQ.1.) GOTO 20
      IF(PARAM(14).NE.0.)IUT4   = PARAM(14)
      IF(PARAM(15).NE.0.)IUT1   = PARAM(15)
      IF(PARAM(16).NE.0.)IUT2   = PARAM(16)
      IF(PARAM(17).NE.0.)IUT5   = PARAM(17)
      IF(PARAM(18).NE.0.)IUT6   = PARAM(18)
      IF(PARAM(19).NE.0.)IUT7   = PARAM(19)
      IF(PARAM(20).NE.0.)IUT8   = PARAM(20)
      IF(PARAM(21).NE.0.)NLB    = PARAM(21)
      IF(PARAM(22).NE.0.)MNRES  = PARAM(22)
      IF(PARAM(1).EQ.2) GOTO 20
      IF(PARAM(23).NE.0.)NHV    = PARAM(23)
   20 CONTINUE
      NPP=0
      NBP=0
      NL=0
      DO 30 I=1,N
   30 IF(XREL(I).EQ.0) NPP=NPP+1
      DO 40 I=1,NB
      IF(GREL(I).EQ.0) NBP=NBP+1
   40 IF(GREL(I).LT.0.)NL=NL+1
      ME=2*N+NB
      LCOLE=ME+2*N-NL+1

      I1=1
      I2=I1+(ME-2*NPP-NBP+1)*(LCOLE-4*NPP-NBP)
      I3=I2+(NB-NBP+1)*(N-NPP)
      I4=I3+(N-NPP+1)*(N-NPP)
      I5=I4+5*N
      I6=I5+(N-NPP+1)*2
      I7=I6+N-NPP
      I8=I7+N-NPP
      I9=I8+NB
      I10=I9+N-NPP
      I11=I10+N-NPP
      I12=I11+N-NPP
      I13=I12+2*N
      I14=I13+N
      I15=I14+N-NPP
      I16=I15+NB
      I17=I16+NB
      I18=I17+N-NPP
      I19=I18+N+1
      I20=I19+5*NB
      I21=I20+8*ME+8*N+10+NL
      I22=I21+N-NPP
      I23=I22+NB
      I24=I23+NB
      I25=I24+N
      I26=I25+N
      I27=I26+NB
      I28=I27+NB-NBP
      BNHV=I28

C
      IF(PARAM(1).NE.3) GOTO 60
      IF(NHV.LT.BNHV)ICONV = -3
   60 PARAM(23)=BNHV
C
      J1=I20
      J2=I20+N
      J3=I20+4*N+NB+1
      J4=J3+ME+1
C
C     HV(I1) = E
C     HV(I2) = G
C     HV(I3) = P
C     HV(I4) = LASTX
C     HV(I5) = SIGNAL
C     HV(I6) = AM
C     HV(I7) = F
C     HV(I8) = G0
C     HV(I9) = LIMIT
C     HV(I10)= XF
C     HV(I11)= X0
C     HV(I12)= XDELTA
C     HV(I13)= XC
C     HV(I14)= XX
C     HV(I15)= G1
C     HV(I16)= G2
C     HV(I17)= SIGMA
C     HV(I18)= ALFA1
C     HV(I19)= LASTG
C     HV(I20)= HV
C     HV(I21)= INDEX
C     HV(I22)= G3
C     HV(I23)= GC
C     HV(I24)= AMAX
C     HV(I25)= CENT
C     HV(I26)= EQLP
C     HV(I27)= INDG
C.... SPECIALUPPDELNINGAR AV HV(I20). SLASKAREOR
C     HV(J1)= CD
C     HV(J2)= AA
C     HV(J3)= EQG

      NE=ME+1-2*NPP-NBP
      NG=NB+1-NBP
      NP=N+1-NPP
C
      print *,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,'pavan'
      print *,'lm1a'
      CALL LM1A(N,NB,NLB,X,F,XREL,GREL,NFMX,
     &NF,NCYK,ITR,TRYCK,FUNK,ICONV,
     &ALFA,SIGLIM,BETA,STPLIM,MFIRST,PCONT,DELTA,
     &EPSI,FEPSI,DEPSI,GAMMA,TD,NE,NG,NP,
     &IUT1,IUT2,IUT3,IUT4,IUT5,IUT6,IUT7,IUT8,
     &AMMAX,ERRM,NOR,PARAM,MNRES,NHV,BNHV,
     &HV(I1),HV(I2),HV(I3),HV(I4),HV(I5),HV(I6),
     &HV(I7),HV(I8),HV(I9),HV(I10),HV(I11),HV(I12),
     &HV(I13),HV(I14),HV(I15),HV(I16),HV(I17),
     &HV(I18),HV(I19),HV(I20),HV(I21),HV(I22),HV(I23),
     &HV(I24),HV(I25),HV(I26),HV(I27),HV(J1),HV(J2),HV(J3))
C
      print *,'out of lmin'
      RETURN
      END

