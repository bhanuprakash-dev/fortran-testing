*********************************************************************
*                                                                   *
*      The copyright to the computer program herein is  the         *
*      property of ABB  TRANSFORMERS, Sweden.  The  program         *
*      may be used or copied only with the written  permis-         *
*      sion of ABB  TRANSFORMERS or in accordance with  the         *
*      terms of agreement under which the program has  been         *
*      supplied.                                                    *
*                                                                   *
*      In no event shall ABB  TRANSFORMERS  be  liable  for         *
*      incidental or consequential damages arising from use         *
*      of this program.                                             *
*                                                                   *
*********************************************************************

************************************************************************
*
*      Subroutine CORTA1
*      -----------------
*
*  0.  Written: 91-08-13 by J Johansson      , SETFO/TS
*      Revised: 91-11-27 by Ron Bell         , ABB Power T & D Muncie
*      Revised: 92-04-14 by B-G Bladh        , SETFO/TS
*
*  1.  Description
C...           Main routine for the  TA1 Core calculation
C...
C...           Based on 1ZBA-4650-101 dated 30 Octiber 1990
C...                    Paragraph 8.1: Acceleration Forces
C...                    Paragraph 8.2: Number of Yoke Bolts
C...                    Paragraph 8.3: Number of Flitch Plates
*
************************************************************************
*
       SUBROUTINE CORTA1(DCORNI,TCORST,RNK,PLIMBI,HLIMBI,FHDI,IFLPMT,
     &                   RMWBL,FAF,ACCLN,ACCVE,ETA,BSTEPI,BPLMAI,
     &                   BBMECH,RLAPSI,
     &                   BBSTLP,DEWBI,DIWBI,RMAP,
     &                   ACORE,RMLMB,RMYK,RMCRNR,
     &                   NPACK,NSHT,SHWDT,THKPCK,RMXWDT,RMNWDT,
     &                   HYOKE,HCORE,TKO,RNFLPL,
     &                   BBLARM,ALARM)
*
       COMMON /ROPTIM/ ARE,REST,TK,RLK1,BSMAX,BSMIN,
     &                 RMCORE,RMLIMB,RMCORN,RMYOKE,
     &                 RNS1,SW,TPKT
       REAL            ARE,REST,TK,RLK1,BSMAX,BSMIN,
     &                 RMCORE,RMLIMB,RMCORN,RMYOKE,
     &                 RNS1(100),SW(100),TPKT(100)
*
       COMMON /IOPTIM/ NPKTS
       INTEGER         NPKTS, NPACK
*
C... Input
*
C... Input in parameter list is in (m).
C... All variables named xxI are converted internally
C... in this routine to xx in (mm)
*
C... DCORN  Net Diameter of core, mm
C... TCORST Thickness of Core plate material
C... RNK    Number of cooling ducts
C... PLIMB  Limb pitch, mm
C... HLIMB  Limb Height, mm
C... FHD    Flitch plate hole diameter, mm
C... IFLPMT Flitch plate material, 1 - ST52, 2 OX812
C... RMWBL  Mass of winding block, kg
C... FAF    Prestressing force winding block, N
C... ACCLN  Longitudal transport acceleration , *G
C... ACCVE  Vertical transport acceleration, *G
C... ETA    Spacefactor of core sheets
C... BSTEP  Core Step Width increment, mm
C... BPLMAX Maximum  core plate width available, mm
C... BPLMAI Maximum  core plate width available, m
C... BBMECH Detailed mechanical calculation TRUE FALSE
*
C... The following is required if BBMECH = .TRUE.
C... RLAPS  Overlap width in joint, mm
C... BBSTLP Type of stepping, FALSE = norm, TRUE = steplap
C... DEWB   Outer diameter winding block, mm
C... DIWB   Inner diameter winding block, mm
C... RMAP   Mass of active part, kg
*
C... Output
*
C... ACORE  Area of Core Limb
C... RMLMB  Mass of Limb
C... RMYK   Mass of Yoke
C... RMCRNR Mass of Corner

C... NSHT   Number of sheets per packet, array
C... SHTWDT Width of sheets (per packet), array, m
C... THKPCK Thickness of package (per packet), array, m
C... RMXWDT Maximum width of an individual packet, m
C... RMNWDT Minimum width of an individual packet, m
C... HYOKE  Height of Yoke, m
C... HCORE  Height of Core, m
C... TKO    Thickness of core, m
C... RNFLPL Number of flitch plates
C... BBLARM Transmits a warning signal, LOGICAL
C... ALARM  Transmits a warning message, CHARACTER
*
       REAL       RMXWDT,RMNWDT,HYOKE,HCORE,TKO,FAX,FTV,BSTEPI,BPLMAI,
     &            DCORNI,HLIMBI,FHDI,RLAPSI,DEWBI,DIWBI,BSTEP,BPLMAX,
     &            DCORN,RNK,PLIMB,HLIMB,FHD,RMWBL,FAF,ACCLN,ACCVE,
     &            RLAPS,DEWB,DIWB,RMAP,ACORE,RMLMB,RMYK,RMCRNR,
     &            FFP,FMYLY,RMY,STOT,RNS,FYB,RNI,FBYP,PLIMBI,
     &            RNR,XI,FLC,SAFETY,BK,AWB,AWBY,X1,FAVYB,FAMYU,FAMYL,
     &            TCORST,BFP,FFRF,ETA,SINI,SINO,COSI,COSO,
     &            ANGI,RNO,ANGO,AD,FBUP,FU,FL,FYBUN,FMYU,FMYL,FTL,
     &            FAT,FCORE,FWBY,FAFY,FLYT,FLITO,FDIRV,FSTAT,FDYNV,
     &            ACCVEA,FDIRL,FDYNL,ACCLNA,RLFCT,FCOMB1,FCOMB2,FCOMB,
     &            ACCCA,ACCCMB,RNFLPL,FBUP1,FBUP2,FBYP1,FBYP2
*
       REAL       RMLF(5),RMVF(5),TFP(3,2),RMAF(5,2),CDR(5,2),
     &            RNBRS(5,4),SHWDT(100),THKPCK(100)
*
       INTEGER    I,IFLPMT,ICASE,NSHT(100),CS(5,10)
*
       LOGICAL    BBMECH,BBSTLP,BBLARM
*
       CHARACTER  ALARM*50

       include'consts.h'
*
C... Data Initialisation (ANSI does not permit data initialisation in
C... a type declaration statement.
*
       RMAF(1,1)= 400000.
       RMAF(1,2)= 800000.
       RMAF(2,1)= 800000.
       RMAF(2,2)=1700000.
       RMAF(3,1)= 800000.
       RMAF(3,2)=1700000.
       RMAF(4,1)=1300000.
       RMAF(4,2)=2500000.
       RMAF(5,1)=1300000.
       RMAF(5,2)=2500000.
*
       RMLF(1)=1000000.
       RMLF(2)=1700000.
       RMLF(3)=2600000.
       RMLF(4)=2600000.
       RMLF(5)=3200000.
*
       RMVF(1)=1000000.
       RMVF(2)=1500000.
       RMVF(3)=2000000.
       RMVF(4)=2000000.
       RMVF(5)=2000000.
*
       CDR(1,1)= 400.
       CDR(1,2)= 550.
       CDR(2,1)= 500.
       CDR(2,2)= 750.
       CDR(3,1)= 650.
       CDR(3,2)= 900.
       CDR(4,1)= 750.
       CDR(4,2)=1000.
       CDR(5,1)= 750.
       CDR(5,2)=1000.
*
       TFP(1,1)= 440000.
       TFP(1,2)= 860000.
       TFP(2,1)= 880000.
       TFP(2,2)=1720000.
       TFP(3,1)=1320000.
       TFP(3,2)=2580000.
*
       RNBRS(1,1)=2.
       RNBRS(1,2)=4.
       RNBRS(1,3)=2.
       RNBRS(1,4)=2.
       RNBRS(2,1)=4.
       RNBRS(2,2)=6.
       RNBRS(2,3)=4.
       RNBRS(2,4)=4.
       RNBRS(3,1)=6.
       RNBRS(3,2)=8.
       RNBRS(3,3)=6.
       RNBRS(3,4)=4.
       RNBRS(4,1)=6.
       RNBRS(4,2)=8.
       RNBRS(4,3)=6.
       RNBRS(4,4)=6.
       RNBRS(5,1)=6.
       RNBRS(5,2)=8.
       RNBRS(5,3)=8.
       RNBRS(5,4)=6.
*
       CS(1, 1)= 4
       CS(2, 1)= 8
       CS(3, 1)=12
       CS(4, 1)=12
       CS(5, 1)=16
       CS(1, 2)= 4
       CS(2, 2)= 8
       CS(3, 2)=12
       CS(4, 2)=12
       CS(5, 2)=12
       CS(1, 3)= 8
       CS(2, 3)=12
       CS(3, 3)=16
       CS(4, 3)=16
       CS(5, 3)=16
       CS(1, 4)=40
       CS(2, 4)=56
       CS(3, 4)=80
       CS(4, 4)=80
       CS(5, 4)=88
       CS(1, 5)=24
       CS(2, 5)=40
       CS(3, 5)=56
       CS(4, 5)=56
       CS(5, 5)=64
       CS(1, 6)= 8
       CS(2, 6)=16
       CS(3, 6)=24
       CS(4, 6)=24
       CS(5, 6)=24
       CS(1, 7)=12
       CS(2, 7)=24
       CS(3, 7)=24
       CS(4, 7)=36
       CS(5, 7)=36
       CS(1, 8)=24
       CS(2, 8)=48
       CS(3, 8)=48
       CS(4, 8)=72
       CS(5, 8)=72
       CS(1, 9)= 1
       CS(2, 9)= 2
       CS(3, 9)= 2
       CS(4, 9)= 3
       CS(5, 9)= 3
       CS(1,10)=12
       CS(2,10)=20
       CS(3,10)=28
       CS(4,10)=28
       CS(5,10)=32
*
       DO 299 I=1,100
          SHWDT(I)=0.
          THKPCK(I)=0.
  299  CONTINUE
*
C... BBLARM is used to transmit a warning to the designer via the panel
*
       BBLARM=.FALSE.
       ALARM='                                                  '
*
       DCORN = DCORNI*1000.
       BSTEP = BSTEPI*1000.
       PLIMB = PLIMBI*1000.
       HLIMB = HLIMBI*1000.
       write(*,*) 'cort1 Line 256 =',HLIMB
       BPLMAX= BPLMAI*1000.
       FHD = FHDI*1000.
       IF (BBMECH) THEN
          RLAPS = RLAPSI*1000.
          DEWB = DEWBI*1000.
          DIWB = DIWBI*1000.
       END IF


*
C... Estimate the core mass
*
       RMCORE=2.27E-5*(PLIMB+0.75*HLIMB+0.49*DCORN)*DCORN**2
*
C... Calculate the forces
*
       FFRF=6670.
       IF (IFLPMT.EQ.2) FFRF=13330.
*
       FAX=((3*RMWBL+2*RMCORE)*GFORCE+3*FAF)/3.+(FHD*FFRF)
       FTL=(RMCORE*GFORCE*ACCLN)
       FTV=(RMCORE*GFORCE*ACCVE)
*
C... Case definition
*
       DO 20 I=1,5
          IF ((RMAF(I,(IFLPMT)).GE.FAX).AND.
     &        (RMLF(I)         .GE.FTL).AND.
     &        (RMVF(I)         .GE.FTV)) GOTO 10
   20  CONTINUE
       WRITE(*,'(1X,//,1X,''Impossible to define a loading case:'',
     &       //,1X,''Axial Force        ='',F8.0,'' N'',
     &        /,1X,''Longitudinal Force ='',F8.0,'' N'',
     &        /,1X,''Vertical Force     ='',F8.0,'' N'')')
     &       FAX,FTL,FTV
       BBLARM=.TRUE.
       ALARM='Acceleration forces exceeded. Cannot proceed.'
       GOTO 199
*
   10  IF (DCORN.GT.CDR(I,2))THEN
          I=I+1
          GOTO 10
       ELSE
         IF (DCORN.LT.CDR(I,1))THEN
           WRITE(*,'(1X,//,1X,''The Core Diameter is smaller than '',
     &      ''the minimum defined for case '',I1)')I
           BBLARM=.TRUE.
           ALARM='Acceleration forces need a larger core diameter.'
           GOTO 199
         END IF
       END IF
       ICASE=I
CCC    WRITE(*,'(1X,//,1X,''----> FIRST CHOICE, CASE='',I1)')ICASE
*
C... Calculate the Flitch Plate geometry
*
 9889  BFP=60.*CS(ICASE,9)+4.*(CS(ICASE,9)-1)
*
C... Estimate TK
*
       TK=(SQRT((DCORN/2.)**2-(BFP/2.)**2)-15.)*2.
*
C... Calculate the Core geometry
*
       CALL OPTIM(DCORN,TK,TCORST,RNK,PLIMB,HLIMB,FHD,ETA,
     &            BSTEP,BPLMAX,BBLARM,ALARM)
*
       IF (BBLARM) THEN
          GOTO 199
       END IF
*
C... Load case verification
*
       IF (BBMECH) THEN
       AD=4.
       RMY=0.2
       ANGI=25.*PI/180.
       ANGO=45.*PI/180.
       SINI=SIN(ANGI)
       SINO=SIN(ANGO)
       COSI=COS(ANGI)
       COSO=COS(ANGO)
       FBUP1=182000.
       FBUP2=279600.
       FBYP1=121000.
       FBYP2=186000.
       STOT=(RLAPS/SQRT(2.))*2.
       IF (BBSTLP) STOT=(10./6.)*(RLAPS/SQRT(2.))
       RLFCT=1.5
*
 7788  FMYLY=3.*FAF*RLFCT
       RNS=(TK-RNK*AD)/(2.*TCORST)
       FYB=(FMYLY*BSMIN)/(2.*SQRT(2.)*RMY*STOT*RNS)
       RNI=(FYB/COSI)/FBYP1
       RNO=(FYB/COSO)/FBYP1
 2226  DO 2222 XI=2,6,2
          IF (RNI.LE.XI) GOTO 2223
 2222  CONTINUE
 2223  RNI=XI
       DO 2224 XI=4,8,2
          IF (RNO.LE.XI) GOTO 2225
 2224  CONTINUE
 2225  RNO=XI
       FU=FYB/(RNI*COSI)
       FL=(RMCORE*GFORCE/2.+FU*(RNO*SINO-
     &     RNI*SINI))/(RNO*SINO-RNI*SINI)
       IF (FL.GT.FBYP1)THEN
          IF (RNI.EQ.6.AND.RNO.EQ.8.)THEN
             IF (FL.LE.FBYP2)THEN
                FBYP=FBYP2
                FBUP=FBUP2
                GOTO 7281
             ELSE
                WRITE(*,'(1X,//,1X,
     &        ''The Yoke Bolts will not stand the active forces'')')
                BBLARM=.TRUE.
                ALARM='The Yoke Bolts not up to the active forces'
                GOTO 199
             END IF
          END IF
          FBYP=FBYP1
          FBUP=FBUP1
          RNI=RNI+2.
          RNO=RNO+2.
          GOTO 2226
       ELSE
          FBYP=FBYP1
          FBUP=FBUP1
       END IF
 7281  FYBUN=2.*((FL*RNO*SINO+FU*RNI*SINI)-
     &       (FU*RNO*SINO+FL*RNI*SINI))
       FMYU=(FU*RNO*COSO+FU*RNI*COSI)*2.*RMY
       FMYL=(FL*RNO*COSO+FL*RNI*COSI)*2.*RMY
       RNR=(FTL-FMYU-FMYL)/(FBUP*2.*SINO)*1.1
       DO 2227 XI=2,8,2
          IF (RNR.LE.XI) GOTO 2228
 2227  CONTINUE
*
       WRITE(*,'(1X,//,1X,
     &     ''The core needs more than 8 retaining bolts'')')
       BBLARM=.TRUE.
       ALARM='The core needs more than 8 retaining bolts'
       GOTO 199
*
 2228  RNR=XI
       FLC=FYBUN+FMYU+FMYL
       SAFETY=FLC/(RMCORE*GFORCE)
       IF (SAFETY.LT.1.6) WRITE(*,'(1X,//,1X,
     &    ''Warning: The Safety Factor for lifting ='',F4.2)')SAFETY
       BK=TK-2.*RLK1
       AWB=0.25*PI*(DEWB**2-DIWB**2)
       AWBY=0.5*(DEWB-DIWB)*BK
       X1=(4.*AWBY)/(3.*AWB)
       FAVYB=2.*((FBYP*RNO*SINO+
     &       FBYP*RNI*SINI)-(FU*RNO*SINO+FL*RNI*SINI))
       FAMYU=2.*RMY*(FU*RNO*COSO+FBYP*RNI*COSI)
       FAMYL=2.*RMY*(FBYP*RNO*COSO+FL*RNI*COSI)
       FAT=FAVYB+FAMYU+FAMYL
       FCORE=RMCORE*GFORCE
       FWBY=RMWBL*GFORCE*X1
       FAFY=3.*FAF*X1
       FLYT=FCORE+FWBY+FAFY
       SAFETY=FAT/FLYT
       IF (SAFETY.LE.1.2) WRITE(*,'(1X,//,1X,
     & ''Warning: The Safety Factor for PRESTRESSING ='',F4.2)')SAFETY
       FLITO=FCORE+FWBY
       SAFETY=FLC/FLITO
       IF (SAFETY.LE.1.6) WRITE(*,'(1X,//,1X,
     & ''Warning: The Safety Factor for LIFTING with PRESTRESSING ='',
     &    F4.2)')SAFETY
       FDIRV=FYBUN+2.*(FBUP-FL)*RNO*SINO
       FSTAT=FMYU+FMYL
       FDYNV=RNO*2.*RMY*(FBUP-FL)*COSO
       ACCVEA=(FDIRV+FSTAT+FDYNV)/FCORE
       IF (ACCVE.GT.ACCVEA)THEN
       RLFCT=RLFCT+0.1
       WRITE(*,'(1X,//,1X,
     &       ''Recalculating due to Vertical Acceleration.'')')
       GOTO 7788
       END IF
       FDIRL=2.*FBUP*RNR*SINO
       FDYNL=2.*RMY*FBUP*RNR*COSO
       ACCLNA=(FDIRL+FSTAT+FDYNL)/FCORE
       IF (ACCLN.GT.ACCLNA)THEN
          RLFCT=RLFCT+0.1
          WRITE(*,'(1X,//,1X,
     &      ''Recalculating due to Longitudinal Acceleration.'')')
          GOTO 7788
       END IF
       FCOMB1=SQRT((FDIRV+FSTAT+FDYNV)**2+(FDIRL+FDYNL)**2)
       FCOMB2=SQRT((FDIRV+FDYNV)**2+(FDIRL+FSTAT+FDYNL)**2)
       FCOMB=MIN(FCOMB1,FCOMB2)
       ACCCA=FCOMB/FCORE
       ACCCMB=SQRT(ACCVE**2+ACCLN**2)
       IF (ACCCMB.GT.ACCCA)THEN
       RLFCT=RLFCT+0.1
       WRITE(*,'(1X,//,1X,
     &     ''Recalculating due to Combined Acceleration.'')')
       GOTO 7788
       END IF
       FFP=1.1*(FYBUN/3.+FAF)+1.4*RMAP*GFORCE/3.
       DO 2229 XI=1,3
       IF (FFP.LE.(TFP(IFIX(XI),IFLPMT)-
     &               (FHD*FFRF))) GOTO 2230
 2229  CONTINUE
       WRITE(*,'(1X,//,1X,
     &      ''The core needs more than 3 Flitch Plates'')')
       BBLARM=.TRUE.
       ALARM='The core needs more than 3 Flitch Plates'
       GOTO 199
 2230  CONTINUE
*
C... Report Number of flitch plates/limb and side
*
       RNFLPL=XI*1.
       DO 2240 I=1,5
       IF (RNI.LE.RNBRS(I,1).AND.
     &     RNO.LE.RNBRS(I,2).AND.
     &     RNR.LE.RNBRS(I,3).AND.
     &     RNFLPL.LE.RNBRS(I,4)) GOTO 2250
 2240  CONTINUE
       WRITE(*,'(1X,//,1X,''Impossible to define a loading case'',
     &       //,1X,''Number of Inner Yoke Bolts     ='',F3.0,
     &        /,1X,''Number of Outer Yoke Bolts     ='',F3.0,
     &        /,1X,''Number of Retaining Yoke Bolts ='',F3.0,
     &        /,1X,''Number of Flitch Plates        ='',F3.0)')
     &        RNI,RNO,RNR,RNFLPL
       BBLARM=.TRUE.
       ALARM='Loading case problem. (Yoke bolts & Flitch Plates)'
       GOTO 199
 2250  IF (I.GT.ICASE) THEN
       WRITE(*,'(1X,//,1X,''-> SECOND CHOICE, CASE='',I1)')ICASE
       IF (CS(ICASE,9).NE.CS(I,9))THEN
       ICASE=I
       GOTO 9889
       END IF
       ICASE=I
       END IF
       END IF
*
       ACORE  = ARE/1.E6
       RMLMB  = RMLIMB
       RMYK   = RMYOKE
       RMCRNR = RMCORN
*
       RMXWDT = 0.
       RMNWDT = 5000.
       TKO = 0.
       DO 2099 I = 1,NPKTS
       NSHT (I)  = NINT(RNS1(I))
       SHWDT(I)  = SW(I)/1000.
       THKPCK(I) = TPKT(I)/1000.
       TKO = TKO + THKPCK(I)
       IF (RMXWDT .LT. SHWDT(I)) RMXWDT = SHWDT(I)
       IF (RMNWDT .GT. SHWDT(I)) RMNWDT = SHWDT(I)
 2099  CONTINUE
*
       HYOKE = RMXWDT
       HCORE = HLIMBI + 2.*HYOKE
       TKO = 2.* TKO + RNK*(AD/1000.)
       NPACK = NPKTS
*
  199  RETURN
       END
