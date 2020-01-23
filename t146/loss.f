*********************************************************************
*                                                                   *
*      The copyright to the computer program herein is  the         *
*      property of ABB TRANSFORMERS , Sweden.  The  program         *
*      may be used or copied only with the written  permis-         *
*      sion of ABB TRANSFORMERS  or in accordance with  the         *
*      terms of agreement under which the program has  been         *
*      supplied.                                                    *
*                                                                   *
*      In no event shall ABB TRANSFORMERS   be  liable  for         *
*      incidental or consequential damages arising from use         *
*      of this program.                                             *
*                                                                   *
*********************************************************************

************************************************************************
*
*      Subroutine LOSS
*      ---------------
*
C...   Title:  Calculate the winding losses
C...           Calculate the resistive
C...           and eddy-current losses for a specified
C...           two-winding performance case
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-09-25 by B-G Bladh  , SETFO/TS
*          Losses for booster are included.
************************************************************************
*
       SUBROUTINE LOSS
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0691(1)    ,  CUR    (1)    ),
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ0771(1)    ,  EDDCON (1)    ),
     &  (XZ0811(1)    ,  FRACTW (1)    ),
     &  (XZ0911(1)    ,  KWITYP (1)    ),
     &  (XZ0951(1)    ,  PCUW   (1)    )
       EQUIVALENCE
     &  (XZ0961(1)    ,  PEDW   (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ1011(1)    ,  RWIND  (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1401       ,  APOED         )
       EQUIVALENCE
     &  (XZ1409       ,  BPOED         ),
     &  (XZ1443       ,  FLUXM         ),
     &  (XZ1446       ,  FREQ          ),
     &  (XZ1476       ,  HWINDM        ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1511       ,  PCU           ),
     &  (XZ1512       ,  PED           ),
     &  (XZ1513       ,  PI            ),
     &  (XZ1521       ,  POED          ),
     &  (XZ1532       ,  RMY0          )
       EQUIVALENCE
     &  (XZ1544       ,  SQR2          ),
     &  (XZ2271(1)    ,  FLUXMD (1)    ),
     &  (XZ2281(1)    ,  FLUXMW (1)    ),
     &  (XZ2401(1)    ,  FLUXI  (1)    ),
     &  (XZ2411(1)    ,  FLUXO  (1)    ),
     &  (XZ2729       ,  REBOOS        ),
     &  (XZ2730       ,  TRBOOS        ),
     &  (XZ2733(1)    ,  BBOOSW(1)     )
*
       REAL         CUR(9),DWIND(9),RRWDG(9),FRACTW(9),ZWIND(9),
     &              EDDCON(9),PCUW(9),PEDW(9),FLUXMD(9),FLUXMW(9),
     &              FLUXI(9),FLUXO(9),RWIND(9),APOED,BPOED,BX,B1,
     &              CURMAX,CURR,D0,D1,D2,FI0,FLUXD,FLUXM,FLUXW,FLUXW1,
     &              FLUXW2,FREQ,FW1,FW2,HWINDM,PCU,PED,PI,POED,RMY0,
     &              SCALE,SQR2,TWTHRD, TRBOOS, REBOOS
*
       INTEGER      IWDG,NWILI,KWITYP(9),MAXCW
*
       LOGICAL      BBOOSW(9)
*
       EXTERNAL     EDMULT
*
       DATA TWTHRD/ 0.6666667 /
*
CC*SEBL End of the declarations block.
*
       CURMAX=0.
       MAXCW=0
       FLUXM=0.
       FI0=0.
       FW2=0.
       FW1=0.
       PCU=0.
       PED=0.
       D0=0.
       SCALE=0.5*SQR2*PI*RMY0/HWINDM
*
C... Calculate for each winding
*
       DO 299 IWDG=1,NWILI
       FLUXI(IWDG)=0.
       FLUXO(IWDG)=0.
       CURR=CUR(IWDG)
*
C... Part 1, Eddy current losses
*
C,,, NON-current-carrying winding
*
       IF(CURR.EQ.0.) THEN
          PCUW(IWDG)=0.
          PEDW(IWDG)=EDMULT(0.0,FW2,EDDCON(IWDG))
*
C... Current-carrying winding
*
       ELSE
          D1=DWIND(IWDG)
          B1=RRWDG(IWDG)
          FW1=ZWIND(IWDG)*FRACTW(IWDG)*CURR
*
C... Remember which winding that has highest current and calculate
C... the CURMAX.
*
          IF (ABS(CURR).GT.CURMAX) THEN
             CURMAX=ABS(CURR)
             MAXCW=IWDG
          ENDIF
*
C... Calculation of the resistive (I2R) losses in winding "IWDG"
*
CC*SBBL
*
          PCUW(IWDG)=RWIND(IWDG)*CURR*CURR*
     &               ABS(FRACTW(IWDG))
          IF(BBOOSW(IWDG))  PCUW(IWDG)=
     &          PCUW(IWDG)+ CURR*CURR*REBOOS*TRBOOS*TRBOOS
          PCU=PCU+PCUW(IWDG)
*
CC*SEBL
*
C... Calculation of the eddy-current losses in windings
C... due to the axial leakage flux
*
CC*SBBL
*
          PEDW(IWDG)=EDMULT(FW1,FW2,EDDCON(IWDG))
*
CC*SEBL
*
C... Leakage flux in the duct between windings "MUT(I-1)" and "IWDG"
*
CC*SBBL
*
          D2=D1-B1
*
          FLUXD=(D2*D2-D0*D0)*FW2/2.
          IF(ABS(FLUXD*SCALE).GT.ABS(FLUXMD(IWDG)))
     &           FLUXMD(IWDG)=FLUXD*SCALE
          FI0=FLUXD+FI0
          D0=D1+B1
*
CC*SEBL
*
C... Calculate the flux in the winding
*
C,,, If the ampere-turn diagram crosses the zero-line in the
C,,, actual winding "IWDG"
*
          IF (((FW1+FW2)*FW2).LT.0.) THEN
*
             FLUXI(IWDG)=FI0
             BX=0.
             IF(FW1.NE.0.) BX=-B1*FW2/FW1
             FLUXW1=BX*(D2+TWTHRD*BX)*FW2
             FI0=FLUXW1+FI0
             IF(ABS(FI0).GT.FLUXM) FLUXM=ABS(FI0)
             BX=B1-BX
             FLUXW2=BX*(D0-TWTHRD*BX)*(FW2+FW1)
             IF(ABS((FLUXW1+FLUXW2)*SCALE).GT.ABS(FLUXMW(IWDG)))
     &               FLUXMW(IWDG)=(FLUXW1+FLUXW2)*SCALE
             FI0=FLUXW2
             FLUXO(IWDG)=FI0
*
C... If the ampere-turn diagram DOES NOT cross the zero-line in the
C... actual winding "IWDG"
*
          ELSE
             FLUXI(IWDG)=FI0
             FLUXW=B1*(D1*(FW2+FW2+FW1)+B1*FW1/3.)
             IF(ABS(FLUXW*SCALE).GT.ABS(FLUXMW(IWDG)))
     &              FLUXMW(IWDG)=FLUXW*SCALE
             FI0=FLUXW+FI0
             FLUXO(IWDG)=FI0
          END IF
*
          FW2=FW2+FW1
*
       END IF
       PED=PED+PEDW(IWDG)
*
       FLUXI(IWDG)=FLUXI(IWDG)*SCALE
       FLUXO(IWDG)=FLUXO(IWDG)*SCALE
*
  299  CONTINUE
*
       CALL PEP1
*
       IF (ABS(FI0).GT.FLUXM) FLUXM=ABS(FI0)
       FLUXM=FLUXM*SCALE
*
C... Decide if we have a D2-winding as outer winding for correction
C... of high current transformers. If so we must correct BPOED to
C... zero as the Other eddy loss already is compensated.
*
       IF ((KWITYP(NWILI).EQ.5).AND.(MAXCW.EQ.NWILI)) THEN
          BPOED=0
       ENDIF
       POED=APOED*FLUXM**1.46+BPOED*CURMAX*CURMAX
       POED=POED*(FREQ/50.)**1.5
*
c       write(*,*)"loss FLUXO",FLUXO
       RETURN
       END
