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
*      Subroutine OPCALC
*      -----------------
*
C...   Title:  Calculation of the physical No. OF turns,
C...           winding dimensions, reactances & losses
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-09-23 by B-G Bladh  , SETFO/TS
*        Booster factor "FABOOS(IWDG)" have been introduced.
************************************************************************
*
       SUBROUTINE OPCALC(BBOUT)
*
C... Declarations
*
CC*SBBL Start of the declarations block
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0661(1)    ,  AWIND  (1)    ),
     &  (XZ0691(1)    ,  CUR    (1)    ),
     &  (XZ0701(1)    ,  CURDEN (1)    ),
     &  (XZ0781(1)    ,  FACT   (1)    )
       EQUIVALENCE
     &  (XZ0791(1)    ,  FILLF  (1)    ),
     &  (XZ0811(1)    ,  FRACTW (1)    ),
     &  (XZ0851(1)    ,  FWIND  (1)    ),
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ0951(1)    ,  PCUW   (1)    )
       EQUIVALENCE
     &  (XZ0961(1)    ,  PEDW   (1)    ),
     &  (XZ1111(1)    ,  WLOSS  (1)    ),
     &  (XZ1121(1)    ,  WLOSSM (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1201(1)    ,  ZWINDW (1)    )
       EQUIVALENCE
     &  (XZ1227(1)    ,  NMSTEP (1)    ),
     &  (XZ1231(1)    ,  NPSTEP (1)    ),
     &  (XZ1263(1)    ,  SRATEP (1)    ),
     &  (XZ1271(1,1)  ,  UN     (1,1)  )
       EQUIVALENCE
     &  (XZ1331(1)    ,  BBRR   (1)    ),
     &  (XZ1341(1)    ,  BBRW   (1)    ),
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1373       ,  BBAUTO        ),
     &  (XZ1380       ,  BBEXAC        )
       EQUIVALENCE
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1500       ,  NPG           ),
     &  (XZ1504       ,  NSG           ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1511       ,  PCU           ),
     &  (XZ1512       ,  PED           ),
     &  (XZ1517       ,  PLOMAX        ),
     &  (XZ1521       ,  POED          ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ1542       ,  SNOML         ),
     &  (XZ1555       ,  TURNRA        )
       EQUIVALENCE
     &  (XZ1574(1,1)  ,  PCUT   (1,1)  ),
     &  (XZ1575(1,1)  ,  PEDT   (1,1)  ),
     &  (XZ1576(1,1)  ,  POEDT  (1,1)  ),
     &  (XZ1577(1,1)  ,  PCEOT  (1,1)  )
       EQUIVALENCE
     &  (XZ1656(1,1)  ,  POSIT  (1,1)  ),
     &  (XZ1689(1,1,1),  URC    (1,1,1)),
     &  (XZ1690(1,1,1),  UXC    (1,1,1)),
     &  (XZ2245(1)    ,  WLOSSE (1)    ),
     &  (XZ2254(1)    ,  WLOSSR (1)    )
       EQUIVALENCE
     &  (XZ2271(1)    ,  FLUXMD (1)    ),
     &  (XZ2281(1)    ,  FLUXMW (1)    ),
     &  (XZ3010(1)    ,  NXSTEP (1)    ),
     &  (XZ2731(1)    ,  FABOOS (1)    )
*
       REAL      WLOSS(9),WLOSSM(9),ZWIND(9),ZWINDW(9),CURX,PCUEDW,
     &           ACOND(9),AWIND(9),CDIM(4),PCU,PED,PLOMAX,PLOSS,
     &           CDIR(4),CUR(9),CURDEN(9),FACT(9),FILLF(9),POED,
     &           FRACTW(9),FWIND(9),PCUW(9),PEDW(9),RINPG,SNOML,
     &           POSIT(9,4),SRATEP(4),UN(4,4),URC(4,4,3),UXC(4,4,3),
     &           WLOSSE(9),WLOSSR(9),FLUXMD(9),FLUXMW(9),SRED,
     &           TURNRA,UK,XX,PCUT(4,3),PEDT(4,3),POEDT(4,3),PCEOT(4,3),
     &           FABOOS(9)
*
       INTEGER   KGROUP(9),NMSTEP(4),NPSTEP(4),NXSTEP(3),ITAP,ITAPP,
     &           ITAP2,ITML,IWDG,IWDG1,IWDG3,JTAP,JTAP2,KTML,KTML1,
     &           KTML2,LTML,LTML2,MTML,NG,NPG,NSG,NTAPS,NWILI,ISTOP
*
       LOGICAL   BBAUTO,BBVFR,BBVR(4),BBRW(9),BBOUT,BBRR(9),
     &           BBLOSS,BBEXAC,BBUT1,BBUT2
*
CC*SEBL End of the declarations block
*
*
C... Set the number of taps
*
CC*SBBL Start of the taps block
*
       print*,'inside opcalc'
       NTAPS=4
       write(*,*) "UN",UN
*
C... For each terminal
*
       DO 109 ITML=1,MIN(NG,3)
*
C... Adjust the number of taps
*
C,,, If the terminal is regulated
*
       IF (BBVR(ITML)) THEN
          IF ((NXSTEP(ITML).EQ.NPSTEP(ITML))
     &    .OR.(NXSTEP(ITML).EQ.NMSTEP(ITML))
     &    .OR.(NXSTEP(ITML).EQ.0        )) NTAPS=3
       END IF
  109  CONTINUE
*
CC*SEBL End of the taps block
*
C... Calculation of physical No. of turns
*
       DO 299 IWDG=1,NWILI
       
       
       ZWIND(IWDG)=ZWINDW(IWDG)*TURNRA*FABOOS(IWDG)
       
       
       
       IF (.NOT.BBRR(IWDG))
     &          AWIND(IWDG)=ACOND(IWDG)*ZWIND(IWDG)/FILLF(IWDG)
                
       FLUXMD(IWDG)=0.
       FLUXMW(IWDG)=0.
       PCUW(IWDG)=0.
       PEDW(IWDG)=0.
       CURDEN(IWDG)=0.
       WLOSS(IWDG)=0.
       WLOSSE(IWDG)=0.
       WLOSSM(IWDG)=0.
       WLOSSR(IWDG)=0.
       IF (KGROUP(IWDG).EQ.4) CURDEN(IWDG)=SRATEP(4)/ACOND(IWDG)/UN(4,1)
  299  CONTINUE
       PLOMAX=0.
*
C... KTML = First terminal , LTML = Second terminal
*
       KTML=1
*
  300  IF (KTML.GT.2.OR.(KTML.EQ.2.AND.NG.EQ.2)) GOTO 399
       KTML2=KTML
*
       LTML=KTML+1
  400  IF (LTML.GT.NG.OR.LTML.GT.3) GOTO 499
       LTML2=LTML
*
       SRED=AMIN1(SRATEP(LTML),SRATEP(KTML))/SNOML
*
C... ITAP = Tap position on the first  terminal
C... JTAP = Tap position on the second terminal
*
       ITAP=1
       BBUT1=.FALSE.
  500  IF (ITAP.GT.NTAPS.OR.BBUT1) GOTO 599
       ITAP2=ITAP
*
C... Calculation of 'CDIM' and 'CDIR' = Rated current at base power
C... for terminal KTML.    CDIM for 'MAIN'; CDIR for 'REGULATING'
*
       CDIM(KTML)=SNOML/UN(KTML,ITAP2)
       CDIR(KTML)=CDIM(KTML)
       IF (KTML.EQ.NPG) RINPG=CDIM(KTML)
C
       JTAP=1
       BBUT2=.FALSE.
  600  IF (JTAP.GT.NTAPS.OR.BBUT2) GOTO 699
       JTAP2=JTAP
*
C... Calculation of 'CDIM' and 'CDIR' = Rated current at base power
C... for terminal LTML
*
       IF (BBVFR.AND.KTML2.EQ.2) JTAP2=ITAP2
       CDIM(LTML)=SNOML/UN(LTML,JTAP2)
       CDIR(LTML)=CDIM(LTML)
       IF (LTML.EQ.NPG) RINPG=CDIM(LTML)
*
       IF (BBAUTO) THEN
          IF (LTML.EQ.2) CDIM(NPG)=RINPG-CDIM(NSG)
          IF (LTML.GT.2.AND.KTML.EQ.NSG) CDIM(NPG)=CDIM(NSG)
          IF (BBVFR) CDIR(NPG)=CDIM(NPG)
       END IF
*
C... For each winding
*
       DO 899 IWDG1=1,NWILI
       KTML1=KGROUP(IWDG1)
*
C... Calculation of FACT = Sign of the current for a given 2-wdg case
*
       FACT (IWDG1)=0.
       FWIND(IWDG1)=0.
       CUR  (IWDG1)=0.
*
C... For the first three terminals
*
       IF (KTML1.NE.4) THEN
          IF (KTML1.EQ.KTML) FACT(IWDG1)=1.
          IF (KTML1.EQ.LTML) FACT(IWDG1)=-1.
*
C... For an auto connected transformer
*
*
C... For terminal 3
*
          IF (BBAUTO) THEN
             IF (LTML.EQ.3.AND.KTML1.EQ.NPG) THEN
                IF (BBVFR.OR..NOT.BBRW(IWDG1)) FACT(IWDG1)=1.
             END IF
          END IF
*
C... FRACTW = The connected portion of each winding is given a value
*
          ITAPP=ITAP2
          IF (FACT(IWDG1).LT.0.) ITAPP=JTAP2
          IF (BBVFR) ITAPP=MAX0(ITAP2,JTAP2)
          FRACTW(IWDG1)=POSIT(IWDG1,ITAPP)
*
C... Calculation of CUR, FWIND & CURDEN
C... FWIND is used for the reactance calculation
C... CUR   is used for the loss      calculation
*
C,,, If FACT(IWDG) <> 0
*
          IF (FACT(IWDG1).NE.0.) THEN
             CURX=CDIM(KTML1)
             IF (BBRW(IWDG1)) CURX=CDIR(KTML1)
             FWIND(IWDG1)=ZWIND(IWDG1)*FACT(IWDG1)*FRACTW(IWDG1)*
     &                    CURX/FABOOS(IWDG1)
             CUR(IWDG1)=CURX/FABOOS(IWDG1)*SRED*FACT(IWDG1)
             XX=SRED
             IF (KTML+LTML.EQ.3) XX=SRATEP(LTML)/SNOML
             write(*,*) "CURX",CURX
             write(*,*) "XX",XX
             write(*,*) "ACONDO",ACOND
             write(*,*) "FABOOS",FABOOS

             CURDEN(IWDG1)=AMAX1(CURDEN(IWDG1),
     &                           CURX/FABOOS(IWDG1)*XX/ACOND(IWDG1))
          END IF
       END IF
  899  CONTINUE
*
       MTML=KTML+LTML-2
*
C... Calculation of the winding dimensions

*
       IF (((ITAP+JTAP+MTML).EQ.3).AND.(.NOT.BBOUT)) THEN
        
          print *,'CALL WINDDI'
          CALL WINDDI
	
*....     Backtracking
          IF(ISTOP.EQ.1) RETURN
       END IF
*
C... Calculation of reactances
*
       print *,"OPCALC > UKX"
       CALL UKX(UK)
       UXC(ITAP2,JTAP2,MTML)=UK
*
C... Calculation of losses
*
       CALL LOSS
       PLOSS=PCU+PED+POED
*
C... Preparing of database-variables.
*
       PCUT(ITAP2,MTML)=PCU
       PEDT(ITAP2,MTML)=PED
       POEDT(ITAP2,MTML)=POED
       PCEOT(ITAP2,MTML)=PLOSS
       URC(ITAP2,JTAP2,MTML)=PLOSS
*
C... Calculation of maximum losses
*
C,,, If an exact calculation
*
       IF (BBEXAC) THEN
          BBLOSS=.FALSE.
          IF (PLOSS.GT.PLOMAX) BBLOSS=.TRUE.
          IF (BBLOSS) PLOMAX=PLOSS
*
C... For each winding
*
          DO 20 IWDG3=1,NWILI
          PCUEDW=PCUW(IWDG3)+PEDW(IWDG3)
*
C... Find the maximum value
*
          IF (WLOSSM(IWDG3).LT.PCUEDW) THEN
             WLOSSM(IWDG3)=PCUEDW
             WLOSSR(IWDG3)=PCUW(IWDG3)
             WLOSSE(IWDG3)=PEDW(IWDG3)
          END IF
          IF (BBLOSS) WLOSS(IWDG3)=PCUEDW
   20     CONTINUE
       END IF
*
C... Print the losses
*
       IF (BBOUT) CALL LOSSPR(ITAP2,JTAP2,KTML2,LTML2,MTML)
*
       BBUT2=.FALSE.
       IF (.NOT.( BBVR(LTML).OR.(BBVFR.AND.MTML.EQ.2))) BBUT2=.TRUE.
       JTAP=JTAP+1
       GOTO 600
  699  CONTINUE
*
       BBUT1=.FALSE.
       IF (.NOT.BBVR(KTML)) BBUT1=.TRUE.
       ITAP=ITAP+1
       GOTO 500
  599  CONTINUE
*
       LTML=LTML+1
       GOTO 400
  499  CONTINUE
*
       KTML=KTML+1
       GOTO 300
  399  write(*,*)"loss FLUXO",XZ2411
       write(*,*) "UXC=",UXC
       write(*,*) "URC=",URC
       CONTINUE
*
       print*,'coming out of opcalc'
       RETURN
       END
