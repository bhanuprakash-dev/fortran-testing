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
*      Subroutine SCSTRE
*      -----------------
*
C...   Title: Calculation of actual short-circuit stresses
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE SCSTRE(I,J,K,BBOUT)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ1021(1)    ,  SHTCUR (1)    ),
     &  (XZ1041(1)    ,  STRWMM (1)    )
       EQUIVALENCE
     &  (XZ1051(1)    ,  STRWMP (1)    ),
     &  (XZ1131(1)    ,  X10    (1)    ),
     &  (XZ1141(1)    ,  X20    (1)    ),
     &  (XZ1151(1)    ,  X30    (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    )
       EQUIVALENCE
     &  (XZ1211(1)    ,  EXTX   (1)    ),
     &  (XZ1271(1,1)  ,  UN     (1,1)  ),
     &  (XZ1291(1)    ,  XRINT  (1)    ),
     &  (XZ1341(1)    ,  BBRW   (1)    ),
     &  (XZ1360(1)    ,  BBSCL  (1)    )
       EQUIVALENCE
     &  (XZ1363(1)    ,  BBSCR  (1)    ),
     &  (XZ1373       ,  BBAUTO        ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1476       ,  HWINDM        ),
     &  (XZ1499       ,  NG            )
       EQUIVALENCE
     &  (XZ1500       ,  NPG           ),
     &  (XZ1504       ,  NSG           ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1513       ,  PI            ),
     &  (XZ1538       ,  SCCONS        )
       EQUIVALENCE
     &  (XZ1542       ,  SNOML         ),
     &  (XZ1647(1)    ,  NPERM  (1)    ),
     &  (XZ1656(1,1)  ,  POSIT  (1,1)  ),
     &  (XZ1690(1,1,1),  UXC    (1,1,1)),
     &  (XZ2733(1)    ,  BBOOSW (1)    ),
     &  (XZ2730       ,  TRBOOS        ) 
*
       REAL      UXBR(3), UXC(4,4,3),
     &           ACOND(9),DWIND(9),EXTX(3),POSIT(9,4),SHTCUR(9),
     &           STRWMM(9),STRWMP(9),UN(4,4),X10(9),X20(9),X30(9),
     &           XRINT(3),ZWIND(9),RISC(3),FW0,HWINDM,PI,RIDENS,
     &           SCCONS,SNOML,X,X1,X2,X3,X4,X5,Y, TRBOOS
*
       INTEGER   KGROUP(9),LPERM(5),NPERM(3),I,IWDG,I1,J,JTML,JTML0,
     &           JTML1,JTML2,K,MAXTML,NG,NPG,NSG,NWILI,N1,N2
*
       LOGICAL   BBAUTO,BBOUT,BBRW(9),BBSCL(4),BBSCR(4),BBVFR
       LOGICAL      BBOOSW(9)
*
CC*SEBL End of the declarations block.
*
       DATA  LPERM /1, 2, 3, 1, 2 /
*
C... UK
*
CC*SBBL Start of Abbreviations
*
       X1=UXC(I,J,1)
       X2=UXC(I,K,2)
       X3=UXC(J,K,3)
*
CC*SEBL End of Abbreviations
*
C... Total impedance per branch:
*
CC*SBBL Start of Tot Imp/branch
*
       UXBR(1)=(X1+X2-X3)/2.+XRINT(1)
       UXBR(2)=(X1+X3-X2)/2.+XRINT(2)
       UXBR(3)=(X2+X3-X1)/2.+XRINT(3)
*
CC*SEBL End of Tot Imp/branch
*
C... Tap-changer position
*
CC*SBBL Start of Tap.chgr position
*
       NPERM(1)=I
       NPERM(2)=J
       NPERM(3)=K
*
CC*SEBL End of Tap.chgr position
*
C... "NPERM" denotes the actual tap-position for each winding group
*
       MAXTML=MIN0(NG,3)
       DO 299 JTML=1,MAXTML
*
C... Calculation of short-circuit currents for the terminal
C... branches, "ISC"
C... "ISC(I)" where "JTML" is the short-circuited terminal and
C...                "I" is the terminal with the actual current.
C... p.u.-values are used
*
C,,, If BBSCL(ITML)
*
       IF (BBSCL(JTML)) THEN
          JTML1=LPERM(JTML+1)
          JTML2=LPERM(JTML+2)
          X1=UXBR(JTML1)+EXTX(JTML1)
          X2=UXBR(JTML2)+EXTX(JTML2)
          Y=1.
          X3=X2
          RISC(JTML)=0.
          RISC(JTML1)=0.
          IF(BBSCR(JTML2).AND.BBSCR(JTML1)) Y=X2/(X1+X2)
          IF(BBSCR(JTML1)) X3=X1*Y
          IF(BBSCR(JTML1).OR.BBSCR(JTML2)) RISC(JTML)=1./(UXBR(JTML)+X3)
          IF (BBSCR(JTML1)) RISC(JTML1)=-RISC(JTML)*Y
          RISC(JTML2)=-RISC(JTML)-RISC(JTML1)
*
C... By using "RISC" the physical current densities "RIDENS" and
C... magnetomotoric forces "FW0" will be calculated for the whole
C... winding arrangement for the actual short-circuiut case starting
C... with winding "A". When "RIDENS" and "FW0"  are known the inside &
C... outside stresses will be calculated as "X2" and "X3" respectively.
C... If now the mean value of "X2" and "X3" exceeds
C...         "STRWMP" if positive
C...     or  "STRWMM" if negative
C... then new values for "STRWMM" and "STRWMP" respectively
C... will be inserted.
*
          FW0=0.
*
C... For each winding
*
          DO 499  IWDG=1,NWILI
          N1=KGROUP(IWDG)
*
C... Calculations
*
C,,, If N1 < 3
*
          IF (N1.LE.3) THEN
             N2=NPERM(N1)
             I1=N2
             IF (BBVFR) I1=J
             X4=POSIT(IWDG,I1)

C... Rev 94-04-12 Include Booster factor in calculation if Booster winding

	     IF(.NOT. BBOOSW(IWDG)) THEN
                X=SNOML*RISC(N1)/UN(N1,N2)
	     ELSE
                X=SNOML*RISC(N1)/UN(N1,N2)/TRBOOS
	     ENDIF
*
C... Calculate X
*
C,,, If Auto and N1 <> NPG
*
             IF (BBAUTO.AND.N1.EQ.NPG) THEN
                IF (BBVFR.OR..NOT.BBRW(IWDG))
     &             X=X+SNOML*RISC(NSG)/UN(NSG,NPERM(NSG))
             END IF
*
             RIDENS=0.
             IF(X4.NE.0.) RIDENS=X/ACOND(IWDG)*SIGN(1.,X4)
             X5=RIDENS*SCCONS*PI/HWINDM
             X1=-FW0*DWIND(IWDG)*X5
             FW0=FW0+X*ZWIND(IWDG)*X4
             X2=-FW0*DWIND(IWDG)*X5
             X3=(X1+X2)/2.
             IF (X3.LT.STRWMM(IWDG)) STRWMM(IWDG)=X3
             IF (X3.GT.STRWMP(IWDG)) STRWMP(IWDG)=X3
*
C... Allocate values to certain variables
*
C,,, If BBOUT
*
             IF(BBOUT) THEN
                X10(IWDG)=X1
                X20(IWDG)=X2
                X30(IWDG)=X3
                SHTCUR(IWDG)=X
             END IF
          END IF
  499     CONTINUE
*
C... Allocate values to certain variables
*
C,,, If BBOUT
*
          IF (BBOUT) THEN
             JTML0=JTML
*
C... Print out the values
*
C,,, If NG <= 2
*
             IF (NG.LE.2) THEN
*
C... Print out the table
*
                CALL TABLE1(JTML0,3-JTML0,3)
*
C... If NG > 2
*
             ELSE
*
C... Print out the table
*
                CALL TABLE1(JTML0,JTML1,JTML2)
             END IF
          END IF
       END IF
  299  CONTINUE
*
       RETURN
       END
