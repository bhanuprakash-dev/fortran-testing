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
*      Subroutine TURNCA
*      -----------------
*
C...   Title: Calculate the relative turn-numbers (NTAL) for
C...          each winding. Winding 1 turn-number gives the
C...          primary limb-voltage.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE TURNCA
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0801(1)    ,  FRACT  (1)    ),
     &  (XZ0821(1)    ,  FRMPOS (1)    ),
     &  (XZ0831(1)    ,  FRPPOS (1)    ),
     &  (XZ0841(1)    ,  FRZPOS (1)    ),
     &  (XZ0891(1)    ,  KCODE  (1)    )
       EQUIVALENCE
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ0931(1)    ,  NLOOP  (1)    ),
     &  (XZ1171(1)    ,  ZTALMW (1)    ),
     &  (XZ1181(1)    ,  ZTALRW (1)    )
       EQUIVALENCE
     &  (XZ1219(1)    ,  KTYPRW (1)    ),
     &  (XZ1223(1)    ,  NLOOPG (1)    ),
     &  (XZ1227(1)    ,  NMSTEP (1)    ),
     &  (XZ1231(1)    ,  NPSTEP (1)    ),
     &  (XZ1235(1)    ,  PLSPOS (1)    )
       EQUIVALENCE
     &  (XZ1239(1)    ,  PUSTEP (1)    ),
     &  (XZ1251(1)    ,  RMIPOS (1)    ),
     &  (XZ1267(1)    ,  UDIM   (1)    ),
     &  (XZ1295(1)    ,  ZERPOS (1)    ),
     &  (XZ1366(1)    ,  BBVR   (1)    )
       EQUIVALENCE
     &  (XZ1373       ,  BBAUTO        ),
     &  (XZ1392       ,  BBUPTR        ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1656(1,1)  ,  POSIT  (1,1)  ),
     &  (XZ1767(1)    ,  ZTALWG (1,1)  ),
     &  (XZ3001(1)    ,  EXTPOS (1)    ),
     &  (XZ3004(1)    ,  FRXPOS (1)    ),
     &  (XZ3010(1)    ,  NXSTEP (1)    )
*
       REAL      EXTPOS(3),FRACT(9),POSIT(9,4),UDIM(4),PUSTEP(4),
     &           PLSPOS(4),ZERPOS(4),RMIPOS(4),ZTALWG(3,4),
     &           FRZPOS(9),FRPPOS(9),FRMPOS(9),FRXPOS(6),
     &           ZTALMW(9),ZTALRW(9),X,X1,X2,X3
*
       INTEGER   KGROUP(9),NLOOPG(4),ITML,ITML1,ITML11,ITML2,ITML3,
     &           NMSTEP(4),NPSTEP(4),KTYPRW(4),NLOOP(9),KCODE(9),
     &           ITAP,IWDG,JTML,KTAP,NG,NWILI,NXSTEP(3)
*
       LOGICAL   BBAUTO,BBVR(4),BBUPTR,BBVFR
*
CC*SEBL End of the declarations block.
*
C... Start values (NLOOP,NLOOPG,ZERPOS,PLSPOS,RMIPOS,ZTALWG)
*
CC*SBBL Start of the start values block.
*
C... Set EXTPOS = 0 for each terminal
*
       DO 11 ITML=1,3
   11  EXTPOS(ITML)=0.
*
C... Set tap positions,etc = 0 for each terminal
*
       DO 5 ITML=1,4
       NLOOPG(ITML)=1
       ZERPOS(ITML)=0.
       PLSPOS(ITML)=0.
       RMIPOS(ITML)=0.
*
C... Set turns = 0 for each tap
*
       DO 5 KTAP=1,3
    5  ZTALWG(KTAP,ITML)=0.
*
C... Set loops = 1 for each winding
*
       DO 15 IWDG=1,NWILI
   15  NLOOP(IWDG)=1
*
CC*SEBL End of the start values block.
*
C... Select FULL or AUTO transformer
*
C,,, FULL trfr
*
       IF (.NOT.BBAUTO) THEN
*
C... VFR type voltage regulation
*
C,,, VFR
*
          IF (BBVFR) THEN
             NPSTEP(1)=NMSTEP(2)
             X1=FLOAT(NMSTEP(2))
             NMSTEP(1)=NPSTEP(2)
             X2=FLOAT(NPSTEP(2))
             PUSTEP(1)=1./(X2-X1+1./PUSTEP(2))
             KTYPRW(1)=KTYPRW(2)
             ZTALWG(1,2)=1.
             ITML=1
*
C... Calculate relative turns
*
             CALL RELNTA(ITML,KTYPRW,NLOOPG,
     &                   NMSTEP,NPSTEP,NXSTEP,
     &                   PLSPOS,PUSTEP,RMIPOS,
     &                   ZERPOS,ZTALWG,EXTPOS)
             X1=(1.-X2*X1*PUSTEP(2)*PUSTEP(1))*UDIM(2)/UDIM(1)
*
C... CFR
*
          ELSE
*
C... For terminals 1 & 2
*
             DO 2799 ITML=1,2
             ITML1=ITML
*
C... Calculate relative turns
*
             IF (     BBVR(ITML)) CALL RELNTA(ITML1,KTYPRW,NLOOPG,
     &                                        NMSTEP,NPSTEP,NXSTEP,
     &                                        PLSPOS,PUSTEP,RMIPOS,
     &                                        ZERPOS,ZTALWG,EXTPOS)
             IF (.NOT.BBVR(ITML)) ZTALWG(1,ITML)=1.
 2799        CONTINUE
             X1=UDIM(2)/UDIM(1)
          END IF
*
C... For tap postions 1 to 3
*
          DO 499 ITAP=1,3
  499     ZTALWG(ITAP,2)=X1*ZTALWG(ITAP,2)
*
C... PART 2, Auto-connected transformers
*
       ELSE
*
C... Auto-connected transformers
*
C,,, Voltage regulation type CFR
*
          IF (.NOT.BBVFR) THEN
*
C... Auto-connected transformers, voltage regulation type CFR
*
             DO 699 ITML=1,2
             ITML1=ITML
*
C... Calculate relative turns
*
             IF (     BBVR(ITML)) CALL RELNTA(ITML1,KTYPRW,NLOOPG,
     &                                        NMSTEP,NPSTEP,NXSTEP,
     &                                        PLSPOS,PUSTEP,RMIPOS,
     &                                        ZERPOS,ZTALWG,EXTPOS)
             IF (.NOT.BBVR(ITML)) ZTALWG(1,ITML)=1.
  699        CONTINUE
             X1=UDIM(2)/UDIM(1)
*
C... For tap postions 1 to 3
*
             DO 799 ITAP=1,3
  799        ZTALWG(ITAP,2)=X1*ZTALWG(ITAP,2)
             IF (     BBUPTR) ZTALWG(1,2)=ZTALWG(1,2)-ZTALWG(1,1)
             IF (.NOT.BBUPTR) ZTALWG(1,1)=ZTALWG(1,1)-ZTALWG(1,2)
*
C... Voltage regulation type VFR
*
          ELSE
*
C... Calculate as required
*
C,,, For step-up transformer
*
             IF (BBUPTR) THEN
*
                X1=UDIM(1)/UDIM(2)
                X3=1./(1.+FLOAT(NPSTEP(2)-NMSTEP(2))*PUSTEP(2)-X1)
                ZTALWG(1,2)=X3*(1.-FLOAT(NMSTEP(2))*PUSTEP(2)-X1)*
     &           ((1.+FLOAT(NPSTEP(2))*PUSTEP(2))/X1-1.)
                NPSTEP(1)=NMSTEP(2)
                X2=FLOAT(NMSTEP(2))
                NMSTEP(1)=NPSTEP(2)
                X1=FLOAT(NPSTEP(2))
                PUSTEP(1)=PUSTEP(2)*X3
                KTYPRW(1)=KTYPRW(2)
                ITML2=1
*
C... Calculate relative turns
*
                CALL RELNTA(ITML2,KTYPRW,NLOOPG,
     &                      NMSTEP,NPSTEP,NXSTEP,
     &                      PLSPOS,PUSTEP,RMIPOS,
     &                      ZERPOS,ZTALWG,EXTPOS)
*
C,,, For NON step-up transformer
*
             ELSE
                X=UDIM(2)/UDIM(1)
                X1=FLOAT(NPSTEP(2))*PUSTEP(2)
                X2=FLOAT(NMSTEP(2))*PUSTEP(2)
                X3=1./(1.+(X1*X2+X2-X1-1.)*X)
                PUSTEP(2)=PUSTEP(2)*X3
                ITML3=2
*
C... Calculate relative turns
*
                CALL RELNTA(ITML3,KTYPRW,NLOOPG,
     &                      NMSTEP,NPSTEP,NXSTEP,
     &                      PLSPOS,PUSTEP,RMIPOS,
     &                      ZERPOS,ZTALWG,EXTPOS)
                PUSTEP(2)=PUSTEP(2)/X3
                X=1./(1./X-X1*X2*X3)
*
C... For tap postions 1 to 3
*
                DO 999 KTAP=1,3
  999           ZTALWG(KTAP,2)=X*ZTALWG(KTAP,2)
                IF (ZERPOS(2).GT.1.5) X1=ZERPOS(2)-2.
                IF (ZERPOS(2).GT.1.5) X2=1.
                IF (ZERPOS(2).LE.1.5) X1=ZERPOS(2)
                IF (ZERPOS(2).LE.1.5) X2=0.
                ZTALWG(1,1)=1.-ZTALWG(1,2)-ZTALWG(2,2)*X1-ZTALWG(3,2)*X2
             END IF
          END IF
       END IF
*
C... NTAL for tertiary and extra windings
*
C,,, If more than 2 terminals
*
       IF (NG.GE.3) THEN
*
C,,, For terminals 3 and 4
*
          DO 1199 ITML=3,NG
          ITML11=ITML
*
C... Calculate relative turns
*
          IF (     BBVR(ITML)) CALL RELNTA(ITML11,KTYPRW,NLOOPG,
     &                                     NMSTEP,NPSTEP,NXSTEP,
     &                                     PLSPOS,PUSTEP,RMIPOS,
     &                                     ZERPOS,ZTALWG,EXTPOS)
          IF (.NOT.BBVR(ITML)) ZTALWG(1,ITML)=1.
          X1=UDIM(ITML)/UDIM(1)
*
C... For tap postions 1 to 3
*
          DO 1299 KTAP=1,3
 1299     ZTALWG(KTAP,ITML)=X1*ZTALWG(KTAP,ITML)
 1199     CONTINUE
       END IF
*
C... Transfer NTAL for each group
C... to a sequential order from the core
C... as they appear in the transformer
*
       DO 2099 IWDG=1,NWILI
       JTML=KGROUP(IWDG)
       ZTALMW(IWDG)=0.
       ZTALRW(IWDG)=0.
       NLOOP(IWDG)=1
*
C... Set tap postion variables
*
C,,, If KCODE(IWDG) = 1
*
       IF (KCODE(IWDG).EQ.1) THEN
*
          FRZPOS(IWDG)=1.
          FRPPOS(IWDG)=1.
          FRMPOS(IWDG)=1.
          FRXPOS(IWDG)=1.
          ZTALMW(IWDG)=ZTALWG(1,JTML)
*
C... If KCODE(IWDG) <> 1
*
       ELSE
*
C... Set ZTALRW
*
C,,, If KCODE(IWDG) <> 2 and <> 4
*
          IF (KCODE(IWDG).NE.2.AND.KCODE(IWDG).NE.4) THEN
             ZTALRW(IWDG)=ZTALWG(3,JTML)
*
C... If KCODE(IWDG) = 2 or 4
*
          ELSE
             ZTALRW(IWDG)=ZTALWG(2,JTML)
             NLOOP(IWDG)=NLOOPG(JTML)
          END IF
*
C... Set tap position variables
*
C,,, If KTYPRW(JTML) = 3
*
          IF(KTYPRW(JTML).EQ.3) THEN
*
C... Set tap position variables
*
C,,, If KCODE(IWDG) = 2
*
             IF (KCODE(IWDG).EQ.2) THEN
                FRMPOS(IWDG)=RMIPOS(JTML)
                FRZPOS(IWDG)=ZERPOS(JTML)
                IF (ZERPOS(JTML).GT.1.5) FRZPOS(IWDG)=ZERPOS(JTML)-2.
                FRPPOS(IWDG)=PLSPOS(JTML)-2.
                FRXPOS(IWDG)=EXTPOS(JTML)
                IF (EXTPOS(JTML).GT.1.5) FRXPOS(IWDG)=EXTPOS(JTML)-2.
*
C... If KCODE(IWDG) <> 2
*
             ELSE
                FRMPOS(IWDG)=0.
                FRZPOS(IWDG)=0.
                IF (ZERPOS(JTML).GT.1.5) FRZPOS(IWDG)=1.
                FRPPOS(IWDG)=1.
                FRXPOS(IWDG)=0.
                IF (EXTPOS(JTML).GT.1.5) FRXPOS(IWDG)=1.
             END IF
*
C... If KTYPRW(JTML) <> 3
*
          ELSE
             FRZPOS(IWDG)=ZERPOS(JTML)
             FRPPOS(IWDG)=PLSPOS(JTML)
             FRMPOS(IWDG)=RMIPOS(JTML)
             FRXPOS(IWDG)=EXTPOS(JTML)
          END IF
*
          IF(KCODE(IWDG).EQ.4) ZTALMW(IWDG)=ZTALWG(1,JTML)
       END IF
*
C... Set tap position variables
*
C... If VFR and terminal 1
*
       IF (BBVFR.AND.JTML.EQ.1) THEN
          X1=FRPPOS(IWDG)
          FRPPOS(IWDG)=FRMPOS(IWDG)
          FRMPOS(IWDG)=X1
       END IF
 2099  CONTINUE
*
C... For each winding
*
       DO 2699 IWDG=1,NWILI
       POSIT(IWDG,1)=FRZPOS(IWDG)
       POSIT(IWDG,2)=FRPPOS(IWDG)
       POSIT(IWDG,3)=FRMPOS(IWDG)
       POSIT(IWDG,4)=FRXPOS(IWDG)
 2699  CONTINUE

*
       RETURN
       END
