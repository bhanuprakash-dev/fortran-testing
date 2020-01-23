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
*      Subroutine RELNTA
*      -----------------
*
C...   Title:  Calculation of relative turn numbers for :
C...         1. Linear      regulation
C...         2. Plus-minus  regulation
C...         3. Coarse-fine regulation
C...      In accordance with the tapchangers in TI 5492 0450
C...      "ZTALWG" gives the relation between the voltage in
C...      one winding and the voltage of the corresponding
C...      winding group.
C...      "PLSPOS","ZERPOS" and "RMIPOS" give the part of the
C...      regulating winding that is connected in circuit.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE RELNTA(ITML,KTYPRW,NLOOPG,
     &                   NMSTEP,NPSTEP,NXSTEP,
     &                   PLSPOS,PUSTEP,RMIPOS,
     &                   ZERPOS,ZTALWG,EXTPOS)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL      PLSPOS(4),ZERPOS(4),RMIPOS(4),ZTALWG(3,4),PUSTEP(4),
     &           EXTPOS(3),X1,X2
*
       INTEGER   NLOOPG(4),NMSTEP(4),NPSTEP(4),NXSTEP(3),KTYPRW(4),
     &           ITML
*
       LOGICAL   BBL
*
CC*SEBL End of the declarations block.
*
       X1=FLOAT(NPSTEP(ITML)+NMSTEP(ITML))
*
C... Reg Type?
*
C,,, Linear
*
       IF (KTYPRW(ITML).EQ.1) THEN
          NLOOPG(ITML)=NPSTEP(ITML)+NMSTEP(ITML)
          ZERPOS(ITML)=FLOAT(NMSTEP(ITML))/X1
          PLSPOS(ITML)=1.
          RMIPOS(ITML)=0.
          EXTPOS(ITML)=FLOAT(NXSTEP(ITML))/X1
          ZTALWG(2,ITML)=X1*PUSTEP(ITML)
          ZTALWG(1,ITML)=1.-ZERPOS(ITML)*ZTALWG(2,ITML)
          ZTALWG(3,ITML)=0.
*
C... Plus-minus
*
       ELSE IF (KTYPRW(ITML).EQ.2) THEN
          NLOOPG(ITML)=2*INT((X1+3.1)/4.)
          IF (X1.LE.16.5) NLOOPG(ITML)=INT((X1+1.1)/2.)
          X1=FLOAT(NLOOPG(ITML))
*
          BBL=.FALSE.
          IF ((2.*X1-FLOAT(NPSTEP(ITML)+NMSTEP(ITML))).GT.1.5)BBL=.TRUE.
          IF (BBL) THEN
             PLSPOS(ITML)=1.-1./X1
          ELSE
             PLSPOS(ITML)=1.
          END IF
          ZERPOS(ITML)=PLSPOS(ITML)-FLOAT(NPSTEP(ITML))/X1
          RMIPOS(ITML)=ZERPOS(ITML)-FLOAT(NMSTEP(ITML))/X1
          EXTPOS(ITML)=
     &    PLSPOS(ITML)-FLOAT(NPSTEP(ITML)+NMSTEP(ITML)-NXSTEP(ITML))/X1
          ZTALWG(2,ITML)=X1*PUSTEP(ITML)
          ZTALWG(1,ITML)=1.-ZERPOS(ITML)*ZTALWG(2,ITML)
          ZTALWG(3,ITML)=0.
*
C... Coarse-fine
*
       ELSE IF (KTYPRW(ITML).EQ.3) THEN
          NLOOPG(ITML)=INT((X1+1.1)/2.)
          X1=FLOAT(NLOOPG(ITML))
          ZTALWG(2,ITML)=X1*PUSTEP(ITML)
*
C... Set X2
*
C,,, X1 < 8.5
*
          IF (X1.LT.8.5) THEN
             X2=1.+X1
*
C... X1 >= 8.5
*
          ELSE
             X2=2.*AINT((X1+1.1)/2.)
          END IF
*
          PLSPOS(ITML)=1.+FLOAT(NMSTEP(ITML)+NPSTEP(ITML))/X1
          ZTALWG(3,ITML)=X2*PUSTEP(ITML)
          BBL=.FALSE.
          IF ((X2-X1).GT.0.5) BBL=.TRUE.
*
C... Set RMIPOS(ITML)
*
C,,, BBL=.TRUE.
*
          IF (BBL) THEN
             RMIPOS(ITML)=1./X1
*
C... BBL=.FALSE.
*
          ELSE
             RMIPOS(ITML)=0.
          END IF
*
C... NMSTEP
*
C,,, NMSTEP > (X1-0.5)
*
          IF (FLOAT(NMSTEP(ITML)).GT.(X1-0.5)) THEN
             ZERPOS(ITML)=1.+FLOAT(NMSTEP(ITML))/X1
             ZTALWG(1,ITML)=
     &          1.-ZTALWG(3,ITML)-ZTALWG(2,ITML)*(ZERPOS(ITML)-2.)
*
C... NMSTEP <= (X1-0.5)
*
          ELSE
             ZERPOS(ITML)=RMIPOS(ITML)+FLOAT(NMSTEP(ITML))/X1
             ZTALWG(1,ITML)=1.-ZERPOS(ITML)*ZTALWG(2,ITML)
          END IF
*
C... NXSTEP
*
C,,, NXSTEP > (X2-0.5)
*
          IF (FLOAT(NXSTEP(ITML)).GT.(X2-0.5)) THEN
             EXTPOS(ITML)=1.          +FLOAT(NXSTEP(ITML))/X1
*
C... NXSTEP <= (X2-0.5)
*
          ELSE
             EXTPOS(ITML)=RMIPOS(ITML)+FLOAT(NXSTEP(ITML))/X1
          END IF
       END IF
*
       RETURN
       END
