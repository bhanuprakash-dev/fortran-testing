C*..* DRAG1
CDRAG1  DRAG1
C
C       WRITTEN  83-02-18  INGE
C
      SUBROUTINE DRAG1
     * (TYP,PTOT,QAKTIV,NDRAG,BDRAG,TDRAG,ALT,BEL,PRMAX)
C-----------------------------------------------------------
C
C...  BEST�MNING AV ANTAL DRAGSKENOR OCH DESS DIMENSIONER
C
      REAL NDRAG
      DIMENSION BELD(4)
      DATA BELD/4755.,9150.,11175.,11175./
C
C...  DIMENSIONERANDE BELASTNING (BEL)
      BEL=0.015*QAKTIV+0.9*PTOT
C
C...  KONTROLL ATT INTE MAX TILL*TEN BELASTNING �VERSKRIDES
      ITYP=TYP-6
      BELA=BELD(ITYP)
      PRMAX=0.
      IF(BEL.LE.BELA) GOTO 1590
      PRMAX=AMIN1(BELA,(BELA-QAKTIV*0.01)/0.75)
C
C...  BEST�M DRAGSKENEALTERNATIV
 1590 IDRAG=4
      GOTO(1601,1602,1603,1603),ITYP
 1601 IF(BEL.LE.3170.) IDRAG=3
      IF(BEL.LE.1580.) IDRAG=2
      IF(BEL.LE.1060.) IDRAG=1
      GOTO 1605
 1602 IF(BEL.LE.6100.) IDRAG=3
      IF(BEL.LE.3050.) IDRAG=2
      IF(BEL.LE.2040.) IDRAG=1
      GOTO 1605
 1603 IF(BEL.LE.7450.) IDRAG=3
      IF(BEL.LE.3730.) IDRAG=2
      IF(BEL.LE.2500.) IDRAG=1
 1605 CONTINUE
C
C...  DRAGSKENEDIMENSIONER
      GOTO(1610,1620,1630,1640),IDRAG
 1610 NDRAG=2
      BDRAG=75.
      TDRAG=10.
      GOTO 1700
 1620 NDRAG=2
      BDRAG=75.
      TDRAG=15.
      GOTO 1700
 1630 NDRAG=4
      BDRAG=75.
      TDRAG=15.
      GOTO 1700
 1640 NDRAG=6
      BDRAG=75.
      TDRAG=15.
 1700 CONTINUE
      ALT=IDRAG
C
      RETURN
      END