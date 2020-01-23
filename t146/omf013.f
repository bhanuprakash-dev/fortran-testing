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
*      Subroutine OMF013
*      -----------------
*
C...   Title:  Search for a common node for an auto-transformer
C...           and allocate terminal numbers to each winding
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-12-03 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-09-17 by B-G Bladh  , SETFO/TS
*KWIFUN = 'RB' and 'RCB' introduced (booster)
*
************************************************************************
*
       SUBROUTINE OMF013(NG,NWILI,WNOD1,WNOD2,TNODE,KCON,UNLINE,
     &                   KGROUP,KWIFUN,KCODE)
*
       REAL      UNLINE(*)
*
       INTEGER   KGROUP(*),KCODE(*),KHELP(9),INDEX(4),IFTYPE(9),
     &           I,IEND1,IEND2,IGROUP,IND,ITERML,IWDG,JITEM,JJ,JTYPE,
     &           JWDG,KK,KWDG,LWDG,NG,NWILI,NWIT,NWNG
*
       DIMENSION ITEST(9)
       CHARACTER WNOD1(*)*4,WNOD2(*)*4,TNODE(*)*4,KWIFUN(*)*4,
     &           WNODS1*4,WNODS2*4,WNOD*4,KCON(*)*4,ITEST*3
*
      DATA ITEST/'M  ','R  ','RC ','MV ','S  ','R1 ','R2 ','RB ','RCB'/,
     &    IFTYPE/ 1   , 2   , 3   , 4   , 5   , 2   , 2   , 2   , 3   /,
     &      INDEX / 1 , 2 , 3  , 4 /
*
       WNOD=' '
*
       DO 22 I=1,9
       KGROUP(I)=0
   22  KCODE (I)=0
*
       IF((KCON(1).NE.'I/A'.AND.KCON(1).NE.'Y/A').OR.
     &    (KCON(2).NE.'I/A'.AND.KCON(2).NE.'Y/A')) GO TO 200
       DO 150 IWDG=1,NWILI
       IEND1=0
       IEND2=0
       IF (    WNOD1(IWDG).EQ.TNODE(1)
     &     .OR.WNOD1(IWDG).EQ.TNODE(2)) IEND1=1
       IF (    WNOD2(IWDG).EQ.TNODE(1)
     &     .OR.WNOD2(IWDG).EQ.TNODE(2)) IEND2=1
       DO 140 JWDG=1,NWILI
       IF (JWDG.EQ.IWDG) GO TO 140
       IF    (WNOD1(IWDG).NE.WNOD1(JWDG)
     &   .AND.WNOD1(IWDG).NE.WNOD2(JWDG)) GOTO 120
       IEND1=IEND1+1
       WNODS1=WNOD1(IWDG)
  120  IF    (WNOD2(IWDG).NE.WNOD1(JWDG)
     &   .AND.WNOD2(IWDG).NE.WNOD2(JWDG)) GOTO 140
       IEND2=IEND2+1
       WNODS2=WNOD2(IWDG)
  140  CONTINUE
       IF (IEND1.EQ.2) WNOD=WNODS1
       IF (IEND2.EQ.2) WNOD=WNODS2
       IF (IEND1.EQ.2.OR.IEND2.EQ.2) GO TO 160
  150  CONTINUE
  160  CONTINUE
*
C... Sort terminal nodes 1 & 2 so the series winding is treated first
*
       IF (UNLINE(2).LT.UNLINE(1)) GO TO 200
       INDEX(1)=2
       INDEX(2)=1
  200  CONTINUE
*
       NWIT=0
       DO 100 IGROUP=1,NG
       ITERML=INDEX(IGROUP)
       DO 1 JWDG=1,NWILI
       JJ=JWDG
       IF (KGROUP(JWDG).NE.0) GO TO 1
       IF(WNOD1(JWDG).EQ.TNODE(ITERML)) GOTO 2
       IF(WNOD2(JWDG).EQ.TNODE(ITERML)) GOTO 2
    1  CONTINUE
    2  NWIT=1
       KHELP(1)=JJ
       KGROUP(JJ)=ITERML
       IF (IGROUP.EQ.1.AND.(     WNOD1(JJ).EQ.WNOD
     &                      .OR. WNOD2(JJ).EQ.WNOD)) GO TO 100
       NWNG=NWILI-NG
       DO 599 JITEM=1,NWNG
       DO 3 KWDG=1,NWILI
       IF(KGROUP(KWDG).NE.0) GO TO 3
       IND=KHELP(JITEM)
       KK=KWDG
       IF   (WNOD1(IND).EQ.WNOD1(KWDG)
     &   .OR.WNOD1(IND).EQ.WNOD2(KWDG)) GOTO 4
       IF   (WNOD2(IND).EQ.WNOD2(KWDG)
     &   .OR.WNOD2(IND).EQ.WNOD1(KWDG)) GOTO 4
    3  CONTINUE
       GOTO 59
    4  NWIT=NWIT+1
       KHELP(NWIT)=KK
       KGROUP(KK)=ITERML
   59  IF(NWIT.EQ.JITEM) GOTO 100
       IF (IGROUP.EQ.1.AND.(    WNOD1(KK).EQ.WNOD
     &                      .OR.WNOD2(KK).EQ.WNOD)) GO TO 100
  599  CONTINUE
  100  CONTINUE
       DO 20 LWDG=1,NWILI
       KCODE(LWDG)=0
       DO 21 JTYPE=1,9
       IF(KWIFUN(LWDG).EQ.ITEST(JTYPE)) KCODE(LWDG)=IFTYPE(JTYPE)
   21  CONTINUE
   20  CONTINUE
       RETURN
       END
