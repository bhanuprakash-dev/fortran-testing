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
*      Subroutine CHECK
*      ----------------
*
C...   Title:  Checks certain indata which have different
C...           limitations for the different USERID's.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-09-09 by B-G Bladh  , SETFO/TS
*        FONAN > 1 introduced (i.e. Rated power = ONAN-power)
************************************************************************
*
       SUBROUTINE CHECK
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ0181(1)    ,  BARR   (1)    ),
     &  (XZ0231(1)    ,  DARR   (1)    ),
     &  (XZ0301(1)    ,  IARR   (1)    ),
     &  (XZ0401(1)    ,  TARR   (1)    )
       EQUIVALENCE
     &  (XZ1215(1)    ,  KCON   (1)    ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1499       ,  NG            ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1545       ,  SQR3          )
*
       REAL      AARR(200),BARR(100),DARR(100),Y(20),CURTML(4),
     &           FONAN,SQR3,SRAT,XCON
*
       INTEGER   IARR(100),KCON(4),ITEM,IWDG,JFC,JTML,LCON,NG,NWILI,
     &           ISTOP
*
       DIMENSION TARR(100)
       CHARACTER TARR*8
*
       LOGICAL   BBSTOP,BBHELP(10)
*
CC*SEBL End of the declarations block.
*
C... Check initialisations
*
       DO 299 ITEM=1,20
  299  Y(ITEM)=0.
*
C... Terminal initialisations
*
       DO 399 JTML=1,4
  399  CURTML(JTML)=0.
*
C... Power ratings
*
CC*SBBL Start of the power ratings block.
*
       FONAN=AARR(154)
       IF(FONAN.GT.3.) FONAN=FONAN/100.
       SRAT=AMAX1(AARR(11),AARR(12),AARR(13),AARR(14))
       IF (FONAN.LE.1) THEN
          Y(1)=SRAT*FONAN
          Y(2)=SRAT
          Y(8)=(-1.)*SRAT
       ELSE
          Y(1)=SRAT
          Y(2)=SRAT*FONAN
          Y(8)=(-1.)*Y(2)
       ENDIF
*
C... Set power ratings
*
C,,, If not 3-phase
*
      IF(IARR(1).NE.3) THEN
        IF(FONAN .LE. 1) THEN
           Y(9)=SRAT
           Y(10)=SRAT*FONAN
        ELSE
           Y(9)=SRAT*FONAN
           Y(10)=SRAT
        ENDIF
      ENDIF
*
CC*SEBL End of the power ratings block.
*
C... Maximum winding currents
*
       DO 599 JTML=1,NG
*
C--- KCON  I=0, Y=1, D=3,  IAUTO=10, YAUTO=11
*
       LCON=MOD(KCON(JTML),10)
       XCON=FLOAT(LCON)
       CURTML(JTML)=AARR(10+JTML)/AARR(15+JTML)*1000./FLOAT(IARR(51))
       IF(LCON.NE.0)  CURTML(JTML) = CURTML(JTML) * (SQR3/SQRT(XCON))
       IF(FONAN.GT.1) CURTML(JTML) = CURTML(JTML) * FONAN
  599  CONTINUE
*
C... Auto-connected transformers
*
CC*SBBL Start of the auto-transformer block.
*
       IF (KCON(1).GT.9.AND.AARR(15+JTML).LT.AARR(16+JTML))
     &    CURTML(1)=CURTML(1)-CURTML(2)
       IF (KCON(2).GT.9.AND.AARR(15+JTML).GT.AARR(16+JTML))
     &    CURTML(2)=CURTML(2)-CURTML(1)
       Y(3)=AMAX1(CURTML(1),CURTML(2),CURTML(3),CURTML(4))
*
CC*SEBL End of the auto-transformer block.
*
C... Line voltage
*
CC*SBBL Start of the line voltage block.
*
       Y(4)=AMAX1(AARR(16),AARR(17),AARR(18),AARR(19))
*
CC*SEBL End of the line voltage block.
*
C... BIL - value
*
CC*SBBL Start of the BIL block.
*
       Y(5)=AMAX1(AARR(26),AARR(27),AARR(28),AARR(29))
*
CC*SEBL End of the BIL block.
*
C... Oil-guided cooling
*
CC*SBBL Start of the BIL block.
*
C... Set oil-guided cooling
*
       DO 699 IWDG=1, NWILI
  699  IF(DARR(IWDG).GT.0.1) Y(6)=4.
*
CC*SEBL End of the BIL block.
*
C... Winding supports
*
CC*SBBL Start of the winding support block.
*
       IF(BARR(30).GT.1.E-6) Y(11)=4.
*
CC*SEBL End of the winding support block.
*
C... Side-limbed cores
*
CC*SBBL Start of the side-limbed cores block.
*
       IF(TARR(51)(1:1).EQ.'Y') Y(12)=4.
*
CC*SEBL End of the side-limbed cores block.
*
C... HI-B core-steel
*
CC*SBBL Start of the HI-B core steel block.
*
       IF(TARR(1 )(1:1).EQ.'Y') Y(13)=4.
*
CC*SEBL End of the HI-B core steel block.
*
C... ASECOND core-banding
*
CC*SBBL Start of the ASECOND block.
*
       IF(TARR(80).EQ.'ASEC    ') Y(14)=4.
*
CC*SEBL End of the ASECOND block.
*
C... Check the limitations for the particular UDB
*
CC*SBBL Start of the Check block.
*
       IF(BBHELP(5)) PRINT *,'USER-LIMITS',Y
*
C... Check the limitations
*
       CALL ICHECK( Y,JFC,BBSTOP)
*
CC*SEBL End of the Check block.
*
C... Check whether to stop or not
*
       IF(BBSTOP)STOP
*    &   ISTOP = 1
*
  199  RETURN
       END
