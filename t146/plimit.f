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
*      Subroutine PLIMIT
*      -----------------
*
C...   Title:  Print out of those limits which have
C...           been reached or exceeded
*
*      Written: 87-03-06 by H.H¢glund  , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PLIMIT(IALT)
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0651(1)    ,  ACOND  (1)    )
       EQUIVALENCE
     &  (XZ0711(1)    ,  CURDM  (1)    ),
     &  (XZ0731(1)    ,  CURRUT (1)    ),
     &  (XZ0971(1)    ,  PRESSM (1)    ),
     &  (XZ1041(1)    ,  STRWMM (1)    ),
     &  (XZ1051(1)    ,  STRWMP (1)    )
       EQUIVALENCE
     &  (XZ1081(1)    ,  TENSM  (1)    ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1405       ,  BLIMB         ),
     &  (XZ1406       ,  BLTOPM        ),
     &  (XZ1407       ,  BMAXPU        ),
     &  (XZ1410       ,  BTANK         )
       EQUIVALENCE
     &  (XZ1411       ,  BTANKM        ),
     &  (XZ1424       ,  DCOMAX        ),
     &  (XZ1425       ,  DCOMIN        ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1466       ,  GTRP          )
       EQUIVALENCE
     &  (XZ1467       ,  GTRPM         ),
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1473       ,  HLMBMA        ),
     &  (XZ1474       ,  HTANK         ),
     &  (XZ1475       ,  HTANKM        )
       EQUIVALENCE
     &  (XZ1486       ,  JFC           ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1516       ,  PLOADM        ),
     &  (XZ1520       ,  PNOLOM        )
       EQUIVALENCE
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1529       ,  RLTNKM        ),
     &  (XZ1543       ,  SOUNDM        ),
     &  (XZ1561       ,  USHORE        ),
     &  (XZ1571       ,  ZWOULI        )
       EQUIVALENCE
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1686(1)    ,  SOUND0 (1)    ),
     &  (XZ1689(1,1,1),  URC    (1,1,1))
*
       REAL      TUSEN,TUSTUS,QUOTE(50,2),CURDM(9),BMAXPU,
     &           URC(4,4,3),SOUND0(3),PRESSM(9),TENSM(9),STRWMM(9),
     &           STRWMP(9),P00(3),ACOND(9),CURRUT(9),CURDUT(9),
     &           BLIMB,BLTOPM,BTANK,BTANKM,DCOMAX,DCOMIN,DCORE,
     &           GTRP,GTRPM,HLIMB,HLMBMA,HTANK,HTANKM,PLOADM,
     &           PNOLOM,Q,RLTANK,RLTNKM,SOUNDM,USHORE,ZWOULI
*
       INTEGER   I,IALT,IWDG,I0,J,JFC,JI,NCONST,NWILI
*
       LOGICAL   BBVFR
*
       DIMENSION TEXT3(14),TEXT4(14)
       CHARACTER TEXT3*24,TEXT4*5
*
       DATA TEXT3/
     & 'Load Losses ............',
     & 'No-load losses .........',
     & 'Core diameter ..........',
     & 'Core diameter ..........',
     & 'Tank height ............',
     & 'Limb height ............',
     & 'Tank width .............',
     & 'Tank length ............',
     & 'Transport mass..........',
     & 'Sound level ............',
     & 'Flux density ...........',
     & 'Current density    wdg',
     & 'Compressive stress wdg',
     & 'Tensile     stress wdg'/
       DATA TEXT4/'kW','kW','mm','mm','mm','mm','mm',
     &      'mm','kg','dB','T ','A/mm2','N/mm2','N/mm2'/
       DATA TUSEN/1.E+3/,TUSTUS/1.E+6/
*
C... First set QUOTE = 0.
*
       DO 299 I=1,50
       QUOTE(I,1)=0.
  299  QUOTE(I,2)=1.
*
C... Print the critical limits if IALT=2
*
       IF (IALT.EQ.2) THEN
*
C... Then calculate the current QUOTE
*
          IF(PLOADM.GT.0) THEN
             QUOTE(1,1)=URC(1,1,1)*ZWOULI/TUSEN
             QUOTE(1,2)=PLOADM/TUSEN
          END IF
*
          IF(PNOLOM.GT.0) THEN
             QUOTE(2,1)=P00(1)/TUSEN
             QUOTE(2,2)=PNOLOM/TUSEN
          END IF
*
          QUOTE(3,1)=DCORE*TUSEN
          QUOTE(3,2)=DCOMAX*TUSEN
          QUOTE(4,1)=DCOMIN*TUSEN
          QUOTE(4,2)=DCORE*TUSEN
          QUOTE(5,1)=HTANK*TUSEN
          QUOTE(5,2)=HTANKM*TUSEN
          QUOTE(6,1)=HLIMB*TUSEN
          QUOTE(6,2)=HLMBMA*TUSEN
          QUOTE(7,1)=BTANK*TUSEN
          QUOTE(7,2)=BTANKM*TUSEN
          QUOTE(8,1)=RLTANK*TUSEN
          QUOTE(8,2)=RLTNKM*TUSEN
          QUOTE(9,1)=GTRP
          QUOTE(9,2)=GTRPM
*
          IF(SOUNDM.GT.0) THEN
             QUOTE(10,1)=SOUND0(2)
             QUOTE(10,2)=SOUNDM
          END IF
*
          IF(BLTOPM.GT.0) THEN
             IF(BBVFR) THEN
                QUOTE(11,1)=BLIMB*BMAXPU
                QUOTE(11,2)=BLTOPM*BMAXPU
             ELSE
                QUOTE(11,1)=BLIMB
                QUOTE(11,2)=BLTOPM
             END IF
          END IF
*
          DO 199 IWDG=1,NWILI
          CURDUT(IWDG)=CURRUT(IWDG)/ACOND(IWDG)
          QUOTE(9+IWDG*3,1)=ABS(CURDUT(IWDG))/TUSTUS
          QUOTE(9+IWDG*3,2)=ABS(CURDM(IWDG))/TUSTUS
          QUOTE(10+IWDG*3,1)=ABS(STRWMM(IWDG))/TUSTUS
          QUOTE(10+IWDG*3,2)=ABS(PRESSM(IWDG))/TUSTUS
          QUOTE(11+IWDG*3,1)=ABS(STRWMP(IWDG))/TUSTUS
  199     QUOTE(11+IWDG*3,2)=ABS(TENSM(IWDG))/TUSTUS
*
C... Print the heading
*
          CALL NPAGE(2)
          WRITE(JFC,16)
*
C... First print general constraints
*
          NCONST=0
          DO 399 I=1,11
          Q=QUOTE(I,1)/QUOTE(I,2)
          IF(QUOTE(I,2).LT.5.) THEN
            IF (Q.GT.1.001) THEN
              NCONST=NCONST+1
              CALL NPAGE(1)
              WRITE(JFC,7)TEXT3(I),QUOTE(I,2),TEXT4(I),QUOTE(I,1)
     &            ,ABS(QUOTE(I,1)-QUOTE(I,2))
     &            ,100.*ABS(QUOTE(I,1)-QUOTE(I,2))/QUOTE(I,2)
            ELSE IF (Q.GT.0.990.AND.Q.LE.1.001) THEN
              NCONST=NCONST+1
              CALL NPAGE(1)
              WRITE(JFC,4)TEXT3(I),QUOTE(I,2),TEXT4(I),QUOTE(I,1)
            END IF
*
          ELSE IF (QUOTE(I,2).GT.500.) THEN
            IF (Q.GT.1.001) THEN
              NCONST=NCONST+1
              CALL NPAGE(1)
              WRITE(JFC,5)TEXT3(I),QUOTE(I,2),TEXT4(I),QUOTE(I,1)
     &            ,ABS(QUOTE(I,1)-QUOTE(I,2))
     &            ,100.*ABS(QUOTE(I,1)-QUOTE(I,2))/QUOTE(I,2)
            ELSE IF (Q.GT.0.990.AND.Q.LE.1.001) THEN
              NCONST=NCONST+1
              CALL NPAGE(1)
              WRITE(JFC,2)TEXT3(I),QUOTE(I,2),TEXT4(I),QUOTE(I,1)
            END IF
          ELSE
            IF (Q.GT.1.001) THEN
              NCONST=NCONST+1
              CALL NPAGE(1)
              WRITE(JFC,6)TEXT3(I),QUOTE(I,2),TEXT4(I),QUOTE(I,1)
     &            ,ABS(QUOTE(I,1)-QUOTE(I,2))
     &            ,100.*ABS(QUOTE(I,1)-QUOTE(I,2))/QUOTE(I,2)
            ELSE IF (Q.GT.0.990.AND.Q.LE.1.001) THEN
              NCONST=NCONST+1
              CALL NPAGE(1)
              WRITE(JFC,3)TEXT3(I),QUOTE(I,2),TEXT4(I),QUOTE(I,1)
            END IF
          END IF
  399     CONTINUE
*
C... Then print winding constraints
*
          DO 499 J=1,NWILI
          DO 599 I0=1,3
            I=11+I0
            JI=I+3*(J-1)
            Q=QUOTE(JI,1)/QUOTE(JI,2)
            IF (Q.GT.1.001) THEN
              NCONST=NCONST+1
              CALL NPAGE(1)
              WRITE(JFC,9)TEXT3(I),J,QUOTE(JI,2),TEXT4(I),QUOTE(JI,1),
     &                    ABS(QUOTE(JI,1)-QUOTE(JI,2))
     &            ,100.*ABS(QUOTE(JI,1)-QUOTE(JI,2))/QUOTE(JI,2)
            ELSE IF (Q.GT.0.990.AND.Q.LE.1.001) THEN
              NCONST=NCONST+1
              CALL NPAGE(1)
              WRITE(JFC,8)TEXT3(I),J,QUOTE(JI,2),TEXT4(I),QUOTE(JI,1)
            END IF
  599     CONTINUE
  499     CONTINUE
*
          IF (NCONST.EQ.0) THEN
              CALL NPAGE(2)
              WRITE (JFC,17)
          ENDIF
*
       END IF
*
       RETURN
*
    2  FORMAT(7X,A24,F8.0,1X,A5,1X,F8.0,1X,F6.0)
    3  FORMAT(7X,A24,F8.1,1X,A5,1X,F8.1,1X,F6.1)
    4  FORMAT(7X,A24,F8.4,1X,A5,1X,F8.4,1X,F6.4)
    5  FORMAT(4X,'>>>',A24,F8.0,1X,A5,1X,F8.0,1X,F6.0,' :',F6.1,' <<<')
    6  FORMAT(4X,'>>>',A24,F8.0,1X,A5,1X,F8.0,1X,F6.0,' :',F6.1,' <<<')
    7  FORMAT(4X,'>>>',A24,F8.4,1X,A5,1X,F8.4,1X,F6.4,' :',F6.1,' <<<')
    8  FORMAT(7X,A22,I2,F8.2,1X,A5,1X,F8.2,1X,F6.2)
    9  FORMAT
     &    (4X,'>>>',A22,I2,F8.2,1X,A5,1X,F8.2,1X,F6.2,' :',F6.1,' <<<')
   15  FORMAT
     &  (1X/6X,'* *   O P T I M I Z A T I O N     L I M I T S   * *',/,
     &  /6X,'  Parameter                  Limit        Value  Excess',/)
   16  FORMAT(1X/6X,' * * C R I T I C A L     P A R A M E T E R S * *',
     &  /11X,'(Within 1% of or exceeding the limit)',2(/),
     &  6X,'  Parameter                  Limit        Value  Excess :',
     &  ' Percent',/)
   17  FORMAT(6X,'   - No parameter has reached its limit -')
       END
