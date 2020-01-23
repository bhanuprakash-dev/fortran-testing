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
*      Subroutine WDGLAY
*      -----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 83-04-29 by H. Westberg  , ZKAB
*      Revised: 87-04-27 by Ron Bell     , TRAFO/IK
*
*  1.  Description
C...       Title:    Calculate:
C...              1. The physical number of turns in each winding
C...              2. The geometrical dimensions of each winding
*
************************************************************************
*
       SUBROUTINE WDGLAY
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1508       ,  NWILI         ),
     &  (XZ0911(1)    ,  KWITYP (1)    ),
     &  (XZ0991(1)    ,  RPART  (1)    ),
     &  (XZ0891(1)    ,  KCODE  (1)    )
       EQUIVALENCE
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1513       ,  PI            ),
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1331(1)    ,  BBRR   (1)    )
       EQUIVALENCE
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ0671(1)    ,  BDUCT  (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ0751(1)    ,  DYOKE  (1)    ),
     &  (XZ0651(1)    ,  ACOND  (1)    )
       EQUIVALENCE
     &  (XZ0791(1)    ,  FILLF  (1)    ),
     &  (XZ2563       ,  BBADJU        ),
     &  (XZ2589(1)    ,  SWIND  (1)    ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ2590(1)    ,  RINNER (1)    ),
     &  (XZ2593(1)    ,  ZPART  (1)    )
*
       REAL      ZWIND(9),RRWDG(9),BDUCT(9),HWIND(9),DYOKE(9),ACOND(9),
     &           FILLF(9),RPART(9),SWIND(9),RINNER(9),ZPART(9)
*
       INTEGER   KCODE(9),KWITYP(9),ISTOP
*
       LOGICAL BBADJU,BBRR(9)
*
       WRITE (*,100) 'Automatic winding layout'
*
C... Convert the winding types for the winding layout routines
*
       CALL TYPWDG
*
       CALL WDGTYP
*
       DO 1200 IWDG=1,NWILI
       RPART(IWDG)=1.
*
C... Calculate winding
*
C,,, Radial dimension Using space factors
*
       IF (BBRR(IWDG)) THEN
          HWIND(IWDG)=HLIMB-DYOKE(IWDG)
          RRWDG(IWDG)=ZWIND(IWDG)*ACOND(IWDG)/HWIND(IWDG)/FILLF(IWDG)
*
          IF (IWDG.EQ.1) THEN
             RINNER(IWDG)=DCORE/2.+BDUCT(IWDG)
          ELSE
             RINNER(IWDG)=RINNER(IWDG-1)+BDUCT(IWDG)+RRWDG(IWDG-1)
          END IF
*
          SWIND(IWDG)=(2.*RINNER(IWDG)+RRWDG(IWDG))*PI
*
C... Complete winding layout
*
       ELSE
*
          IF      (KWITYP(IWDG).EQ.1.OR.
     &             KWITYP(IWDG).EQ.6) THEN
                                           CALL SCREW(IWDG)
          ELSE IF (KWITYP(IWDG).EQ.2) THEN
                                           CALL LWIND(IWDG)
          ELSE IF (KWITYP(IWDG).EQ.3.OR.
     &             KWITYP(IWDG).EQ.4.OR.
     &             KWITYP(IWDG).EQ.5) THEN
                                           CALL CONTD(IWDG)
          ELSE IF (KWITYP(IWDG).EQ.7) THEN
                                           CALL LOOP1(IWDG)
          END IF
       END IF
1200   CONTINUE
*
* ...  BACKTRACKING
       IF(ISTOP.EQ.1) RETURN
*
C... Convert the winding types for the main program routines
*
       CALL WDGTYP
*
       CALL ADJUST
       BBADJU=.TRUE.
*
       RETURN
  100  FORMAT (' ',/,' ',A,/)
       END
