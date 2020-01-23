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
*      Subroutine CHFUNK
*      -----------------
*
C...   Title:  Checks for correct winding function codes
C...              and for correct winding fractions
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE CHFUNK
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0801(1)    ,  FRACT  (1)    ),
     &  (XZ0891(1)    ,  KCODE  (1)    ),
     &  (XZ0901(1)    ,  KGROUP (1)    ),
     &  (XZ1219(1)    ,  KTYPRW (1)    ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1341(1)    ,  BBRW   (1)    )
       EQUIVALENCE
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1373       ,  BBAUTO        ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1500       ,  NPG           )
       EQUIVALENCE
     &  (XZ1508       ,  NWILI         )
*
       REAL       FRACT(9),SUM,X
*
       INTEGER    KGROUP(9),KCODE(9),KTYPRW(4),LX(4,4),ICODE,ITML,JFC,
     &            IWDG,JTML,JWDG,LX1M,LX2M,LX3M,LX4M,MTML,NG,NPG,NWILI,
     &            ISTOP
*
       LOGICAL    BBRW(9),BBVFR,BBVR(4),BBAUTO,BBX1,BBX2,BBX3,
     &            BBTS,BBERR,BBHELP(10)
*
CC*SEBL End of the declarations block.
*
C... Initialisation of LX
*
CC*SBBL Start of the initialisation block.
*
C... For 1 to 4
*
       DO 299 ICODE=1,4
*
C... For 1 to 4
*
       DO 299 JTML=1,4
  299  LX(ICODE,JTML)=0
*
CC*SEBL End of the initialisation block.
*
C... Determination of the No. of windings and their respective
C... function codes per terminal.
*
       DO 399 IWDG=1,NWILI
  399  LX(KCODE(IWDG),KGROUP(IWDG))=1+LX(KCODE(IWDG),KGROUP(IWDG))
*
C... Loop covering checks for each winding by terminal.
*
       DO 499 ITML=1,NG
*
C... Regulating windings are stated illegally.
*
       IF  ( LX(2,ITML)+LX(4,ITML).GT.1
     &       .OR. LX(3,ITML)+LX(4,ITML).GT.1
     &       .OR.(LX(3,ITML).EQ.1.AND.LX(2,ITML).NE.1)) THEN
                CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &              'CHFUNK:Reg windings are stated illegally')
* ...           BACKTRACKING
                IF(ISTOP.EQ.1) RETURN
       ENDIF
*
C... Main winding missing.
*
       IF(LX(1,ITML)+LX(4,ITML).LT.1) THEN
             CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &                 'CHFUNK:Main winding missing')
* ...        BACKTRACKING
             IF(ISTOP.EQ.1) RETURN
       ENDIF
*
C... Initialisation of MTML
*
       MTML=ITML
*
C... Determine MTML , the terminal No. used later.
*
C,,, VFR and terminals 1 & 2 only.
*
       IF(ITML.GT.2.OR..NOT.BBVFR) GOTO 599
*
C... Determine MTML.
*
          MTML=1
          IF(BBAUTO) MTML=NPG
  599  CONTINUE
*
C... Checking the Regulating winding function.
*
C,,, Regulated terminal.
*
       IF(BBVR(ITML)) THEN
*
C... Preparing some logical variables.
*
          LX1M=LX(1,MTML)
          LX2M=LX(2,MTML)
          LX3M=LX(3,MTML)
          LX4M=LX(4,MTML)
          BBX1=.FALSE.
          IF (KTYPRW(ITML).EQ.1.AND..NOT.(LX2M+LX4M.EQ.1.AND.LX3M.EQ.0))
     &    BBX1=.TRUE.
          BBX2=.FALSE.
          IF (KTYPRW(ITML).EQ.2.AND..NOT.(LX2M.EQ.1.AND.LX3M+LX4M.EQ.0))
     &    BBX2=.TRUE.
          BBX3=.FALSE.
          IF (KTYPRW(ITML).EQ.3.AND..NOT.(LX2M*LX3M.EQ.1.AND.LX4M.EQ.0))
     &    BBX3=.TRUE.
*
C... Regulating winding conflicts with function code.
*
          IF(BBX1.OR.BBX2.OR.BBX3) THEN
                      CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &               'CHFUNK:Reg-wdg conflicts with function code')
* ...                BACKTRACKING
                       IF(ISTOP.EQ.1) RETURN
          ENDIF
*
C... NON-regulated terminal.
*
       ELSE
*
C... Preparing some logical variables.
*
          BBX1=.FALSE.
          IF (.NOT.(BBVFR.AND.ITML.EQ.MTML))
     &    BBX1=.TRUE.
          BBX2=.FALSE.
          IF ((LX(2,ITML)+LX(3,ITML)+LX(4,ITML)).GT.0)
     &    BBX2=.TRUE.
*
C... Reg-wdg given on non-regulated terminal
*
          IF(BBX1.AND.BBX2) THEN
                CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &               'CHFUNK:Reg-wdg given on non-regulated terminal')
* ...                BACKTRACKING
                     IF(ISTOP.EQ.1) RETURN
          ENDIF
       END IF
*
C... Initialisation prior to winding fractions check.
*
       SUM=0.
       X=1.
*
C... Winding fraction check
*
       DO 899 JWDG=1,NWILI
*
C... If the winding belongs to the group being calculated .....
*
C,,, KGROUP(JWDG).EQ.ITML
*
       IF(KGROUP(JWDG).EQ.ITML) THEN
*
C... Calculate the fractions and add as necessary
*
          IF(BBRW(JWDG)) X=X*FRACT(JWDG)
          IF(.NOT.BBRW(JWDG)) SUM=SUM+FRACT(JWDG)
       END IF
  899  CONTINUE
*
C... Sum of fractions is not equal to 1.
*
       IF(ABS(SUM-1.0)+ABS(X-1.0).GT.1.E-6) THEN
          CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &              'CHFUNK:Sum of fractions is not equal to 1')
* ...     BACKTRACKING
          IF(ISTOP.EQ.1) RETURN
       ENDIF
*
C... Return
*
  499  CONTINUE
*
       RETURN
       END
