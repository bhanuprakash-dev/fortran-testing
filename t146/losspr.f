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
*      Subroutine LOSSPR
*      -----------------
*
C...   Title:  Edit a table which presents
C...           total load losses for different tap-positions
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 86-10-08 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE LOSSPR(ITAP2,JTAP2,KTML2,LTML2,MTML)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0691(1)    ,  CUR    (1)    ),
     &  (XZ0692(1,1)  ,  CURPRT (1,1)  ),
     &  (XZ0693(1,1)  ,  CURMNT (1,1)  ),
     &  (XZ0694(1,1)  ,  CURMXT (1,1)  )
       EQUIVALENCE
     &  (XZ0695(1,1)  ,  CURSPT (1,1)  ),
     &  (XZ0911(1)    ,  KWITYP (1)    ),
     &  (XZ0951(1)    ,  PCUW   (1)    ),
     &  (XZ0961(1)    ,  PEDW   (1)    ),
     &  (XZ1215(1)    ,  KCON   (1)    )
       EQUIVALENCE
     &  (XZ1263(1)    ,  SRATEP (1)    ),
     &  (XZ1271(1,1)  ,  UN     (1,1)  ),
     &  (XZ1272(1)    ,  UNTAP  (1)    ),
     &  (XZ1443       ,  FLUXM         ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1511       ,  PCU           ),
     &  (XZ1512       ,  PED           ),
     &  (XZ1521       ,  POED          ),
     &  (XZ1545       ,  SQR3          ),
     &  (XZ1571       ,  ZWOULI        )
       EQUIVALENCE
     &  (XZ2298(1)    ,  NCOL   (1)    ),
     &  (XZ2401(1)    ,  FLUXI  (1)    ),
     &  (XZ2411(1)    ,  FLUXO  (1)    )
*
       REAL      PCUW(9),PEDW(9),CUR(9),ACOND(9),SRATEP(4),
     &           ARR1(9),ARR2(9),ARR3(9),ARR4(9),UNTAP(24),
     &           UN(4,4),ACUR(9),FLUXI(9),FLUXO(9),
     &           CURPRT(9,3),CURMNT(9,3),CURMXT(9,3),CURSPT(9,3)
*
       INTEGER   KCON(4),MTML,ITAP2,JTAP2,KTML2,LTML2,KWITYP(9)
*
       DIMENSION NCOL(9),WTYPE1(9)
       CHARACTER NCOL*1,WTYPE1*4
*
CC*SEBL End of the declarations block.
*
C... Adjust paper position on the printout
*
  100  CALL NPAGE(15)
       C1=ZWOULI/1.E+3
       C2=1.E-3
       IF(KCON(KTML2).EQ.1.OR.KCON(KTML2).GT.10) C2=C2*SQR3
       C3=1.E-3
       IF(KCON(LTML2).EQ.1.OR.KCON(LTML2).GT.10) C3=C3*SQR3
       C4=1.E-3
*
       WRITE(JFC,1) KTML2,LTML2,AMIN1(SRATEP(KTML2),
     &              SRATEP(LTML2))*C1/1.E+3,
     &              UN(KTML2,ITAP2)*C2,UN(LTML2,JTAP2)*C3
       WRITE(JFC,2)
       WRITE(JFC,3) PCU*C1,PED*C1,POED*C1,(PCU+PED+POED)*C1
       IF (KWITYP(NWILI).EQ.5) THEN
          WRITE(JFC,5)
       ELSE
          WRITE(JFC,6)
       ENDIF
*
C... Prepare some data for the database
*
       UNTAP(ITAP2+4*(KTML2+LTML2-3))=UN(KTML2,ITAP2)*C2
       UNTAP(12+ITAP2+4*(KTML2+LTML2-3))=UN(LTML2,JTAP2)*C3
*
C... Prepare the data for printout
*
  200  DO 299  IWDG=1,NWILI
       WTYPE1(IWDG)='    '
*
C... Create the required character string
*
       CALL CONCAT(WTYPE1(IWDG),1,NCOL(IWDG),1,1)
       ACUR(IWDG)=ABS(CUR(IWDG))
*
C...   Prepare data for the database.
*
       IF (ITAP2.EQ.1) THEN
          CURPRT(IWDG,MTML)=ACUR(IWDG)
       ELSE IF (ITAP2.EQ.2) THEN
          CURMNT(IWDG,MTML)=ACUR(IWDG)
       ELSE IF (ITAP2.EQ.3) THEN
          CURMXT(IWDG,MTML)=ACUR(IWDG)
       ELSE IF (ITAP2.EQ.4) THEN
          CURSPT(IWDG,MTML)=ACUR(IWDG)
       ENDIF
       ARR1(IWDG)=ACUR(IWDG)/ACOND(IWDG)*C4*C4
       ARR2(IWDG)=(PCUW(IWDG)+PEDW(IWDG))*C4
       ARR3(IWDG)=PCUW(IWDG)*C4
  299  ARR4(IWDG)=PEDW(IWDG)*C4
*
C... Print a row of CHARACTER data
*
       CALL PRWT ('Winding  ',WTYPE1 ,JFC,NWILI)
*
C... Print a row of REAL data ( Last parameter defines No. of decimals)
*
       CALL PRWA ('Curr. (A)',ACUR ,1,JFC,NWILI)
*
CC*SOFF Switch OFF structure analyser (Not all calls need be shown)
*
       CALL PRWA ('J (A/mm2)',ARR1 ,3,JFC,NWILI)
       CALL PRWA ('RI2  (kW)',ARR3 ,1,JFC,NWILI)
       CALL PRWA ('DRI2 (kW)',ARR4 ,1,JFC,NWILI)
       CALL PRWA ('Total(kW)',ARR2 ,1,JFC,NWILI)
       CALL PRWA ('Flux ins.',FLUXI,3,JFC,NWILI)
       CALL PRWA ('Flux out.',FLUXO,3,JFC,NWILI)
       WRITE(JFC,4) FLUXM
*
CC*SON  Switch ON  structure analyser (Not all calls need be shown)
*
  199  RETURN
*
    1  FORMAT(1X/6X,'Terminal',I2,'-',I1,F15.1,' MVA',
     &        F15.2,'kV /',F7.2,' kV')
    2  FORMAT(1X/6X,'L O S S E S',8X,'R*I*I',11X,'Eddy',7X,
     &       'Other.eddy',8X,'Total')
    3  FORMAT(6X,'(KW)',5X,2F15.1,F15.3,F15.1)
CCC 3  FORMAT(6X,'(KW)',5X,2F15.1,F15.3,F15.1,/,1X)
    4  FORMAT(6X,'Maximum flux (Vs)',F10.4)
    5  FORMAT(6X,'WARNING: Other.eddy losses are underestimated ',
     &       '(D2-winding).')
    6  FORMAT(1X)
       END
