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
*      Subroutine TABLE1
*      -----------------
*
C...   Title:  Print out the short-circuit cases
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE TABLE1(I1,I2,I3)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1021(1)    ,  SHTCUR (1)    ),
     &  (XZ1131(1)    ,  X10    (1)    ),
     &  (XZ1141(1)    ,  X20    (1)    ),
     &  (XZ1151(1)    ,  X30    (1)    ),
     &  (XZ1255(1)    ,  SLINE  (1)    )
       EQUIVALENCE
     &  (XZ1486       ,  JFC           ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1647(1)    ,  NPERM  (1)    ),
     &  (XZ2298(1)    ,  NCOL   (1)    )
*
       REAL      SLINE(4),X10(9),X20(9),X30(9),SHTCUR(9)
*
       INTEGER   NPERM(3),I,IWDG,I1,I2,I3,I3NG,JFC,NG,NWILI
*
       DIMENSION NCOL(9) , TAP(4) , WTYPE1(9)
       CHARACTER NCOL*1 , TAP*5 , WTYPE1*4
*
       DATA TAP / '  Prc', '  Max', '  Min', ' Spec' /
*
CC*SEBL End of the declarations block.
*
C... Print the heading for a new page
*
       CALL NPAGE(9)
*
       WRITE(JFC,100) I1
       IF (NG.EQ.2) WRITE(JFC,200) I2,SLINE(I2)/1.E+9
       IF (NG.GE.3) WRITE(JFC,300) I2,SLINE(I2)/1.E+9,I3,SLINE(I3)/1.E+9
*
       I3NG=MIN0(3,NG)
       WRITE(JFC,400) ('Term',I,TAP(NPERM(I)),I=1,I3NG)
*
CC*SBBL Start of the iteration block.
*
C... For each winding
*
       DO 10 IWDG=1,NWILI
       WTYPE1(IWDG)='    '
*
C... Create the required character string
*
       CALL CONCAT(WTYPE1(IWDG),1,NCOL(IWDG),1,1)
   10  CONTINUE
*
CC*SEBL End of the iteration block.
*
C... Print the data in string format
*
       CALL  PRWT('Winding  ', WTYPE1,JFC,NWILI)
*
C... Print the data in decimal format
*
       CALL  PRWR('Inner    ', X10,   1,1.E-6,JFC,NWILI)
*
CC*SOFF Switch OFF structure analyser (Not all calls need be shown)
*
       CALL  PRWR('Outer    ', X20,   1,1.E-6,JFC,NWILI)
       CALL  PRWR('Mean     ', X30,   1,1.E-6,JFC,NWILI)
       CALL  PRWR('Curr (kA)', SHTCUR,1,1.E-3,JFC,NWILI)
       WRITE(JFC, 500)
*
CC*SON  Switch ON  structure analyser (Not all calls need be shown)
*
       RETURN
*
  100  FORMAT(6X,'TERMINAL ',I1,' SHORTCIRCUITED')
  200  FORMAT(6X,'Supply from terminal ',I1,' (',F6.1,' GVA)')
  300  FORMAT(6X,'Supply from terminal ',I1,' (',F6.1,' GVA)',
     &            T43,' and terminal ',I1,' (',F6.1,' GVA)')
  400  FORMAT(6X,4(A4,I2,A5,' tap',3X))
  500  FORMAT(1X)
*
       END
