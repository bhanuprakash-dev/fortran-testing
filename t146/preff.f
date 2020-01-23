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
*      Subroutine PREFF
*      ----------------
*
C...   Title: Calculation & printout of voltage cases & efficiency
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PREFF
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1259(1)    ,  SRATE  (1)    ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1689(1,1,1),  URC    (1,1,1))
       EQUIVALENCE
     &  (XZ1690(1,1,1),  UXC    (1,1,1))
*
       REAL      P00(3),SRATE(4),URC(4,4,3),UXC(4,4,3),ETA(5),DELTA(5),
     &           COSFI,FACT,HELP,HELP1,HELP2,Q,ZWOULI
*
       INTEGER   I,J,JFC,J1
*
CC*SEBL End of the declarations block.
*
C... Start a new page
*
       CALL  NPAGE(13)
       WRITE(JFC,80)
       WRITE(JFC,81) SRATE(2)/1.E+6
       WRITE(JFC,82)
*
       COSFI=0.75
       FACT=URC(1,1,1)*ZWOULI/SRATE(2)
*
C... Calculate DELTA for each of 5 power factors
*
       DO 299 I=1,5
       COSFI=COSFI+0.05
       Q=SQRT(1.-COSFI*COSFI)
       DELTA(I)=(COSFI*FACT+Q*UXC(1,1,1)+
     &          (COSFI*UXC(1,1,1)-Q*FACT)**2/2.)*100.
  299  CONTINUE
*
       WRITE(JFC,83) (DELTA(I),I=1,5)
       WRITE(JFC,84)
*
       HELP1=1.
       COSFI=0.75
*
C... Calculate ETA for each of 4 situations
*
       DO 399 I=1,4
       HELP2=HELP1-FLOAT(I-1)*0.25
       HELP=P00(1)+HELP2**2*URC(1,1,1)*ZWOULI
*
C... Calculate ETA for each of 5 power factors
*
       DO 499 J=1,5
       J1=J
       ETA(J1)=  HELP/(SRATE(2)*HELP2*(COSFI+FLOAT(J1)*0.05)+HELP)
  499  ETA(J1)= (1.-ETA(J1))*100.
*
C... Print the result
*
C,,, I <= 1
*
       IF (I.LE.1) THEN
          WRITE(JFC,85) HELP2,ETA(1),ETA(2),ETA(3),ETA(4),ETA(5)
*
C... I > 1
*
       ELSE
          WRITE(JFC,86) HELP2,ETA(1),ETA(2),ETA(3),ETA(4),ETA(5)
       END IF
  399  CONTINUE
*
       RETURN
*
   80  FORMAT(1X/1X/1X,'     VOLTAGE DROP and EFFICIENCY')
   81  FORMAT(1X/1X,5X,'K*',F7.1,
     &       ' MVA  Terminal1-Terminal2, Principal tap')
   82  FORMAT(1X/1X,5X,'COS(Phi)',6X,'      0.80      0.85      0.90',
     &  '      0.95      1.00')
   83  FORMAT(6X,'K=1.00 Delta U(%)  ',5(F6.3,4X))
   84  FORMAT(1X)
   85  FORMAT(6X,'K=',F4.2,' Eta    (%)',T26,4(F6.3,4X),F6.3)
   86  FORMAT(6X,'K=',F4.2,T26,4(F6.3,4X),F6.3)
*
       END
