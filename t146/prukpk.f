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
*      Subroutine PRUKPK
*      -----------------
*
C...   Title: Prints out concentrated information on
C...          rated voltages, short-circuit voltages,
C...          and load losses.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PRUKPK
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1267(1)    ,  UDIM   (1)    ),
     &  (XZ1271(1,1)  ,  UN     (1,1)  ),
     &  (XZ1283(1)    ,  UNLINE (1)    ),
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1486       ,  JFC           )
       EQUIVALENCE
     &  (XZ1499       ,  NG            ),
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1689(1,1,1),  URC    (1,1,1)),
     &  (XZ1690(1,1,1),  UXC    (1,1,1))
*
       REAL      UXC(4,4,3),UNLINE(4),UDIM(4),
     &           UN(4,4),U(3,3),URC(4,4,3),CCON
*
       INTEGER   I,IFORM,ITML,JFC,NG,NWOULI,N1,N2
*
       DIMENSION FOR810(19),FOR815(5),CH0(9)
       CHARACTER FOR810*4,FOR815*4,CH0*1
*
       LOGICAL   BBVR(4),BB1
*
       DATA
     & CH0/'1','2','3','4','5','6','7','8','9'/
     & FOR810 / '(6X,',  '''VOL',  'T(kV',  ') U''',  ',I1,',
     &        '''='',',   'F6. ',  '    ',  ',''  ',  'U'',I',
     &         '1,''=',  ''',T3',  '0,  ',   ' F6.',  '    ',
     &          ')   ',   '    ',  '    ',   '    ' /
*
       DATA FOR815 / '3F9.',  ',T58',  ',''U3',  '='',F',  '6.2)'/
*
CC*SEBL End of the declarations block.
*
C... Calculate rated voltage for each terminal
*
       DO 299 ITML=1,NG
       CCON=UNLINE(ITML)/UDIM(ITML)
       U(ITML,1)=UN(ITML,1)*CCON

       IF(BBVR(ITML)) U(ITML,2)=UN(ITML,2)*CCON
      
       IF(BBVR(ITML)) U(ITML,3)=UN(ITML,3)*CCON
 
  299  CONTINUE
*
  
       N2=2
       IF(BBVR(1).AND..NOT.BBVR(2)) N2=1
       N1=3 -N2
       BB1=.FALSE.
       IF (BBVR(1).OR.BBVR(2)) BB1=.TRUE.
       FOR810(8)=CH0(6-INT(ALOG10(U(N1,1))))
       FOR810(15)=CH0(6-INT(ALOG10(U(N2,1))))
    
       IF (BB1) FOR810(14)=FOR815(1)
*
   
C... Adjust the formats to be used
*
C,,, If more than 2 terminals
*
       IF (NG.NE.2) THEN
*
 
C... Adjust the formats
*
          DO 699 IFORM=1,4
  699     FOR810(15+IFORM)=FOR815(IFORM+1)
       END IF
*

       IF (.NOT.BB1.AND.NG.EQ.2)
     & WRITE(JFC,FOR810) N1,U(N1,1)/1.E+3,N2,U(N2,1)/1.E+3
       IF (.NOT.BB1.AND.NG.GE.3)
     & WRITE(JFC,FOR810) N1,U(N1,1)/1.E+3,N2,U(N2,1)/1.E+3,U(3,1)/1.E+3
*
       IF (BB1.AND.NG.EQ.2)
     & WRITE(JFC,FOR810) N1,U(N1,1)/1.E+3,N2,(U(N2,I)/1.E+3,I=1,3)
       IF (BB1.AND.NG.GE.3)
     & WRITE(JFC,FOR810) N1,U(N1,1)/1.E+3,N2,(U(N2,I)/1.E+3,I=1,3),
     &                   U(3,1)/1.E+3
*
       IF (.NOT.BBVR(2)) WRITE(JFC,825) (1.E+2*UXC(I,1,1),I=1,3),
     & UXC(1,1,2)*100.,UXC(1,1,3)*100.
       IF (BBVR(2))      WRITE(JFC,825) (1.E+2*UXC(1,I,1),I=1,3),
     & UXC(1,1,2)*100.,UXC(1,1,3)*100.
*
       IF (.NOT.BBVR(2)) WRITE(JFC,845) (URC(I,1,1)*NWOULI/1.E+3,I=1,3)
       IF (     BBVR(2)) WRITE(JFC,845) (URC(1,I,1)*NWOULI/1.E+3,I=1,3)
  
       RETURN
*
  825  FORMAT(6X,'Reactance   (%) 1-2  ',3F9.2,'  1-3',F7.2,'  2-3',
     &        F7.2)
  845  FORMAT(6X,'Load losses(kW) 1-2  ',3F9.1)
*
        END
