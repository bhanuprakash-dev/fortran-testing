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
*      Subroutine PRUX
*      ---------------
*
C...   Title:  Output of reactances
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PRUX
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1215(1)    ,  KCON   (1)    ),
     &  (XZ1227(1)    ,  NMSTEP (1)    ),
     &  (XZ1231(1)    ,  NPSTEP (1)    ),
     &  (XZ1259(1)    ,  SRATE  (1)    ),
     &  (XZ1271(1,1)  ,  UN     (1,1)  )
       EQUIVALENCE
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1545       ,  SQR3          )
       EQUIVALENCE
     &  (XZ1690(1,1,1),  UXC    (1,1,1)),
     &  (XZ3010(1)    ,  NXSTEP (1)    )
*
       REAL      SRATE(4),UN(4,4),UXC(4,4,3),SQR3,C1,C2
*
       INTEGER   KCON(4),NMSTEP(4),NPSTEP(4),NXSTEP(3),IN,ITML,
     &           JFC,NTAPS,JTML,KTAP,LTML,M,NG,NGMAX
*
       LOGICAL   BBVFR,BBVR(4)
*
       DIMENSION CTAP(4)
       CHARACTER CTAP*12,ROWHED*12
*
       DATA CTAP /'Prc.  tap : ',
     &            'Max.  tap : ',
     &            'Min.  tap : ',
     &            'Spec. tap : ' /
*
CC*SEBL End of the declarations block.
*
       NGMAX=MIN0(3,NG)
*
       WRITE(JFC,80) SRATE(1)/1.E+6
*
C... Calculate No. of taps to be used
*
       DO 299 JTML=1,NGMAX-1
*
C... Adjust the No. of taps
*
C,,, If a 'special' tap is given
*
       NTAPS=1
       IF (BBVR(JTML)) THEN
          NTAPS=4
          IF ((NXSTEP(JTML).EQ.NPSTEP(JTML))
     &    .OR.(NXSTEP(JTML).EQ.NMSTEP(JTML))
     &    .OR.(NXSTEP(JTML).EQ.0           )) NTAPS=3
       END IF
       C2=1.E-3
       IF (KCON(JTML).EQ.1.OR.KCON(JTML).EQ.11) C2=SQR3/1.E+3
*
C... Print the reactances
*
CC*SBBL Start of the printing block
*
C... For each terminal required
*
       DO 399 ITML=JTML+1,NGMAX
       LTML=ITML+JTML-2
       IN=1
       IF (BBVR(ITML).OR.BBVFR) IN=3
       C1=1.E-3
       IF (KCON(ITML).EQ.1.OR.KCON(ITML).EQ.11) C1=SQR3/1.E+3
       WRITE(JFC,81) ('U',ITML,UN(ITML,KTAP)*C1,KTAP=1,IN)
*
C... Adjust the heading for each tap
*
       DO 499 KTAP=1,NTAPS
*
C... Adjust the heading
*
C,,, If regulated terminal or VFR
*
       IF (BBVR(ITML).OR.BBVFR) THEN
          ROWHED='     '
*
C... If NON-regulated terminal or CFR
*
       ELSE
          ROWHED=CTAP(KTAP)
       END IF
       WRITE(JFC,82) ROWHED,
     & JTML,UN(JTML,KTAP)*C2,(100.*UXC(KTAP,M,LTML),M=1,IN)
  499  CONTINUE
  399  CONTINUE
*
CC*SEBL End of the printing block
*
  299  CONTINUE
*
       RETURN
*
   80  FORMAT(1X/'      Reactances  (in percent. Base',F7.1,' MVA)')
   81  FORMAT(1X/22X,3(6X,A1,I1,'=',F7.2,' kV'))
   82  FORMAT(6X,A12,'U',I1,'=',F7.2,' kV',1X,
     &        2(F6.2,' %',11X),F6.2,' %')
*
       END
