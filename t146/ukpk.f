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
*      Subroutine UKPK
*      ---------------
*
C...   Title:  Calculation of rated voltages,
C...                          short-circuit voltages,
C...                      and load losses.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE UKPK(U1,U2,U3,UX12,UX13,UX23,UR12,UR13,UR23)
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
       REAL      UNLINE(4),UDIM(4),UN(4,4),UCONV(4,4),
     &           UXC(4,4,3),URC(4,4,3),
     &           UR12(4),UR13(4),UR23(4),
     &           U1(4),U2(4),U3(4),U4(4),UX12(4),UX13(4),UX23(4),
     &           CCON
*
       INTEGER   I,J,JFC,JTML,NG,NWOULI,N2
*
       LOGICAL BBVR(4),BB1
*
CC*SEBL End of the declarations block.
*
C... Initialise certain variables
*
       DO 10 I=1,4
       UX12(I)=0.
       UX13(I)=0.
       UX23(I)=0.
       UR12(I)=0.
       UR13(I)=0.
       UR23(I)=0.
*
C... Initialise certain variables
*
       DO 10 J=1,4
   10  UCONV(I,J)=0.
*
C... Calculate values for each terminal
*
       DO 299 JTML=1,NG
       CCON=UNLINE(JTML)/UDIM(JTML)
       UCONV(1,JTML)=UN(JTML,1)*CCON
       IF (BBVR(JTML)) UCONV(2,JTML)=UN(JTML,2)*CCON
       IF (BBVR(JTML)) UCONV(3,JTML)=UN(JTML,3)*CCON
       IF (BBVR(JTML)) UCONV(4,JTML)=UN(JTML,4)*CCON
  299  CONTINUE
*
       N2=2
       IF (BBVR(1).AND..NOT.BBVR(2)) N2=1
       BB1=.FALSE.
       IF (BBVR(1).OR.BBVR(2)) BB1=.TRUE.
*
C... Calculate values for each terminal
*
       DO 599 JTML=1,4
*
       U1(JTML)=UCONV(1,JTML)/1.E+3
       U2(JTML)=UCONV(2,JTML)/1.E+3
       U3(JTML)=UCONV(3,JTML)/1.E+3
       U4(JTML)=UCONV(4,JTML)/1.E+3
*
       IF (.NOT.BBVR(2)) UX12(JTML)=UXC(JTML,1,1)*100.
       IF (     BBVR(2)) UX12(JTML)=UXC(1,JTML,1)*100.
*
       IF (.NOT.BBVR(2)) UR12(JTML)=URC(JTML,1,1)*NWOULI/1.E+3
       IF (     BBVR(2)) UR12(JTML)=URC(1,JTML,1)*NWOULI/1.E+3
       write(*,*) "ukpk UX12=",UX12(JTML)
  599  CONTINUE
*
       UX13(1)=UXC(1,1,2)*100.
       UX23(1)=UXC(1,1,3)*100.
*
       UR13(1)=URC(1,1,2)*NWOULI/1.E+3
       UR23(1)=URC(1,1,3)*NWOULI/1.E+3
*
       RETURN
       END
