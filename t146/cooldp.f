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
*      Subroutine COOLDP
*      -----------------
*
C...   Title:  Estimate a preliminary price
C...           for the coolers if required.
C...           The following types can be handled:
C...           (CODE = Cooling type) :
C...           1=ONAN,  2=ONAF,  3=OFAF,  4=ONAN/F
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-09-13 by B-G Bladh  , SETFO/TS
*               Calculations for  FONAN > 1 have been introduced.
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
************************************************************************
*
       SUBROUTINE COOLDP
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1412       ,  CCOOL         ),
     &  (XZ1432       ,  DTSFF         ),
     &  (XZ1433       ,  DTSNF         )
       EQUIVALENCE
     &  (XZ1434       ,  DTSNN         ),
     &  (XZ1445       ,  FONAN         ),
     &  (XZ1454       ,  GCOOL         ),
     &  (XZ1458       ,  GFAN          ),
     &  (XZ1461       ,  GOFWF         )
       EQUIVALENCE
     &  (XZ1464       ,  GRAD          ),
     &  (XZ1468       ,  GVVAH         ),
     &  (XZ1488       ,  KCOOL         ),
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1650(1)    ,  P00    (1)    )
       EQUIVALENCE
     &  (XZ1689(1,1,1),  URC    (1,1,1))
*
       REAL      P00(3),URC(4,4,3),AOFAF,AONAF,AONAN,CCOOL,
     &           DTSFF,DTSNF,DTSNN,FONAN,GCOOL,GFAN,GOFWF,GRAD,
     &           GRADNF,GRADNN,GVVAH,PF,PK,VTOT
*
       INTEGER   KCOOL,NWOULI
*
       LOGICAL   BBVR(4)
*
CC*SEBL End of the declarations block.
*
C... Initialisation
*
CC*SBBL Start of the initialisation block.
*
       GCOOL=0.
       CCOOL=0.
       GRAD=0.
       GFAN=0.
       GVVAH=0.
       GOFWF=0.
*
CC*SEBL End of the initialisation block.
*
C... Cooling selection
*
C,,, Cooling
*
       IF(KCOOL.NE.0) THEN
*
          PK=URC(1,1,1)
          IF(BBVR(1)) PK=AMAX1(PK,URC(2,1,1),URC(3,1,1))
          IF(BBVR(2)) PK=AMAX1(PK,URC(1,2,1),URC(1,3,1))
*
          PK=PK*NWOULI
*
C... Mean oil temperature rises (DTSNN,DTSNF,DTSFF)
C... calculated in 'PKL75'
*
C,,, ONAN
*
          IF (KCOOL.EQ.1) THEN
             AONAN=P00(2)+PK
             GRAD=AONAN/22.*(40./DTSNN)**1.33
*
C... OFAF
*
          ELSE IF (KCOOL.EQ.3) THEN
             AOFAF=P00(2)+PK
             GVVAH=AOFAF/(DTSFF*9267.)*2500.
*
C... ONAF
*
          ELSE IF ((KCOOL.EQ.2).OR.(KCOOL.EQ.4)) THEN
             IF (FONAN.LE.1) THEN
                AONAF=P00(2)+PK
                AONAN=P00(2)+PK*FONAN*FONAN
             ELSE
                AONAF=P00(2)+PK*FONAN*FONAN
                AONAN=P00(2)+PK
             ENDIF
             GRADNN=AONAN/22.*(40./DTSNN)**1.33
             GRADNF=AONAF/2.22/DTSNF
             GRAD=AMAX1( GRADNN, GRADNF )
             PF=AONAF/(GRAD*0.111)*(40./DTSNF)
             VTOT=0.0185*GRAD*(1.81/(2.-(PF/1.E+3))-1.)
             GFAN=13.07*VTOT
          END IF
*
       END IF
*
       RETURN
       END
