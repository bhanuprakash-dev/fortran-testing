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
*      Subroutine PAGEUT
*      -----------------
*
C...   Title: Controlling routine for output of results
*
*      Written: XX-XX-XX by A.N.Other     , XXXX
*      Revised: 85-10-18 by Ron Bell      , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell      , TRAFO/IK
*      Revised: 88-11-04 by Per Sãderberg , TRAFO/IK
*      Revised: 90-01-09 by Per Sãderberg , SETFO/IK
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE PAGEUT(BBRES)
*
C... Declarations
*
CC*SBBL Start of the declarations block
*
       include'com1.h'
*
       REAL           RES79(241,6)
       COMMON/KONSRE/ RES79
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ0581(1)    ,  UARR   (1)    ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1387       ,  BBOUT         )
       EQUIVALENCE
     &  (XZ1482       ,  IPAGE         ),
     &  (XZ1483       ,  MNLY          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1587       ,  CHCOST        ),
     &  (XZ1589       ,  DATE          ),
     &  (XZ1604(1)    ,  IDENT  (1)    ),
     &  (XZ1632(1)    ,  RUBCH  (1)    )
*
       REAL      AARR(200),PCLASS
*
       INTEGER   IPAGE,JFC,NG,NWILI, MNLY
*
       DIMENSION TARR(100),RUBCH(15),IDENT(18)
       CHARACTER TARR*8,CHCOST*6,RUBCH*4,IDENT*4,DATE*26
*
       DIMENSION UARR(100),CONVAR(9),TCEPOX(9)
       CHARACTER UARR*4,CONVAR*4,TCEPOX*4
*
       LOGICAL   BBFINE,BBOPCH,BBOUT,FBB,BBRES,BBERR,BBTS,
     &           BBHELP(10), BB79
*
CC*SEBL End of the declarations block
*
       print*,'inside pageut'
       DO 100 IWDG=1,NWILI
          CONVAR(IWDG)=UARR(30+IWDG)
          TCEPOX(IWDG)=UARR(40+IWDG)
  100  CONTINUE
       BBFINE=FBB('PAGEUT',BBERR,BBHELP,BBTS,JFC,TARR(78))
       BBOPCH=FBB('PAGEUT',BBERR,BBHELP,BBTS,JFC,TARR(60))
       BB79  = (MNLY.GE.79 .AND. MNLY.LT.91)
*
C... Output of results
*
       CALL PAGE01
       print*,'after page01'
*
*
C... Further output of results
*
       print*,'before page2'
       IF(BBOPCH) CALL PAGE02(JFC,BBOUT,BBRES)
*
C... Output of masses and costs
*
*
       WRITE(JFC,80 )
*
C... Print costs
*
       IF (BB79)THEN
               print*,'before kuts'
          CALL KUTS(JFC,NWILI,IDENT,DATE,IPAGE,RUBCH,BBFINE,
     &              CONVAR,TCEPOX)
          print*,'before tab79'
          CALL TAB79 (RES79,JFC,CHCOST,NWILI)
          IF (BBFINE) CALL PAGE79
          print*,'after page79'
       ENDIF
*
       RETURN
   80  FORMAT('1    ')
       END
