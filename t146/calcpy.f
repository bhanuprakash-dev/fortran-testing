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
*
************************************************************************
*
*      Subroutine CALCPY
*      -----------------
*
C...   Calculate the loadloss for a specific winding.
*
*      Written: 89-04-13 by Per Sãderberg  , SETFO/IK
*      Revised: 91-09-13 by B-G Bladh      , SETFO/TS
*               Calculation for FONAN > 1 have been introduced.
************************************************************************
*
       SUBROUTINE CALCPY(WPY,IWDG,BB009)
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1445       ,  FONAN         ),
     &  (XZ1121(1)    ,  WLOSSM (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ1061(1)    ,  TCOV1  (1)    ),
     &  (XZ1071(1)    ,  TCOV2  (1)    ),
     &  (XZ2585(1)    ,  HCLAC  (1)    ),
     &  (XZ0911(1)    ,  KWITYP (1)    ),
     &  (XZ2555(1)    ,  KWIND  (1)    )
*
       REAL WPY,WLOSSM(9),ZWIND(9),HWIND(9),DWIND(9),RRWDG(9),
     &      HPART(9),TCOV1(9),TCOV2(9),HCLAC(9),PLCOOL,ALCOOL,
     &      HDISC1
*
       INTEGER IWDG,KWITYP(9),KWIND(9),NODISC
*
       LOGICAL BB009
*
C... BB009 Checks if its ONAN or ONAF.
*
      IF (FONAN.LE.1) THEN
         IF (BB009) THEN
            PLCOOL=FONAN*FONAN*WLOSSM(IWDG)
         ELSE
            PLCOOL=WLOSSM(IWDG)
         ENDIF
      ELSE
         IF (BB009) THEN
            PLCOOL=WLOSSM(IWDG)
         ELSE
            PLCOOL=FONAN*FONAN*WLOSSM(IWDG)
         ENDIF
      ENDIF
*
C... Decide No. of discs for winding of type S its related
C... to No. of turns. For CD or SD its calculated from
C... windingheight and isolation compressed 7%.
*
       HDISC1=HPART(IWDG)+0.93*(TCOV1(IWDG)+TCOV2(IWDG)+
     &        HCLAC(IWDG))
       IF (KWITYP(IWDG).EQ.1) THEN
          NODISC=INT(ZWIND(IWDG)*KWIND(IWDG)+0.99)
       ELSEIF (KWITYP(IWDG).EQ.3.OR.KWITYP(IWDG).EQ.4) THEN
          NODISC=INT(HWIND(IWDG)/HDISC1+0.99)
       ELSE
          NODISC=0
          HDISC1=0.0
       ENDIF
*
C... ALCOOL is area in connection with oil on winding.
C... Isolation compressed 7%.
*
       PI=4*ATAN(1.0)
       ALCOOL=2*(RRWDG(IWDG)+HDISC1)*(PI*DWIND(IWDG)*0.70)*
     &            NODISC
*
C... The solution of loadlosses per area unit is calculated:
*
       IF (ALCOOL.LT.1.E-5) THEN
          WPY=0
       ELSE
          WPY=PLCOOL/ALCOOL
       ENDIF
*
       RETURN
       END
