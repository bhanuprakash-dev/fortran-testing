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
*      Subroutine COOLEQ
*      -----------------
*
C...   Idea of simplified calculation of No. of radiators.
*
*      Written: 89-04-12 by Per Sãderberg  , SETFO/IK
*      Revised:
*
************************************************************************
*
       SUBROUTINE COOLEQ
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1410       ,  BTANK         ),
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1488       ,  KCOOL         ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ1474       ,  HTANK         )
       EQUIVALENCE
     &  (XZ1689(1,1,1),  URC    (1,1,1)),
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1259(1)    ,  SRATE  (1)    ),
     &  (XZ1650(1)    ,  P00    (1)    )
*
       LOGICAL BBONAF,BBRAD,BB009
*
       REAL HDIFFT,HDIFFB,WPYMAX,WPY,DTLM,DTMMAX,PRCOOL,
     &      CONST1,CONST2,CONST3,ATCOOL,PRAD,PFCOOL,QMIN,
     &      URC(4,4,3),ZWOULI,SRATE(4),P00(3),PKCOOL,PTOT,
     &      URFCTR
*
       INTEGER MAXRAD,IRADH,NOW,NRADC,NRMAX,NFAN,NG,
     &         IHTANK,IRLTNK,IBTANK
*
       CHARACTER*10 CRD,CAF,CKM
       DIMENSION  TARR(100)
       CHARACTER*8  TARR
*
C... Choose maximum radiatorheigth.
*
       BBONAF=(KCOOL.EQ.2.OR.KCOOL.EQ.4)
       CRD=' '
       CALL CMOVD(CRD,'RDH ',1,8)
       CAF=' '
       CALL CMOVD(CAF,'CAF ',1,8)
       CKM=' '
       CALL CMOVD(CKM,'CKM ',1,9)
       IHTANK=NINT(HTANK*1.E+3)
       IBTANK=NINT(BTANK*1.E+3)
       IRLTNK=NINT(RLTANK*1.E+3)
       URFCTR=ZWOULI/1.E+3
       HDIFFT=IHTANK-250
       HDIFFB=450
       MAXRAD=HDIFFT-HDIFFB
       IF (MAXRAD.GE.2800) THEN
          IRADH=2800
       ELSEIF (MAXRAD.GE.2300) THEN
          IRADH=2300
       ELSEIF (MAXRAD.GE.1800) THEN
          IRADH=1800
       ELSEIF (MAXRAD.GE.1500) THEN
          IRADH=1500
       ELSEIF (MAXRAD.GE.1200) THEN
          IRADH=1200
       ELSE
          IRADH=0
       ENDIF
*
C... Decide which winding has the most losses per area unit (ONAN).
C... Its not necessarily the winding with most losses.
*
       WPYMAX=0.
       BB009=.TRUE.
       DO 100 IWDG=1,NWILI
          CALL CALCPY(WPY,IWDG,BB009)
          IF (WPY.GT.WPYMAX) THEN
             WPYMAX=WPY
             NOW=IWDG
          ENDIF
  100  CONTINUE
*
C... Calculate the surface load WPY (ONAN).
*
       WPY=WPYMAX
*
C... For oilguided windings at ONAN
C... we approximate the windingtemperature increase
C... over meanwindingoiltemperature in tank.
*
       DTLM=6+WPY*15/2000
       DTMMAX=59-DTLM
*
C... Depending on radiators heigth decide constant value on
C... PRCOOL cooling ability at 40 degrees celsius (kW).
*
       IF (IRADH.EQ.2800) THEN
          PRCOOL=17.8
       ELSEIF (IRADH.EQ.2300) THEN
          PRCOOL=15.3
       ELSEIF (IRADH.EQ.1800) THEN
          PRCOOL=12.7
       ELSEIF (IRADH.EQ.1500) THEN
          PRCOOL=11.0
       ELSEIF (IRADH.EQ.1200) THEN
          PRCOOL=9.2
       ELSE
          PRCOOL=0.0000001
       ENDIF
*
C... Set constants in formula for calculation of radiators.
C    CONST1=0.3 (Painted tank)
C    CONST2=0.6 (Part of tank not hidden by radiators)
C    CONST3=1.0 (Distance between radiators=80mm)
*
       CONST1=0.3
       CONST2=0.6
       CONST3=1.0
*
C... Total losses at selfcooling PTOT (kW).
C... Area of surface of tank ATCOOL (m2).
C... TARR(75)=YES means soundscreen on tank.
*
       CALL PCOOL(URC,URFCTR,NG,SRATE,PKCOOL)
       PTOT=PKCOOL+P00(1)/1.E+3
       IF (TARR(75).EQ.'YES      ') THEN
          ATCOOL=RLTANK*BTANK
       ELSE
          ATCOOL=RLTANK*BTANK+2*(RLTANK+BTANK)*HTANK
       ENDIF
*
C... Formula for calculation No. of radiators.
*
       NRADC=(PTOT*(40/DTMMAX)**1.33-(0.3+CONST1*CONST2)*ATCOOL)/
     &       PRCOOL/CONST3
*
C... Calculate maximum no. of radiators on tank.
*
       NRMAX=INT(((5*IRLTNK-4*IBTANK-2500)/3000)*2+10)
       IF (NRMAX.GT.12) NRMAX=12
       IF (NRADC.GT.NRMAX) THEN
          CALL IMOVD(NRADC,'NRD ',1,6)
          CRD='Warning'
          CALL CMOVD(CRD,'RDH ',1,8)
          CAF='too many'
          CALL CMOVD(CAF,'CAF ',1,8)
          CKM='radiators'
          CALL CMOVD(CKM,'CKM ',1,9)
          BBRAD=.TRUE.
       ELSE
          CALL IMOVD(IRADH,'RDH ',1,6)
          CALL IMOVD(NRADC,'NRD ',1,6)
          IF (BBONAF) THEN
*
C... Decide which winding has the most losses per area unit (ONAF).
C... Its not necessarily the winding with most losses.
*
             WPYMAX=0.
             BB009=.FALSE.
             DO 200 IWDG=1,NWILI
                CALL CALCPY(WPY,IWDG,BB009)
                IF (WPY.GT.WPYMAX) THEN
                   WPYMAX=WPY
                   NOW=IWDG
                ENDIF
  200        CONTINUE
*
C... Calculate the surface load WPY (ONAF).
*
             WPY=WPYMAX
*
C... For oilguided windings at ONAF
C... we approximate the windingtemperature increase
C... over meanwindingoiltemperature in tank.
*
             DTLM=6+WPY*15/2000
             DTMMAX=59-DTLM
*
C... By radiators cooled power.
*
             PRAD=PTOT-(0.3+CONST1*CONST2)*ATCOOL*(DTMMAX/40)**1.33
*
C... Coolingsurface for radiators with 32 sections ARCOOL (m2).
*
             IF (IRADH.EQ.2800) THEN
                ARCOOL=97.6
             ELSEIF (IRADH.EQ.2300) THEN
                ARCOOL=79.8
             ELSEIF (IRADH.EQ.1800) THEN
                ARCOOL=62.2
             ELSEIF (IRADH.EQ.1500) THEN
                ARCOOL=51.5
             ELSEIF (IRADH.EQ.1200) THEN
                ARCOOL=41.0
             ELSE
                ARCOOL=0.0000001
             ENDIF
*
C... Coolingpower for fans.
*
             PFCOOL=PRAD*40/NRADC/ARCOOL/DTMMAX
*
C... Minimum airflow needed (m3/s)
*
             QMIN=NRADC*ARCOOL*(PFCOOL-0.2)/10.4/(1.2-PFCOOL)
*
C... Control of possibility to place fans.
*
             IF (NRADC.LE.4) THEN
                NFAN=1
             ELSEIF (NRADC.LE.8) THEN
                NFAN=2
             ELSEIF (NRADC.EQ.9) THEN
                NFAN=3
             ELSEIF (NRADC.LE.12) THEN
                NFAN=4
             ELSE
                NFAN=0
             ENDIF
*
C... If radiatorheigth 2800 is used its possible to double No. of fans
C... by having 2 over each other.
*
             IF (IRADH.EQ.2800) NFAN=2*NFAN
*
C... For PMA140 and 550ppm the airflow is 14.7 m3/s
*
             QMAX=14.7
             IF (QMIN.LT.NFAN*QMAX) THEN
                CALL RMOVD(QMIN,'CKM ',1,5,1)
             ELSE
                CRD='Warning  '
                CALL CMOVD(CRD,'RDH ',1,9)
                CAF='to many  '
                CALL CMOVD(CAF,'CAF ',1,9)
                CKM='fans     '
                CALL CMOVD(CKM,'CKM ',1,9)
             ENDIF
          ENDIF
       ENDIF
*
       RETURN
       END
