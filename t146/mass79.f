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
*      Subroutine MASS79
*      -----------------
*
C...   Title:  Calculation of masses using MNL-79 basis
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-07-18 by B-G Bladh  , SETFO/TS
*        RPARTRR standard value is '2'  for LR and OR strand insulation
*      Revised: 91-10-07 by B-G Bladh  , STEFO/TS
*        Volume for Booster is included to volume for active part
*        Mass of booster is included in goods for cleats and leads
*        terminal 2 (MABOOS -> GG32(2)
*        Free oil (GSN142) are reduced with 960.* VOBOOS (booster vol.)
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*      Revised: 92-01-10 by B-G Bladh    SETFO/TS
*             Oil mass are returned to GOIL
*
************************************************************************
*
       SUBROUTINE MASS79
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       COMMON /V79/
     & GS11(9),    GS23,       GS25,        GS31(4),    GS32(4),
     & GS41(9),    GS42,       GS45,        GS46,
     & GS47,       GS48,
     & GS51,       GS52,       GS53,        GS054,
     & GS61,       GS71,       GS72,        GS81,
     & GS91,       GS92,       GS102,       GS111,      GS112,
     & GS121(9),   GS122(9),   GS131(9),    GS132(9),
     & GS133(9),   GS134(9),   GS1351(9),   GS1352(9),
     & GS136(9),
     & GS141,      GS142,      GS143,       GS211,      GS212,
     & GS221,      GS222,      GS241,       GS242,      GS1210
       COMMON /VG79/
     & GG21,        GG22,        GG23,         GG24,        GG32(4),
     & GG49,        GG51,        GG52,         GG53,        GG054,
     & GG54,        GG55,        GG61,         GG63,        GG71,
     & GG72,        GG74,        GG81,         GG83,
     & GG91,        GG92,        GG94,         GG101,       GG102,
     & GG111,       GG1210 ,     GG151,        GG152
*
       EQUIVALENCE
     &  (XZ0181(1)    ,  BARR   (1)    ),
     &  (XZ0231(1)    ,  DARR   (1)    ),
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ0581(1)    ,  UARR   (1)    ),
     &  (XZ0651(1)    ,  ACOND  (1)    )
       EQUIVALENCE
     &  (XZ0671(1)    ,  BDUCT  (1)    ),
     &  (XZ0681(1)    ,  BPART  (1)    ),
     &  (XZ0721(1)    ,  IPISOL (1)    ),
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ0871(1)    ,  HPART  (1)    )
       EQUIVALENCE
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ0911(1)    ,  KWITYP (1)    ),
     &  (XZ0921(1)    ,  NGROUP (1)    ),
     &  (XZ0931(1)    ,  NLOOP  (1)    ),
     &  (XZ0941(1)    ,  NOUCO  (1)    )
       EQUIVALENCE
     &  (XZ0981(1)    ,  RAACON (1)    ),
     &  (XZ0991(1)    ,  RPART  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ1061(1)    ,  TCOV1  (1)    ),
     &  (XZ1071(1)    ,  TCOV2  (1)    )
       EQUIVALENCE
     &  (XZ1091(1)    ,  TSPIN  (1)    ),
     &  (XZ1161(1)    ,  ZCODU  (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1215(1)    ,  KCON   (1)    ),
     &  (XZ1219(1)    ,  KTYPRW (1)    )
       EQUIVALENCE
     &  (XZ1227(1)    ,  NMSTEP (1)    ),
     &  (XZ1231(1)    ,  NPSTEP (1)    ),
     &  (XZ1263(1)    ,  SRATEP (1)    ),
     &  (XZ1267(1)    ,  UDIM   (1)    ),
     &  (XZ1271(1,1)  ,  UN     (1,1)  )
       EQUIVALENCE
     &  (XZ1300       ,  BB051         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1366(1)    ,  BBVR   (1)    )
       EQUIVALENCE
     &  (XZ1371       ,  BB132         ),
     &  (XZ1373       ,  BBAUTO        ),
     &  (XZ1377       ,  BBEND         ),
     &  (XZ1380       ,  BBEXAC        ),
     &  (XZ1381       ,  BBFLDS        )
       EQUIVALENCE
     &  (XZ1385       ,  BBLT10        ),
     &  (XZ1390       ,  BBSLIM        ),
     &  (XZ1392       ,  BBUPTR        ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1397       ,  ACOVER        )
       EQUIVALENCE
     &  (XZ1398       ,  AFIELD        ),
     &  (XZ1400       ,  ANDEL         ),
     &  (XZ1402       ,  ASECL         ),
     &  (XZ1403       ,  ASHELL        ),
     &  (XZ1410       ,  BTANK         )
       EQUIVALENCE
     &  (XZ1423       ,  DCOCOV        ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1428       ,  DOUTW         ),
     &  (XZ1429       ,  DPHAS         ),
     &  (XZ1430       ,  DPHSL         )
       EQUIVALENCE
     &  (XZ1435       ,  DWITA         ),
     &  (XZ1438       ,  EXTANK        ),
     &  (XZ1439       ,  F1TANK        ),
     &  (XZ1440       ,  F2TANK        ),
     &  (XZ1441       ,  F3TANK        )
       EQUIVALENCE
     &  (XZ1445       ,  FONAN         ),
     &  (XZ1455       ,  GCORLA        ),
     &  (XZ1457       ,  GCOVER        ),
     &  (XZ1458       ,  GFAN          ),
     &  (XZ1461       ,  GOFWF         )
       EQUIVALENCE
     &  (XZ1464       ,  GRAD          ),
     &  (XZ1465       ,  GTANK         ),
     &  (XZ1468       ,  GVVAH         ),
     &  (XZ1471       ,  HCORE         ),
     &  (XZ1472       ,  HLIMB         )
       EQUIVALENCE
     &  (XZ1474       ,  HTANK         ),
     &  (XZ1477       ,  HYOKE         ),
     &  (XZ1479       ,  ISAM          ),
     &  (XZ1481(1)    ,  NCONDU (1)    ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1488       ,  KCOOL         )
       EQUIVALENCE
     &  (XZ1490       ,  KCOOL2        ),
     &  (XZ1491       ,  KCORE         ),
     &  (XZ1492       ,  KTAN79        ),
     &  (XZ1493       ,  KTANK         ),
     &  (XZ1494       ,  NCLA          )
       EQUIVALENCE
     &  (XZ1496       ,  NCOOLC        ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1501       ,  NPHAS         ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1509       ,  NWOULI        )
       EQUIVALENCE
     &  (XZ1515       ,  PLIMB         ),
     &  (XZ1527       ,  ISTGRD        ),
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1530       ,  RLTRAN        ),
     &  (XZ1531       ,  RLYOKE        )
       EQUIVALENCE
     &  (XZ1542       ,  SNOML         ),
     &  (XZ1547       ,  TANKDI        ),
     &  (XZ1550       ,  TCORE         ),
     &  (XZ1557       ,  TWSUP         )
       EQUIVALENCE
     &  (XZ1558       ,  TYPCOR        ),
     &  (XZ1562       ,  VACTP         ),
     &  (XZ1563       ,  VTANK         ),
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ1650(1)    ,  P00    (1)    )
       EQUIVALENCE
     &  (XZ1689(1,1,1),  URC    (1,1,1)),
     &  (XZ2581(1)    ,  ZLAG   (1)    ),
     &  (XZ2585(1)    ,  HCLAC  (1)    ),
     &  (XZ2727       ,  MABOOS        ),
     &  (XZ2728       ,  VOBOOS        ),
     &  (XZ2737       ,  GOIL          )
*
       REAL    ACOND(9),BARR(100),BDUCT(9),BPART(9),DARR(100),
     &         DWIND(9),HPART(9),HWIND(9),ACOVER,AFIELD,ANDEL,
     &         P00(3),RAACON(9),RPART(9),RRWDG(9),ASECL,ASHELL,
     &         SRATEP(4),TCOV1(9),TCOV2(9),TSPIN(9),UDIM(4),
     &         UN(4,4),URC(4,4,3),RAA(9),ZCODU(9),ZWIND(9),
     &         HSPOL(9),HCLAC(9),ZLAG(9),ZCOIAR(9),BOOST,BTANK,
     &         CORLAN,CORLAX,CURR,CUTOT,DBW,DCOCOV,DCORE,DOUTW,
     &         DPHAS,DPHSL,DWITA,EXTANK,FACTOR,FONAN,F1TANK,
     &         F2TANK,F3TANK,GASEC,GCORLA,GCOVER,GFAN,GOFWF,GRAD,GOIL,
     &         GTANK,GVVAH,HCORE,HLIMB,HLP,HTANK,HYOKE,PCSNED,
     &         PK,PLIMB,PLOSS,PLOSS1,PSPIN,RGUID,RJH,RLCON,
     &         RLTANK,RLTRAN,RLYOKE,RPRTAR,RPRTRR,RRAD,SNOML,
     &         TANKDI,TCLAC,TCORE,TWSUP,TYPCOR,UNX,VACTP,VTANK,
     &         VVAH,WWINDY,XT,ZWOULI, VOBOOS, MABOOS
*
       INTEGER KCON(4),NPSTEP(4),IPISOL(9),NGROUP(9),ISTGRD,IWDG,JFC,
     &         KTYPRW(4),KWITYP(9),NLOOP(9),NMSTEP(4),NOUCO(9),
     &         ISAM,JTML,J1,KCOOL,KCOOL1,KCOOL2,KCORE,NCONDU(9),
     &         KTANK,KTAN79,KTML,KX031,NCLA,NCON,NCOOLC,NG,NPHAS,
     &         NWILI,NWOULI
*
       DIMENSION TARR(100),UARR(100)
       CHARACTER TARR*8,UARR*4,OPTTNK*4
*
       LOGICAL   BBVR(4),BB048,BBPRNT,
     &           BB03,BB051,BB07,BB08,BB132,BBAUTO,BBEND,BBEXAC,
     &           BBFLDS,BBHELP(10),BBLT10,BBSLIM,BBUPTR,BBVFR
*
       DATA      BB07 /.FALSE./,
     &           RAA,HSPOL,           ZCOIAR /27*0./,
     &           FACTOR /1.E+3/
*
CC*SEBL End of the declarations block.
*
*
C...Cooling equipment on tank
       KCOOL1 = 1
*
C... Tap changer on short tank side
       BB03 = .FALSE.
*
C... Conservator located on main tank
       BB08 = .TRUE.
*
       BBPRNT=.FALSE.
       IF (BBEND.AND.BBHELP(5)) BBPRNT=.TRUE.
*
C... MASS01 Core
*
CC*SBBL Start of the MASS01 Core block.
*      
       WRITE(*,*) 'MASS79 GCORLA =', GCORLA
       WRITE(*,*) 'MASS79 ANDEL =', ANDEL
       CORLAN=GCORLA*(1.-ANDEL)
       CORLAX=GCORLA*ANDEL
       GASEC=ASECL*0.048
*
C... Yoke clamps
*
       CALL PBALK(BBPRNT,JFC,ANINT(DCORE*FACTOR),HYOKE*FACTOR,
     &            HLIMB*FACTOR,PLIMB*FACTOR,ANINT(TWSUP*FACTOR),
     &            BBEXAC,SNOML/1.E+6,RLYOKE*FACTOR,
     &            TCORE*FACTOR,KCORE,NINT(TYPCOR),BBSLIM,GS211)
*
C... Core
*
       CALL MASS01(BBPRNT,JFC,NINT(TYPCOR),ISTGRD,NCLA,
     &             HLIMB*FACTOR,ANINT(DCORE*FACTOR),HYOKE*FACTOR,
     &             RLYOKE*FACTOR,FLOAT(NCOOLC),CORLAN,CORLAX,GASEC)
*
CC*SEBL End of the MASS01 Core block.
*
C... MASS02  Windings
C... Adjustment height = RJH
*
CC*SBBL Start of the MASS02 Windings block.
*
       RJH=HLIMB*FACTOR-0.34*(FACTOR*DCORE)**0.83
       DO  15 IWDG=1,NWILI
       ZCODU(IWDG)=RRWDG(IWDG)*40.-1.
       IF(KWITYP(IWDG).GE.3.AND.KWITYP(IWDG).LE.5)
     & ZCODU(IWDG)=RRWDG(IWDG)*20.-1.
       IF(ZCODU(IWDG).LT.0.) ZCODU(IWDG)=0.
       IF(BARR(IWDG).GE.0.)  ZCODU(IWDG)=BARR(IWDG)
       TCLAC=0.
*
C... Calculation of clacks and parallel parts
*
       RPRTAR=1.
       RPRTRR=1.
C...  Control if the strands per cable is for OR or LR.
C...  If so it willimply 2 strands per cable
       IF(IPISOL(IWDG).EQ.2.OR.IPISOL(IWDG).EQ.3) RPRTRR=2.
*
       IF(TSPIN(IWDG).GT.0.001.AND.KWITYP(IWDG).LT.6) RPRTRR=2.
       RPART(IWDG)=RPRTRR
       PSPIN=0.5E-3
       IF(IPISOL(IWDG).EQ.2) PSPIN=0.15E-3
       IF(IPISOL(IWDG).EQ.3) PSPIN=0.18E-3
       IF(DARR(30+IWDG).GT.0.) PSPIN=DARR(30+IWDG)/1.E+3
*
C,,, FOR KWITYP <> 2 AND <> 7
*
       IF(KWITYP(IWDG).NE.2.AND.KWITYP(IWDG).NE.7) THEN
          CUTOT=ACOND(IWDG)*ZWIND(IWDG)
          HLP=BPART(IWDG)+(TSPIN(IWDG)-PSPIN)/RPRTRR+PSPIN
          CURR=(RRWDG(IWDG)-5.*ZCODU(IWDG)/1.E+3)/HLP*BPART(IWDG)
          TCLAC=HWIND(IWDG)-
     &          CUTOT/CURR/HPART(IWDG)*(HPART(IWDG)+TSPIN(IWDG))
          TCLAC=AMAX1(0.,TCLAC)
       END IF
*
C... Outer cross-overs for SD windings
*
       NOUCO(IWDG)=0
       IF(KWITYP(IWDG).EQ.4)
     & NOUCO(IWDG)=0.5+(HWIND(IWDG)-TCLAC)/(HPART(IWDG)+TSPIN(IWDG))/2.
       NOUCO(IWDG)=NOUCO(IWDG)*NWOULI
       RGUID=DARR(IWDG)
       IF (RGUID.GT.1.) ZCODU(IWDG)=0.
       PCSNED=DARR(10+IWDG)
       write(*,*)"79HPART",HPART
       CALL MASS02(BBPRNT,JFC,IWDG,ANINT(DCORE*FACTOR),HLIMB*FACTOR,
     &             HWIND(IWDG)*FACTOR,RJH,
     &             KWITYP(IWDG),PLIMB*FACTOR,ACOND(IWDG)*FACTOR*FACTOR,
     &             RAACON(IWDG)/FACTOR,NCONDU(IWDG),NWOULI,
     &             HPART(IWDG)*FACTOR,BPART(IWDG)*FACTOR,RPRTAR,RPRTRR,
     &             TCOV1(IWDG)*FACTOR,
     &             TCOV2(IWDG)*FACTOR,FLOAT(NLOOP(IWDG)),
     &             ZWIND(IWDG),DWIND(IWDG)*FACTOR,RRWDG(IWDG)*FACTOR,
     &             BDUCT(IWDG)*FACTOR,
     &             ISAM,TCLAC*FACTOR,RGUID,ZCODU(IWDG),PCSNED,
     &             HSPOL(IWDG),HCLAC(IWDG),ZLAG(IWDG),ZCOIAR(IWDG),
     &             NGROUP(IWDG),IPISOL(IWDG))
   15  CONTINUE
*
CC*SEBL End of the MASS02 Windings block.
*
C... MASS03  Cleats & leads
*
CC*SBBL Start of the MASS03 Cleats & Leads block.
*
       DO 20 J1=1,NG
       JTML=J1
*
C... Set KX031
*
C,,, 3 Phase
*
       IF(NPHAS.EQ.3) THEN
          KX031=5
          IF(KCON(JTML).EQ.3)  KX031=8
          IF(KCON(JTML).EQ.11) KX031=6
          IF(KCON(JTML).EQ.0) KX031=3
*
C... 1 Phase
*
       ELSE
          KX031=NWOULI
       END IF
       NCON=0
       RLCON=0.
*
C... Set KTML
*
C,,, VFR
*
       IF(BBVFR) THEN
          KTML=1
          IF(BBAUTO.AND..NOT.BBUPTR)  KTML=2
*
C... CFR
*
       ELSE
          KTML=JTML
       END IF
*
C... Calculate NCON
*
C,,, VFR or regulated terminal
*
       IF (BBVFR.OR.BBVR(JTML)) THEN
*
C... Calculate NCON
*
C,,, JTML=KTML
*
          IF(JTML.EQ.KTML) THEN
             NCON=NPSTEP(KTML)+NMSTEP(KTML)
             IF(KTYPRW(KTML).EQ.2)  NCON=NCON/2
             IF(KTYPRW(KTML).EQ.3)  NCON=2+NCON/2
             NCON=NCON+1
             RLCON=600.*RLYOKE+1000.*HLIMB
             IF(BB03) RLCON=1200.*HLIMB
          END IF
       END IF
*
       UNX=UN(JTML,1)
       IF(BBVR(JTML))  UNX=UN(JTML,3)
       BOOST=1.
*
C... Cleats & leads
*
       CALL MASS03(BBPRNT,JFC,JTML,KX031,UDIM(JTML),SRATEP(JTML)/UNX,
     &             HLIMB*FACTOR,NWOULI,NCON,RLCON,BOOST)
   20  CONTINUE
*
C...   Correction for BOOSTER. Mass included in goods for cleat and lds.
       GG32(2) = GG32(2) + MABOOS
*
CC*SEBL End of the MASS03 Cleats & Leads block.
*
C... MASS04  Active part : Others
*
CC*SBBL Start of the MASS04 Active part : others block.
*
       DBW=DPHAS
       IF(BBSLIM.AND.NWOULI.EQ.1)DBW=DPHSL
       WWINDY=RRWDG(NWILI)
       BB048=.FALSE.
       CALL MASS04(BBPRNT,JFC,DBW*FACTOR,DOUTW*FACTOR,WWINDY*FACTOR,
     &             RJH,NWOULI,NCLA,
     &             CORLAN,CORLAX,NINT(TYPCOR),ANINT(DCORE*FACTOR),
     &             DPHSL*FACTOR,HLIMB*FACTOR,ANINT(TWSUP*FACTOR),BB048)
*
CC*SEBL End of the MASS04 Active part : others block.
*
C... MASS05  Tank, Cooling equipment, Cover
*
CC*SBBL Start of the MASS05  Tank, Cooling equipment, Cover block.
*
       PK=URC(1,1,1)
       IF(BBVR(1))  PK=AMAX1(PK,URC(2,1,1),URC(3,1,1))
       IF(BBVR(2))  PK=AMAX1(PK,URC(1,2,1),URC(1,3,1))
*
C... Calculation of tank dimensions
*
       OPTTNK=UARR(20)
*
C... Set tank dimensions
*
C,,, If they are to be optimised
*
       IF (OPTTNK(1:1).EQ.'Y') THEN
          RLTANK=RLTRAN+EXTANK
          BTANK=DOUTW+2.*DWITA
          HTANK=HCORE+DCOCOV
       END IF
*
C... Set tank dimensions to zero
*
C,,, NO TANK
*
       IF(KTANK.EQ.0) THEN
          RLTANK=0.
          BTANK=0.
          HTANK=0.
       END IF
       ACOVER=(F1TANK*RLTANK+F2TANK*BTANK)*BTANK
       ASHELL=(RLTANK+F3TANK*BTANK)*HTANK*2.
       IF(KTANK.EQ.0) ACOVER=0.
       IF(KTANK.EQ.0) ASHELL=0.
       write(*,*)"ACOVER",ACOVER
       write(*,*)"HTANK",HTANK
       VTANK=ACOVER*HTANK
       VVAH=GVVAH/1250.
       RRAD=GRAD/500.
       IF(KTANK.NE.0)
*
C... Tank mass
*
     & CALL TANK79(BBPRNT,JFC,HTANK,BTANK,RLTANK,VVAH,RRAD,VTANK,
     &             ASHELL,ACOVER,KTAN79,TANKDI,GTANK,BBLT10,BBEXAC)
*
C... Cooler variables
*
       PLOSS1=PK*ZWOULI+P00(2)
       IF(KCOOL.EQ.2.OR.KCOOL.EQ.4)  PK=FONAN*FONAN*PK
       PLOSS=PK*ZWOULI+P00(2)
       IF(PLOSS.LT.0.)  PLOSS=0.
       XT=0.
       IF(BB132)  XT=2000.
       AFIELD=0.
       IF(BBFLDS) AFIELD=2.*RLTANK*HTANK
*
C... Tank, Cooling equipment, Cover
*
       CALL MASS05(BBPRNT,JFC,BB051,GTANK,VTANK*FACTOR,ACOVER,
     &             ASHELL,BB07,GCOVER,KCOOL1,PLOSS,AFIELD,
     &             GRAD,XT*(BTANK+RLTANK),KTAN79)
*
CC*SEBL End of the MASS05  Tank, Cooling equipment, Cover block.
*
C... MASS07  Final assembly, Dis-assembly
*
CC*SBBL Start of the MASS07  Final assembly, Dis-assembly block.
*
       DO 42 IWDG=1,NWILI
   42  RAA(IWDG)=RAACON(IWDG)/1.E+3
*
C... Final assembly, Dis-assembly
*
       CALL MASS07(BBPRNT,JFC,NCLA,NWILI,NG,VTANK*FACTOR,
     &             RLTANK*FACTOR,BTANK*FACTOR,HTANK*FACTOR,BB132,
     &             RAA,VACTP,GVVAH,GOFWF)
*
C... Correction for BOOSTER
       VACTP=VACTP/FACTOR+VOBOOS
C... Free oil:
       GS142 = GS142 - VOBOOS * 960.
*
*
C... MASS06  Conservator, Signal leads
*
       CALL MASS06(BBPRNT,JFC,BB08,KCOOL1,PLOSS1,KCOOL2,GFAN)
*
       GOIL = GS142
       write(*,*) "GG24",GG24
       write(*,*) "VOBOOS",VOBOOS
       write(*,*) "GG32",GG32
       write(*,*) "GSN81",GS81
       WRITE(*,*) "GSN142",GS142
       write(*,*) "GG var", GG21,        GG22,        GG23,         GG24,        GG32(4),
     & GG49,        GG51,        GG52,         GG53,        GG054,
     & GG54,        GG55,        GG61,         GG63,        GG71,
     & GG72,        GG74,        GG81,         GG83,
     & GG91,        GG92,        GG94,         GG101,       GG102,
     & GG111,       GG1210 ,     GG151,        GG152
*
       RETURN
       END
