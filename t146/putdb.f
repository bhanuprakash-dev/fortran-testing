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
*      Subroutine PUTDB
*      ----------------
*
C...   Title: Store input and calculated data for program T31146
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE PUTDB(BBSTOR)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL           RES79(241,6)
       COMMON/KONSRE/ RES79
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
     & GS221,      GS222,      GS241,       GS242,      GN1210
       COMMON /VG79/
     & GG21,        GG22,        GG23,         GG24,        GG32(4),
     & GG49,        GG51,        GG52,         GG53,        GG054,
     & GG54,        GG55,        GG61,         GG63,        GG71,
     & GG72,        GG74,        GG81,         GG83,
     & GG91,        GG92,        GG94,         GG101,       GG102,
     & GG111,       GG1210 ,     GG151,        GG152
*
       COMMON
     & /CVIKT7/  GNS131,GNG131,
     &           GOILF,GOILA,
     &           GNS13A,GNS13S,GGS13A,GGS13S,GNG132
*
       include'com1.h'
*
       include'cominp.h'
*
       EQUIVALENCE
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0681(1)    ,  BPARTX (1)    ),
     &  (XZ0692(1,1)  ,  CURPRT (1,1)  ),
     &  (XZ0693(1,1)  ,  CURMNT (1,1)  ),
     &  (XZ0694(1,1)  ,  CURMXT (1,1)  ),
     &  (XZ0695(1,1)  ,  CURSPT (1,1)  ),
     &  (XZ0871(1)    ,  HPARTX (1)    ),
     &  (XZ0701(1)    ,  CURDEN (1)    ),
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ0791(1)    ,  FILLFX (1)    ),
     &  (XZ0861(1)    ,  GWINCO (1)    )
       EQUIVALENCE
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    ),
     &  (XZ1011(1)    ,  RWIND  (1)    ),
     &  (XZ1021(1)    ,  SHTCUR (1)    ),
     &  (XZ1041(1)    ,  STRWMM (1)    ),
     &  (XZ1051(1)    ,  STRWMP (1)    ),
     &  (XZ1101(1)    ,  VOLTUT (1)    ),
     &  (XZ1111(1)    ,  WLOSS  (1)    ),
     &  (XZ1121(1)    ,  WLOSSM (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    )
       EQUIVALENCE
     &  (XZ1259(1)    ,  SRATEX (1)    ),
     &  (XZ1272(1)    ,  UNTAP  (1)    ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1396       ,  ACORE         ),
     &  (XZ1405       ,  BLIMBX        ),
     &  (XZ1407       ,  BMAXPU        )
       EQUIVALENCE
     &  (XZ1410       ,  BTANKX        ),
     &  (XZ1426       ,  DCOREX        ),
     &  (XZ1452       ,  GACTP         ),
     &  (XZ1455       ,  GCORLA        ),
     &  (XZ1457       ,  GCOVER        ),
     &  (XZ1465       ,  GTANK         ),
     &  (XZ1466       ,  GTRP          ),
     &  (XZ1471       ,  HCORE         ),
     &  (XZ1472       ,  HLIMBX        )
       EQUIVALENCE
     &  (XZ1474       ,  HTANKX        ),
     &  (XZ1496       ,  NCOOLC        ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1509       ,  NWOULI        )
       EQUIVALENCE
     &  (XZ1515       ,  PLIMB         ),
     &  (XZ1518       ,  PLSL          ),
     &  (XZ1528       ,  RLTANK        ),
     &  (XZ1531       ,  RLYOKE        ),
     &  (XZ1539       ,  SEQU2W        )
       EQUIVALENCE
     &  (XZ1559       ,  U0            ),
     &  (XZ1562       ,  VACTP         ),
     &  (XZ1567       ,  YOKAMP        ),
     &  (XZ1571       ,  ZWOULI        ),
     &  (XZ1574(1,1)  ,  PCUT   (1,1)  ),
     &  (XZ1575(1,1)  ,  PEDT   (1,1)  ),
     &  (XZ1576(1,1)  ,  POEDT  (1,1)  ),
     &  (XZ1577(1,1)  ,  PCEOT  (1,1)  ),
     &  (XZ1650(1)    ,  P00    (1)    ),
     &  (XZ1651(1)    ,  PNOLO  (1)    ),
     &  (XZ1653(1)    ,  Q00    (1)    ),
     &  (XZ1654(1)    ,  RINOLO (1)    )
       EQUIVALENCE
     &  (XZ1686(1)    ,  SOUND0 (1)    ),
     &  (XZ1687(1)    ,  SOUND  (1)    ),
     &  (XZ2763       ,  SPRED         ),
     &  (XZ1689(1,1,1),  URC    (1,1,1)),
     &  (XZ1690(1,1,1),  UXC    (1,1,1))
       EQUIVALENCE
     &  (XZ2245(1)    ,  WLOSSE (1)    ),
     &  (XZ2254(1)    ,  WLOSSR (1)    )
*
       REAL       ACOND(9),CURDEN(9),DWIND(9),FILLFX(9),HWIND(9),
     &            P00(3),Q00(3),RRWDG(9),SOUND0(3),SRATEX(4),
     &            URC(4,4,3),UXC(4,4,3),UNTAP(24),
     &            WLOSS(9),WLOSSM(9),VOLTUT(9),
     &            ZWIND(9),GWINCO(9),URC12X(16),URC13X(16),URC23X(16),
     &            UXC12X(16),UXC13X(16),UXC23X(16),DWINDX(9),RWIND(9),
     &            ACONDX(9),RRWDGX(9),HWINDX(9),CURDEX(9),BPARTX(9),
     &            WLOSSX(9),WLOSMX(9),GWINDA(9),GWINDC(9),HPARTX(9),
     &            ACORE,BLIMBX,BMAXPU,BTANKX,DCOREX,DCRE,GACTP,GCORLA,
     &            GCOVER,HCORE,HLIMBX,HLMB,HTANKX,PCLASS,PKCOOL,
     &            PLMB,PLIMB,PLSL,PSLMB,REACHX,Q00SRA,RLTANK,BLIMBS,
     &            RLYOKE,SEQU,SEQU2W,URFCTR,U0,VACTP,YOKA,
     &            YOKAMP,ZWOULI,HPARTS(9),BPARTS(9),STRWMM(9),STRWMP(9),
     &            WLOSSE(9),WLOSSR(9),SHTCUR(9),SPRED,
     &            PNOLO(5),SOUND(5),RINOLO(5),XPNOLO(5),XRINOL(5),
     &            CURPRT(9,3),CURMNT(9,3),CURMXT(9,3),CURSPT(9,3),
     &            PCUT(4,3),PEDT(4,3),POEDT(4,3),PCEOT(4,3),
     &            XCUPRT(27),XCUMNT(27),XCUMXT(27),XCUSPT(27),
     &            XPCUT(12),XPEDT(12),XPOEDT(12),XPCEOT(12)
*
       INTEGER   IPOS(2,16),I,INDI,INDJ,IWDG,NCOOLC,NG,NPKL,NWILI,
     &           NWOULI,ICOUNT,
     &           IPOS1(2,12),IPOS2(2,27)
*
       LOGICAL   BBVFR,BBSTOR
*
       CHARACTER KEY3*8,RUNFLG*8
*
       DATA KEY3/'       1'/,
     &      IPOS/  1,1, 1,2, 1,3, 1,4, 2,1, 2,2, 2,3, 2,4,
     &             3,1, 3,2, 3,3, 3,4, 4,1, 4,2, 4,3, 4,4/
     &      IPOS1/ 1,1, 1,2, 1,3, 2,1, 2,2, 2,3,
     &             3,1, 3,2, 3,3, 4,1, 4,2, 4,3/
     &      IPOS2/ 1,1, 1,2, 1,3, 2,1, 2,2, 2,3,
     &             3,1, 3,2, 3,3, 4,1, 4,2, 4,3,
     &             5,1, 5,2, 5,3, 6,1, 6,2, 6,3,
     &             7,1, 7,2, 7,3, 8,1, 8,2, 8,3,
     &             9,1, 9,2, 9,3/
*
CC*SEBL End of the declarations block.
*
C... Adjust the dimensions of data to be stored
*
       DO 9 IWDG=1,9
       GWINDC(IWDG)=0.
    9  GWINDA(IWDG)=0.
*
       DO 10 IWDG=1,NWILI
       GWINDC(IWDG)=RES79(1+19*(IWDG-1),2)
       GWINDA(IWDG)=RES79(2+19*(IWDG-1),2)
       ACONDX(IWDG)=1.E+6*ACOND(IWDG)
       RRWDGX(IWDG)=1.E+3*RRWDG(IWDG)
       HWINDX(IWDG)=1.E+3*HWIND(IWDG)
       DWINDX(IWDG)=1.E+3*(DWIND(IWDG)-RRWDG(IWDG))
       CURDEX(IWDG)=CURDEN(IWDG)/1.E+6
       WLOSSX(IWDG)=WLOSS(IWDG)/1.E+3
   10  WLOSMX(IWDG)=WLOSSM(IWDG)/1.E+3
*
C... Outdata block 1
*
CC*SBBL Start of the output block
*
       SEQU=SEQU2W*FLOAT(NWOULI)/1.E+6
       YOKA=(YOKAMP-1.)*100.
*
       HLMB=NINT(HLIMBX*1000.)
*
C... Store the required data on the database
*
       CALL PUTR('TRANSF  ','EVALP0  ',KEY3,EVALP0,1)
       CALL PUTR('TRANSF  ','EVALPK  ',KEY3,EVALPK,1)
       CALL PUTR('TERM    ','URATEDTP',KEY3,UNTAP,24)
       CALL PUTR('CORE    ','HLIMB   ',KEY3,HLMB,1)
       CALL PUTC('T146    ','REOPT   ',KEY3,REOPT,4,3)
       CALL PUTC('T146    ','DETPREC ',KEY3,DETPRE,1,3)
       IF (HLIMBM.GT.0.) CALL PUTR('T146LIM ','MAXHLIMB',KEY3,HLIMBM,1)
       IF (MAXP0.GT.0.) CALL PUTR('T146LIM ','MAXP0   ',KEY3,MAXP0,1)
       IF (MAXPK.GT.0.) CALL PUTR('T146LIM ','MAXPK   ',KEY3,MAXPK,1)
*
CC*SEBL End of the output block
*
CC*SOFF Switch OFF structure analyser (Not all calls need be shown)
*
       DCRE=NINT(DCOREX*1000.)
       CALL PUTR('CORE    ','DLIMB   ',KEY3,DCRE,1)
       CALL PUTR('CORE    ','REACHX0 ',KEY3,USHORE,1)
       CALL PUTR('CORE    ','MASSCORE',KEY3,GCORLA,1)
       BLIMBS=BLIMBX
       IF (BBVFR) BLIMBS=BLIMBX*BMAXPU
       CALL PUTR('CORE    ','BLIMB   ',KEY3,BLIMBS,1)
       CALL PUTR('TANK    ','LTANK   ',KEY3,RLTANK*1000,1)
       CALL PUTR('TANK    ','BTANK   ',KEY3,BTANKX*1000.,1)
       CALL PUTR('TANK    ','HTANK   ',KEY3,HTANKX*1000.,1)
       CALL PUTR('TANK    ','MASSTANK',KEY3,GTANK,1)
       CALL PUTR('TANK    ','MASSCOVR',KEY3,GCOVER,1)
       CALL PUTR('TANK    ','MASSOILF',KEY3,GOILF,1)
       CALL PUTR('CORE    ','HCORE   ',KEY3,HCORE*1000.,1)
       CALL PUTR('CORE    ','LCORE   ',KEY3,RLYOKE*1000.,1)
       PLMB=NINT(PLIMB*1000.)
       CALL PUTR('CORE    ','PITCHML ',KEY3,PLMB,1)
       PSLMB=NINT(PLSL*1000.)
       CALL PUTR('CORE    ','PITCHSL ',KEY3,PSLMB,1)
       CALL PUTR('CORE    ','NCOOLDC ',KEY3,FLOAT(NCOOLC),1)
       CALL PUTR('CORE    ','ALIMB   ',KEY3,1.E+4*ACORE,1)
       CALL PUTR('CORE    ','UTURNC  ',KEY3,U0*BLIMBS,1)
       CALL PUTR('TRANSF  ','SRATEDEQ',KEY3,SEQU,1)
       CALL PUTR('TRANSF  ','MASSTRNS',KEY3,GTRP,1)
       CALL PUTR('TRANSF  ','MASSACTP',KEY3,GACTP,1)
       CALL PUTR('TRANSF  ','VOLACTP ',KEY3,VACTP,1)
       CALL PUTR('CORE    ','YOKAMP  ',KEY3,YOKA,1)
*
       CALL PUTC('WINDING ','REOPTW  ',KEY3,WNDOPT,NWILI,3)
       CALL PUTR('WINDING ','NTURNS  ',KEY3,ZWIND,NWILI)
       CALL PUTR('WINDING ','SPACEFAC',KEY3,FILLFX,NWILI)
       CALL PUTR('WINDING ','BDUCT   ',KEY3,BDUCT,NWILI)
       CALL PUTR('WINDING ','ACOND   ',KEY3,ACONDX,NWILI)
       CALL PUTR('WINDING ','VOLTAGE ',KEY3,VOLTUT,NWILI)
       CALL PUTR('WINDING ','RESLIMB ',KEY3,RWIND,NWILI)
       CALL PUTR('WINDING ','TENSSTR ',KEY3,STRWMP,NWILI)
       CALL PUTR('WINDING ','COMPSTR ',KEY3,STRWMM,NWILI)
       CALL PUTR('WINDING ','RII     ',KEY3,WLOSSR,NWILI)
       CALL PUTR('WINDING ','EDDY    ',KEY3,WLOSSE,NWILI)
       CALL PUTR('WINDING ','SCCURR  ',KEY3,SHTCUR,NWILI)
       DO 123 IWDG=1,NWILI
       HPARTS(IWDG)=HPARTX(IWDG)*1000.
  123  BPARTS(IWDG)=BPARTX(IWDG)*1000.
       CALL PUTR('WINDING ','HSTRAND ',KEY3,HPARTS,NWILI)
       CALL PUTR('WINDING ','BSTRAND ',KEY3,BPARTS,NWILI)
       CALL PUTR('WINDING ','BWIND   ',KEY3,RRWDGX,NWILI)
       CALL PUTR('WINDING ','HWIND   ',KEY3,HWINDX,NWILI)
       CALL PUTR('WINDING ','DWIND   ',KEY3,DWINDX,NWILI)
       CALL PUTR('WINDING ','CURRDENS',KEY3,CURDEX,NWILI)
       CALL PUTR('WINDING ','PWFA    ',KEY3,WLOSSX,NWILI)
       CALL PUTR('WINDING ','MAXPWFA ',KEY3,WLOSMX,NWILI)
       CALL PUTR('WINDING ','MWCONLIM',KEY3,GWINCO,NWILI)
       CALL PUTR('WINDING ','GWDGCLIM',KEY3,GWINDC,NWILI)
       CALL PUTR('WINDING ','GWDGALIM',KEY3,GWINDA,NWILI)
       CALL PUTR('WINDING ','GCLCKBND',KEY3,GS134,NWILI)
*
CC*SON  Switch ON  structure analyser (Not all calls need be shown)
*
C... Outdata block 2
*
CC*SBBL Start of the allocation block.
*
       URFCTR=ZWOULI/1.E+3
*
C... Convert the required data to a suitable format
*
       DO 92 I=1,16
       URC12X(I)=0.
       URC13X(I)=0.
       URC23X(I)=0.
       UXC12X(I)=0.
       UXC13X(I)=0.
   92  UXC23X(I)=0.
*
       DO 93 I=1,12
       XPCUT(I) =0.
       XPEDT(I) =0.
       XPOEDT(I)=0.
   93  XPCEOT(I)=0.
*
       DO 94 I=1,27
       XCUPRT(I)=0.
       XCUMNT(I)=0.
       XCUMXT(I)=0.
   94  XCUSPT(I)=0.
*
       DO 91 I=1,16
       INDI=IPOS(1,I)
       INDJ=IPOS(2,I)
       URC12X(I)=URC(INDI,INDJ,1)*URFCTR
       URC13X(I)=URC(INDI,INDJ,2)*URFCTR
       URC23X(I)=URC(INDI,INDJ,3)*URFCTR
       UXC12X(I)=UXC(INDI,INDJ,1)*100.
       UXC13X(I)=UXC(INDI,INDJ,2)*100.
   91  UXC23X(I)=UXC(INDI,INDJ,3)*100.
*
       DO 95 I=1,12
       INDI=IPOS1(1,I)
       INDJ=IPOS1(2,I)
       XPCUT(I) =PCUT(INDI,INDJ)*URFCTR
       XPEDT(I) =PEDT(INDI,INDJ)*URFCTR
       XPOEDT(I)=POEDT(INDI,INDJ)*URFCTR
   95  XPCEOT(I)=PCEOT(INDI,INDJ)*URFCTR
*
       DO 96 I=1,27
       INDI=IPOS2(1,I)
       INDJ=IPOS2(2,I)
       XCUPRT(I)=CURPRT(INDI,INDJ)
       XCUMNT(I)=CURMNT(INDI,INDJ)
       XCUMXT(I)=CURMXT(INDI,INDJ)
   96  XCUSPT(I)=CURSPT(INDI,INDJ)
*
CC*SEBL End of the allocation block.
*
C... Calculate approximate 3-winding losses
C... for the cooling program.
*
       CALL PCOOL(URC,URFCTR,NG,SRATEX,PKCOOL)
       Q00SRA=Q00(1)/SRATEX(1)*100.
       REACHX=UXC(1,1,1)*100.
*
C... Store the required data on the database
*
       CALL PUTR('TRANSF  ','PK12    ',KEY3,URC12X,16)
*
CC*SOFF Switch OFF structure analyser (Not all calls need be shown)
*
       DO 101 ICOUNT=1,5
       XRINOL(ICOUNT)=RINOLO(ICOUNT)*100.
  101  XPNOLO(ICOUNT)=PNOLO(ICOUNT)/1.E+3
*
       CALL PUTR('TRANSF  ','PK13    ',KEY3,URC13X,16)
       CALL PUTR('TRANSF  ','PK23    ',KEY3,URC23X,16)
       CALL PUTR('TRANSF  ','REAC12  ',KEY3,UXC12X,16)
       CALL PUTR('TRANSF  ','REAC13  ',KEY3,UXC13X,16)
       CALL PUTR('TRANSF  ','REAC23  ',KEY3,UXC23X,16)
*
       CALL PUTR('TRANSF  ','PCUTAP  ',KEY3,XPCUT ,12)
       CALL PUTR('TRANSF  ','PEDTAP  ',KEY3,XPEDT ,12)
       CALL PUTR('TRANSF  ','POEDTAP ',KEY3,XPOEDT,12)
       CALL PUTR('TRANSF  ','PCEOTAP ',KEY3,XPCEOT,12)
*
       CALL PUTR('TRANSF  ','CURPRTAP',KEY3,XCUPRT,27)
       CALL PUTR('TRANSF  ','CURMNTAP',KEY3,XCUMNT,27)
       CALL PUTR('TRANSF  ','CURMXTAP',KEY3,XCUMXT,27)
       CALL PUTR('TRANSF  ','CURSPTAP',KEY3,XCUSPT,27)
*
       CALL PUTR('TRANSF  ','PKCOOL  ',KEY3,PKCOOL,1)
       CALL PUTR('TRANSF  ','P0      ',KEY3,P00(1)/1.E+3,1)
       CALL PUTR('TRANSF  ','P0IND   ',KEY3,XPNOLO,5)
       CALL PUTR('TRANSF  ','I0      ',KEY3,Q00SRA,1)
       CALL PUTR('TRANSF  ','I0IND   ',KEY3,XRINOL,5)
       CALL PUTR('TRANSF  ','SOUNDPOW',KEY3,SOUND(3),1)
       CALL PUTR('TRANSF  ','SOUNDPRE',KEY3,SOUND(3)-SPRED,1)
       CALL PUTR('TRANSF  ','REACHX  ',KEY3,REACHX,1)
*
CC*SON  Switch ON  structure analyser (Not all calls need be shown)
*
       RUNFLG='T146RUN '
*
C... Set RUNFLAG on the data base to indicate that the results
C... have been stored.
*
       CALL PUTC('T146    ','RUNFLAG ',KEY3,RUNFLG,1,8)
*
       BBSTOR=.TRUE.
       RETURN
       END
