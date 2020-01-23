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
*      Subroutine COST79
*      -----------------
*
C...   Title:  Total cost calculation in MNL79
*
*      Written: XX-XX-XX by A.N.Other      , XXXX
*      Revised: 86-01-28 by Ron Bell       , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 90-01-09 by Per S�derberg  , SETFO/IK
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE COST79
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0231(1)    ,  DARR   (1)    ),
     &  (XZ0301(1)    ,  IARR   (1)    ),
     &  (XZ0581(1)    ,  UARR   (1)    ),
     &  (XZ0651(1)    ,  ACOND  (1)    )
       EQUIVALENCE
     &  (XZ0671(1)    ,  BDUCT  (1)    ),
     &  (XZ0681(1)    ,  BPART  (1)    ),
     &  (XZ0721(1)    ,  IPISOL (1)    ),
     &  (XZ0741(1)    ,  DWIND  (1)    ),
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    )
       EQUIVALENCE
     &  (XZ0911(1)    ,  KWITYP (1)    ),
     &  (XZ0931(1)    ,  NLOOP  (1)    ),
     &  (XZ0941(1)    ,  NOUCO  (1)    ),
     &  (XZ0991(1)    ,  RPART  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    )
       EQUIVALENCE
     &  (XZ1061(1)    ,  TCOV1  (1)    ),
     &  (XZ1071(1)    ,  TCOV2  (1)    ),
     &  (XZ1161(1)    ,  ZCODU  (1)    ),
     &  (XZ1191(1)    ,  ZWIND  (1)    ),
     &  (XZ1219(1)    ,  KTYPRW (1)    )
       EQUIVALENCE
     &  (XZ1227(1)    ,  NMSTEP (1)    ),
     &  (XZ1231(1)    ,  NPSTEP (1)    ),
     &  (XZ1263(1)    ,  SRATEP (1)    ),
     &  (XZ1271(1,1)  ,  UN     (1,1)  ),
     &  (XZ1287(1)    ,  USURG  (1)    )
       EQUIVALENCE
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1377       ,  BBEND         ),
     &  (XZ1380       ,  BBEXAC        ),
     &  (XZ1385       ,  BBLT10        )
       EQUIVALENCE
     &  (XZ1397       ,  ACOVER        ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1474       ,  HTANK         ),
     &  (XZ1481(1)    ,  NCONDU (1)    ),
     &  (XZ1486       ,  JFC           )
       EQUIVALENCE
     &  (XZ1492       ,  KTAN79        ),
     &  (XZ1494       ,  NCLA          ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1509       ,  NWOULI        )
       EQUIVALENCE
     &  (XZ1513       ,  PI            ),
     &  (XZ1527       ,  ISTGRD        ),
     &  (XZ1547       ,  TANKDI        ),
     &  (XZ1558       ,  TYPCOR        ),
     &  (XZ2581(1)    ,  ZLAG   (1)    )
*
       REAL      ACOND(9),BDUCT(9),BPART(9),DARR(100),HWIND(9),
     &           DWIND(9),HPART(9),RPART(9),RRWDG(9),SRATEP(4),
     &           TCOV1(9),TCOV2(9),UN(4,4),USURG(4),ZCODU(9),
     &           EFKV(4),ACONDA(4),RCONN(4),CURTML(4),
     &           ZCABEL(9),ZCOIAR(9),ZLAG(9),ZWIND(9),
     &           ABUNCH,ACONDX,ACOVER,APARTX,CBUNCH,CCOV1,CCOV2,
     &           DCORE,FACTOR,GS24,HTANK,PI,RGUID,RSEK,SUM41,
     &           TANKDI,TYPCOR,UNX,WCLS,XBUNCH,XPART
*
       INTEGER   IARR(100),KTYPRW(4),KWITYP(9),NLOOP(9),NMSTEP(4),
     &           NOUCO(9),NPSTEP(4),NCOND(4),NGATE(4),IPISOL(9),
     &           NCONDU(9),I,IDUM,ISTGRD,IT,ITML,IWDG,JFC,KODRAD,
     &           KTAN79,NCEQ,NCLA,NG,NTANK,NTAPP,NWILI,NWOULI
*
       LOGICAL   BBREAC,BBLNX,BBVR(4),
     &           BBHELP(10),BBPRNT,BBEND,BBEXAC,BBLT10
*
       DIMENSION UARR(100),CONVAR(9),TCEPOX(9)
       CHARACTER UARR*4,CONVAR*4,TCEPOX*4
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
        INTEGER IHTAN(6)
*
        DATA BBREAC/.FALSE./,BBLNX/.FALSE./,FACTOR /1.E+3/
     &       IHTAN /1,3,5,6,7,8/
        DATA EFKV,ACONDA,RCONN,CURTML,NCOND,NGATE,ZCABEL,ZCOIAR
     &       /42*0./
*
CC*SEBL End of the declarations block.
*
C... Set certain variables
*
       BBPRNT=.FALSE.
       IF (BBEND.AND.BBHELP(5)) BBPRNT=.TRUE.
       GS24=GS241+GS242
*
C... Core
*
       WRITE(*,*) 'BBPRNT,JFC,NINT(TYPCOR),ISTGRD', BBPRNT,JFC,TYPCOR,ISTGRD
       WRITE(*,*) 'GS211,GS212,GG21,GS241', GS211,GS212,GG21,GS241
       WRITE(*,*) 'GS242,GG24,GS25,DCORE', GS242,GG24,GS25,DCORE
       WRITE(*,*) 'FACTOR', FACTOR
       
       CALL CCORET(BBPRNT,JFC,NINT(TYPCOR),ISTGRD,NCLA,
     &             GS211,GS212,GG21,GS241,GS242,GG24,GS25,DCORE*FACTOR)
*
C... WINDINGS
*
       DO 20 IWDG=1,NWILI
       CCOV1=(HPART(IWDG)+BPART(IWDG)+TCOV1(IWDG))*FACTOR
       CCOV2=CCOV1+(TCOV2(IWDG)+(RPART(IWDG)-1.)*BPART(IWDG))*FACTOR
       WCLS=HPART(IWDG)*FACTOR
       NTAPP=2*NWOULI
       IT=IARR(20+IWDG)
       IF(IARR(30+IWDG).EQ.4)
     & NTAPP=IARR(40+IWDG)*(IARR(5+IT)+IARR(10+IT)+2)*NWOULI
       XBUNCH=ZWIND(IWDG)*DWIND(IWDG)*IARR(40+IWDG)*PI*NWOULI
       ABUNCH=FACTOR*FACTOR*ACOND(IWDG)/IARR(40+IWDG)
       CBUNCH=XBUNCH*ACOND(IWDG)/(BPART(IWDG)*HPART(IWDG))
*
C... Calculate some auxiliary variables
*
C,,, Depending on winding type
*
       IF(KWITYP(IWDG).GT.5) THEN
          XBUNCH=XBUNCH/FLOAT(NLOOP(IWDG))
          ABUNCH=ABUNCH*FLOAT(NLOOP(IWDG))
       END IF
       APARTX=FACTOR*FACTOR*HPART(IWDG)*BPART(IWDG)
       ACONDX=RPART(IWDG)*APARTX
       IF(NCONDU(IWDG).EQ.3) ACONDX=FACTOR*FACTOR*ACOND(IWDG)
       RGUID=DARR(IWDG)
       XPART=0.5+ABUNCH/APARTX
      
       CONVAR(IWDG)=UARR(30+IWDG)
       TCEPOX(IWDG)=UARR(40+IWDG)
*
C... Winding costs
*
       CALL CWIND(BBPRNT,JFC,IWDG,NCONDU(IWDG),KWITYP(IWDG),RGUID,
     &            GS11(IWDG),GS121(IWDG),GS122(IWDG),GS131(IWDG),
     &            GS132(IWDG),GS133(IWDG),GS134(IWDG),GS1351(IWDG),
     &            GS1352(IWDG),GS136(IWDG),APARTX,TCOV1(IWDG)*FACTOR,
     &            TCOV2(IWDG)*FACTOR,CCOV1,CCOV2,NWOULI,
     &            DWIND(IWDG)*FACTOR,RRWDG(IWDG)*FACTOR,WCLS,
     &            BDUCT(IWDG)*FACTOR,NTAPP,ACONDX,
     &            XBUNCH,ABUNCH,ZCODU(IWDG),NOUCO(IWDG),XPART,BBEXAC,
     &            BPART(IWDG)*FACTOR,ZCABEL(IWDG),ZCOIAR(IWDG),
     &            ZLAG(IWDG),BBLNX,RPART(IWDG),
     &            ZWIND(IWDG),IPISOL(IWDG),HPART(IWDG)*FACTOR,CBUNCH,
     &            CONVAR(IWDG),TCEPOX(IWDG))
 20    CONTINUE
*
C... Calculations for cleats & leads
*
       DO 30 ITML=1,NG
          NCOND(ITML)=3
          NGATE(ITML)=1
          UNX=UN(ITML,1)
          IF(BBVR(ITML)) UNX=UN(ITML,3)
          CURTML(ITML)=SRATEP(ITML)/UNX
          IF(CURTML(ITML).GT.3150.) NCOND(ITML)=1
          IF(USURG(ITML).GE.750.E+3) NGATE(ITML)=2
          EFKV(ITML)=UN(ITML,1)/FACTOR
 30    CONTINUE
*
C... Cleats & leads costs
*
       CALL CCLLE(BBPRNT,JFC,NCOND,NGATE,GS31,GS32,GG32,EFKV)
*
       SUM41=0.
       DO 40 I=1,9
 40    SUM41=SUM41+GS41(I)
*
C... Number of connections in each terminal
*
       DO 45 ITML=1,4
          ACONDA(ITML)=0.
          RCONN(ITML)=2.
*
C... For each terminal
*
C,,, In the actual transformer
*
          IF(ITML.LE.NG) THEN
*
C... Assume 2.5 A/mm2 in cleats & leads
*
             ACONDA(ITML)=CURTML(ITML)/2.5
*
C... For each terminal
*
C,,, If regulated
*
             IF(BBVR(ITML)) THEN
                IF(KTYPRW(ITML).EQ.1)
     &                   RCONN(ITML)=2.+2.*(NPSTEP(ITML)+NMSTEP(ITML))
                IF(KTYPRW(ITML).EQ.2)
     &                   RCONN(ITML)=2.+2.*(NPSTEP(ITML)+NMSTEP(ITML))
                IF(KTYPRW(ITML).EQ.3)
     &                   RCONN(ITML)=2.+2.+(NPSTEP(ITML)+NMSTEP(ITML))
             END IF
          END IF
 45    CONTINUE
*
C... Initialise GS48=0.
*
CC*SBBL
*
       GS48=0.
*
C... Active part cost
*
       CALL CACTP(BBPRNT,JFC,SUM41,GS42,GS45,GS46,GS47,GS48,NWOULI,
     &            DCORE*FACTOR,GS24,GG49,RCONN,ACONDA,EFKV,NG,BBREAC)
*
CC*SEBL
*
       NTANK=IHTAN(KTAN79)
*
C... ( BBLT10=.FALSE. F�R KTAN79=3,4,5,6 )
*
CC*SBBL
*
       IF(BBLT10) NTANK=NTANK+1
*
CC*SEBL
*
C... Set a code for the radiators
*
C,,, NOT an exact calculation
*
       IF(.NOT.BBEXAC) THEN
          KODRAD=4
*
C... Exact calculation
*
       ELSE
          KODRAD=1
          IF(HTANK.GT.2.1) KODRAD=2
          IF(HTANK.GT.2.5) KODRAD=3
          IF(HTANK.GT.2.8) KODRAD=4
          IF(HTANK.GT.3.3) KODRAD=5
       END IF
       RSEK=32.
C... Cooling equipment on direct on tank : NCEQ = 1
       NCEQ= 1
*
C--- IDUM is set to 0 - the corresponding NRAD in CTANK is not used ��
*
       IDUM=0
*
C... Tank cost
*
       CALL CTANK(BBPRNT,JFC,NTANK,ACOVER,TANKDI,KODRAD,IDUM,RSEK,NCEQ,
     &            GS51,GS52,GS53,GS61,GS71,GS72,GG151,GG152,
     &            GG51,GG52,GG54,GG55,GG61,GG63,GG71,GG72,GG74)
*
C... Pipe-work cost
*
       CALL CPIPE(BBPRNT,JFC,GS81,GS91,GS92,GS102,
     &            GG81, GG83,GG91,GG92,GG94,GG101,GG102)
*
C... Final assembly cost
*
       CALL CFIN (BBPRNT,JFC,GS24, GS111,GS112,GS141,GS142,GN1210,
     &            GG111, GG1210, GG151, GG152)
     
       RETURN
       END
