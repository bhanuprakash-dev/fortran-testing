CBLOCKC
       BLOCK DATA BLOCKC
*
       REAL
     &   BOOSMA, BOOSVO, BOOSRE, BOOSTR,
     &   CUC92(3), CUC92P(3), FEC92(4), FEC92P(4), OLC92, OLC92P
*
       LOGICAL         BBBOOS
*
        COMMON
     &   /CVIKT1/  GNS011(9),GGS011(9),GAO011(9),V011(9),
     &             GNS012(9),GGS012(9),GAO012(9),V012(9),
     &             GNS013(9),GGS013(9),GAO013(9),V013(9),
     &             GNS014(9),GGS014(9),GAO014(9),V014(9),
     &             GNS015(9),GGS015(9),GAO015(9),V015(9),
     &             GNS016(9),GGS016(9),GAO016(9),V016(9),
     &             GNS017(9),GGS017(9),GAO017(9),V017(9),
     &             GNS042(9),GGS042(9),GAO042(9),V042(9)
*
        DATA
     &             GNS011/9*0./,GGS011/9*0./,GAO011/9*0./,V011/9*0./,
     &             GNS012/9*0./,GGS012/9*0./,GAO012/9*0./,V012/9*0./,
     &             GNS013/9*0./,GGS013/9*0./,GAO013/9*0./,V013/9*0./,
     &             GNS014/9*0./,GGS014/9*0./,GAO014/9*0./,V014/9*0./,
     &             GNS015/9*0./,GGS015/9*0./,GAO015/9*0./,V015/9*0./,
     &             GNS016/9*0./,GGS016/9*0./,GAO016/9*0./,V016/9*0./,
     &             GNS017/9*0./,GGS017/9*0./,GAO017/9*0./,V017/9*0./,
     &             GNS042/9*0./,GGS042/9*0./,GAO042/9*0./,V042/9*0./
        COMMON
     &   /CVIKT2/  GNS021,GGS021,GNG021,V021,
     &             GNS02S,GNS02W,GGS02S,GGS02W,GNG022,GAO022,V022 ,
     &             GNS023,GGS023,V023
*
        DATA
     &             GNS021,GGS021,GNG021,V021,
     &             GNS02S,GNS02W,GGS02S,GGS02W,GNG022,GAO022,V022 ,
     &             GNS023,GGS023,V023
     &             /14*0./
        COMMON
     &   /CVIKT3/  GNS031(4),GGS031(4),GAO031(4),V031(4),
     &             GNS032(4),GGS032(4),GAO032(4),V032(4),
     &             GNS033(4),GGS033(4),GNG033(4),GAO033(4),V033(4)
*
        DATA
     &             GNS031,GGS031,GAO031,V031,
     &             GNS032,GGS032,GAO032,V032,
     &             GNS033,GGS033,GNG033,GAO033,V033
     &             /52*0./
        COMMON
     &   /CVIKT4/  GNS041,GGS041,GNG041,GAO041,V041,
     &             GNS043,GGS043,GAO043,V043,
     &             GNS044,GGS044,GAO044,V044,
     &             GNS045,GGS045,V045
*
        DATA
     &             GNS041,GGS041,GNG041,GAO041,V041,
     &             GNS043,GGS043,GAO043,V043,
     &             GNS044,GGS044,GAO044,V044,
     &             GNS045,GGS045,V045
     &             /16*0./
        COMMON
     &   /CVIKT5/  GNS051,GGS051,GNG051,GAO051,
     &             GNS052,GGS052,GNG052,GNG521,
     &             GNS053,GGS053,GNG053,V053,
     &             GNS054,GGS054,GNG054,
     &             GNS06,GGS06,GNG06,GNS07,GGS07,GNG07
*
        DATA
     &             GNS051,GGS051,GNG051,GAO051,
     &             GNS052,GGS052,GNG052,GNG521,
     &             GNS053,GGS053,GNG053,V053,
     &             GNS054,GGS054,GNG054,
     &             GNS06,GGS06,GNG06,GNS07,GGS07,GNG07
     &             /21*0./
        COMMON
     &   /CVIKT6/  GNS08,GGS08,GNG08,
     &             GNS111,GGS111,GNG111,
     &             GNS112,GGS112,GNG112,
     &             GNS12,GGS12,GNG12
        DATA
     &             GNS08,GGS08,GNG08,
     &             GNS111,GGS111,GNG111,
     &             GNS112,GGS112,GNG112,
     &             GNS12,GGS12,GNG12
     &             /12*0./
        COMMON
     &   /CVIKT7/  GNS131,GNG131,
     &             GOILF,GOILA,
     &             GNS13A,GNS13S,GGS13A,GGS13S,GNG132
     &   /CVIKTT/  GNSTOT,GNGTOT,GGSTOT
     &   /VNET00/  VNET01(9),VNET02,VNET03,VNET04,VNET05,VNET06,
     &             VNET07,VNET08,VNET11,VNET12,VNET13,VNET14,VNET1
        DATA
     &             GNS131,GNG131,
     &             GOILF,GOILA,
     &             GNS13A,GNS13S,GGS13A,GGS13S,GNG132,
     &             GNSTOT,GNGTOT,GGSTOT,
     &             VNET01,VNET02,VNET03,VNET04,VNET05,VNET06,
     &             VNET07,VNET08,VNET11,VNET12,VNET13,VNET14,VNET1
     &             /33*0./
        COMMON
     &   /CCOST1/  CS011(9),CM011A(9),CM011B(9),CM011C(9),CM011D(9),
     &             CM011(9),CS012(9),CM012(9),CS013(9),CM013(9),
     &             CS014(9),CG014(9),CM014(9),CS015(9),CM015(9),
     &             CS016(9),CM016(9),CS017(9),CM017(9),
     &             CS042(9),CM042(9),CM047(9)
        DATA
     &             CS011,CM011A,CM011B,CM011C,CM011D,
     &             CM011,CS012,CM012,CS013,CM013,
     &             CS014,CG014,CM014,CS015,CM015,
     &             CS016,CM016,CS017,CM017,
     &             CS042,CM042,CM047
     &             /198*0./
        COMMON
     &   /CCOST2/  CS021,CG021,CM021,
     &             CS022S,CS022W,CG022,CM022,CS023,CM023
        DATA
     &             CS021,CG021,CM021,
     &             CS022S,CS022W,CG022,CM022,CS023,CM023
     &             /9*0./
        COMMON
     &   /CCOST3/  CS031(4),CM031(4),CS032(4),
     &             CS033(4),CG033(4),CM033(4),
     &             CM034(4)
        DATA
     &             CS031,CM031,CS032,
     &             CS033,CG033,CM033,
     &             CM034
     &             /28*0./
        COMMON
     &   /CCOST4/  CS041,CG041,CM041,CS043,CM043,
     &             CS044,CM044,CS045,CM045,CM046,CG045
        DATA
     &             CS041,CG041,CM041,CS043,CM043,
     &             CS044,CM044,CS045,CM045,CM046,CG045
     &             /11*0./
        COMMON
     &   /CCOST5/  CS051,CG051,CM051,CP051,CS052,CG052,CM052,CP052,
     &             CG0521,CS053,CG053,CM053,CS054,CG054,CM054,
     &             CS06,CG06,CM06,CS07,CG07,CM07,CP07
        DATA
     &             CS051,CG051,CM051,CP051,CS052,CG052,CM052,CP052,
     &             CG0521,CS053,CG053,CM053,CS054,CG054,CM054,
     &             CS06,CG06,CM06,CS07,CG07,CM07,CP07
     &             /22*0./
        COMMON
     &   /CCOST6/  CS08,CG08,CM08,CP08,CS111,CG111,CM111,
     &             CS112,CG112,CM112,CS12,CG12,CM12
        DATA
     &             CS08,CG08,CM08,CP08,CS111,CG111,CM111,
     &             CS112,CG112,CM112,CS12,CG12,CM12
     &             /13*0./
        COMMON
     &   /CCOST7/  CS131,COIL,CG131,CM131,CS132S,
     &             CS132A,CG132,CM132,CM133,CG14,CM14,COILF,COILA
        DATA
     &             CS131,COIL,CG131,CM131,CS132S,
     &             CS132A,CG132,CM132,CM133,CG14,CM14,COILF,COILA
     &             /13*0./
       COMMON
     &   /CSUB00/  CSUB01(9),CSUB02,CSUB03,CSUB04,CSUB05,CSUB06,
     &             CSUB07,CSUB08,CSUB11,CSUB12,CSUB13,CSUB14,CSUB1
     &   /CTOTAL/  CTOS11,CTOS21,CTOS5X,CTOSXX,CTOGXX,CTOMXX
       DATA
     &             CSUB01,CSUB02,CSUB03,CSUB04,CSUB05,CSUB06,
     &             CSUB07,CSUB08,CSUB11,CSUB12,CSUB13,CSUB14,CSUB1,
     &             CTOS11,CTOS21,CTOS5X,CTOSXX,CTOGXX,CTOMXX
     &             /27*0./
       COMMON/CST92/
     &   CUC92, CUC92P, FEC92, FEC92P, OLC92, OLC92P
       DATA
     &   CUC92, CUC92P, FEC92, FEC92P, OLC92, OLC92P /16*0./
*
       COMMON
     &   /KONSRE/  RES79(241,6)
       DATA
     &             RES79/1446*0./
*
       COMMON /V79/
     & GSN11(9),    GSN23,       GSN25,        GSN31(4),    GSN32(4),
     & GSN41(9),    GSN42,       GSN45,        GSN46,
     & GSN47,       GSN48,
     & GSN51,       GSN52,       GSN53,        GSN054,
     & GSN61,       GSN71,       GSN72,        GSN81,
     & GSN91,       GSN92,       GSN102,       GSN111,      GSN112,
     & GSN121(9),   GSN122(9),   GSN131(9),    GSN132(9),
     & GSN133(9),   GSN134(9),   GS1351(9),    GS1352(9),
     & GSN136(9),
     & GSN141,      GSN142,      GSN143,       GSN211,      GSN212,
     & GSN221,      GSN222,      GSN241,       GSN242,      GS1210
       DATA
     & GSN11,       GSN23,       GSN25,        GSN31   ,    GSN32   ,
     & GSN41   ,    GSN42,       GSN45,        GSN46,
     & GSN47,       GSN48,
     & GSN51,       GSN52,       GSN53,        GSN054,
     & GSN61,       GSN71,       GSN72,        GSN81,
     & GSN91,       GSN92,       GSN102,       GSN111,      GSN112,
     & GSN121   ,   GSN122   ,   GSN131   ,    GSN132   ,
     & GSN133   ,   GSN134   ,   GS1351   ,    GS1352   ,
     & GSN136   ,
     & GSN141,      GSN142,      GSN143,       GSN211,      GSN212,
     & GSN221,      GSN222,      GSN241,       GSN242,      GS1210
     & /137*0./
       COMMON /VG79/
     & GG21,        GG22,        GG23,         GG24,        GG32(4),
     & GG49,        GG51,        GG52,         GG53,        GG054,
     & GG54,        GG55,        GG61,         GG63,        GG71,
     & GG72,        GG74,        GG81,         GG83,
     & GG91,        GG92,        GG94,         GG101,       GG102,
     & GG111,       GG1210 ,     GG151,        GG152
       DATA
     & GG21,        GG22,        GG23,         GG24,        GG32   ,
     & GG49,        GG51,        GG52,         GG53,        GG054,
     & GG54,        GG55,        GG61,         GG63,        GG71,
     & GG72,        GG74,        GG81,         GG83,
     & GG91,        GG92,        GG94,         GG101,       GG102,
     & GG111,       GG1210 ,     GG151,        GG152
     & /31*0./
*
       COMMON/BOOS/    BOOSMA, BOOSVO, BOOSRE, BOOSTR, BBBOOS
       DATA            BOOSMA, BOOSVO, BOOSRE, BOOSTR/3*0. , 1./
       DATA            BBBOOS /.FALSE./
*
       END
