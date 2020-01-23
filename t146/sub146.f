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
*      Subroutine SUB146
*      -----------------
*
C...   Title:  Analysis of indata and calculation control routine
*
*      Written: XX-XX-XX by A.N.Other      , XXXX
*      Revised: 85-10-18 by Ron Bell       , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 90-02-12 by Per Sãderberg  , SETFO/IK
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE SUB146(BBRES)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ0080       ,  FNEXTV        ),
     &  (XZ0090       ,  FEXTV         ),
     &  (XZ0145       ,  U0IN          ),
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ0581(1)    ,  UARR   (1)    )
       EQUIVALENCE
     &  (XZ1255(1)    ,  SLINE  (1)    ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1351(1)    ,  BBSCI  (1)    ),
     &  (XZ1354(1)    ,  BBSCJ  (1)    ),
     &  (XZ1357(1)    ,  BBSCK  (1)    ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ1360(1)    ,  BBSCL  (1)    )
       EQUIVALENCE
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1386       ,  BBOPTI        ),
     &  (XZ1390       ,  BBSLIM        ),
     &  (XZ1393       ,  BBVFR         )
       EQUIVALENCE
     &  (XZ1406       ,  BLTOPM        ),
     &  (XZ1407       ,  BMAXPU        ),
     &  (XZ1424       ,  DCOMAX        ),
     &  (XZ1425       ,  DCOMIN        )
       EQUIVALENCE
     &  (XZ1434       ,  DTSNN         ),
     &  (XZ1433       ,  DTSNF         ),
     &  (XZ1432       ,  DTSFF         ),
     &  (XZ1439       ,  F1TANK        ),
     &  (XZ1440       ,  F2TANK        ),
     &  (XZ1441       ,  F3TANK        ),
     &  (XZ1482       ,  IPAGE         ),
     &  (XZ1486       ,  JFC           )
       EQUIVALENCE
     &  (XZ1491       ,  KCORE         ),
     &  (XZ1492       ,  KTAN79        ),
     &  (XZ1493       ,  KTANK         ),
     &  (XZ1494       ,  NCLA          ),
     &  (XZ1499       ,  NG            )
       EQUIVALENCE
     &  (XZ1501       ,  NPHAS         ),
     &  (XZ1571       ,  ZWOULI        )
       EQUIVALENCE
     &  (XZ1509       ,  NWOULI        ),
     &  (XZ1527       ,  ISTGRD        ),
     &  (XZ1560       ,  UMAXPU        )
       EQUIVALENCE
     &  (XZ1565       ,  YHADD         ),
     &  (XZ1566       ,  YHRED         ),
     &  (XZ1572       ,  CHCORE        ),
     &	(XZ1515       ,  PLIMB		),
     &(XZ1472 , HLIMB)
*
       REAL      AARR(200),SLINE(4),BLTOPM,BMAXPU,DCOMAX,DCOMIN,
     &           FEXTV,FNEXTV,UMAXPU,U0IN,YHADD,YHRED,
     &           F1TANK,F2TANK,F3TANK,DTSNN,DTSNF,DTSFF,
     &           ZWOULI
*
       INTEGER   IPAGE,JFC,KCORE,NCLA,NG,NPHAS,NWOULI,KTAN79,KTANK,
     &           ISTGRD,ISTOP
*
       DIMENSION UARR(100), TARR(100)
       CHARACTER UARR*4,CHCORE*60, TARR*8
*
       LOGICAL   BBRES,BBOPTI,BBSLIM,BBVFR,
     &           BBVR(4),BBSCI(4),BBSCJ(4),BBSCK(4),BBSCL(4),
     &           BBERR,BBTS,BBHELP(10)
*
*
        IPAGE= 0
*
C... Read and analyse the indata
*
       write(*,*)'********** SUB146 before HEAD1     *********'
       CALL HEAD1
       write(*,*)'********** SUB146 after  HEAD1     *********'
* ...  BACKTRACKING
       IF(ISTOP.EQ.1) RETURN
  
*
C... Calculate the dimensioning winding currents and
C... equivalent 2-winding rating
*
       write(*,*)'********** SUB146 before DIMCUR    *********'
       CALL DIMCUR
       write(*,*)'********** SUB146 before PRECRE    *********'
*

c       CALL PRINT_SAI
C... Preliminary core variables
*
       CALL PRECRE(AARR,BBSLIM,TARR,BBVFR,BMAXPU,JFC,
     &             NPHAS,NWOULI,UARR,UMAXPU,
     &             BLTOPM,CHCORE,DCOMAX,DCOMIN,FEXTV,FNEXTV,
     &             KCORE,NCLA,ISTGRD,U0IN,YHADD,
     &             YHRED,BBERR,BBTS,BBHELP)
*
       print*,'After Precre'
c       CALL PRINT_SAI
C... Determine the number of parameters
*
       write(*,*)'********** SUB146 before PREP1     *********'
       CALL PREP1
*
c       CALL PRINT_SAI
C... Preliminary temperature variables
*
       write(*,*)'********** SUB146 before PRETEM    *********'
       CALL PRETEM(AARR,DTSNN,DTSNF,DTSFF)
*
C... Preliminary tank variables
*
       write(*,*)'********** SUB146 before PRETAN    *********'
       CALL PRETAN(F1TANK,F2TANK,F3TANK,KTAN79,KTANK)
*
       write(*,*)'********** SUB146 before PKL79     *********'
       CALL PKL79
*
C... Determine the dimensioning short-circuit case
*
       write(*,*)'********** SUB146 before DIMSCC    *********'
       CALL DIMSCC(BBVR,NG,SLINE,BBSCI,BBSCJ,BBSCK,BBSCL)
*
* ...  BACKTRACKING
       IF(ISTOP.EQ.1) RETURN
*

c       write(*,*)'b4 head2 DPHAS=',DPHAS
c       write(*,*)'b4 head2 XZ1515=',XZ1515
C... Optimisation
*

       write(*,*)'********** SUB146 before HEAD2     *********'
       CALL HEAD2
* ...  BACKTRACKING
       IF(ISTOP.EQ.1) RETURN

c       write(*,*)'XZ1515=',XZ1515
C... Check which limits, if any, have been exceeded
*
       write(*,*)'********** SUB146 before CONSTR    *********'
       CALL CONSTR

c       write(*,*)'XZ1515=',XZ1515
*
C... Print results on file JFC, also list on file 06
*
       write(*,*)'********** SUB146 before PAGEUT    *********'
       CALL PAGEUT(BBRES)
       print*,'after pageut'
*
        print*,'coming out of sub146'
       RETURN
       END
