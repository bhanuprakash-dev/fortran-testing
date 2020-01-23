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
*      Subroutine WX146
*      ----------------
*
C...   Title: Translate indata from the DAISY-panels
*
*      Written: 83-01-25 by SE Jansson    , ZKB
*      Revised: 85-06-18 by S Andersson   , ZKAB
*      Revised: 85-10-18 by Ron Bell      , TRAFO/IK
*      Revised: 86-10-10 by Ron Bell      , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell      , TRAFO/IK
*      Revised: 89-01-04 by Per S�derberg , TRAFO/IK
*      Revised: 89-04-21 by Per S�derberg , SETFO/IK
*      Revised: 89-10-02 by B-G Bladh     , SETFO/K1
*      Revised: 89-10-24 by Per S�derberg , SETFO/IK
*      Revised: 90-01-08 by Per S�derberg , SETFO/IK
*      Revised: 91-09-05 by Jan Johansson , SETFO/TS, added TA1 core
*      Revised: 91-09-18 by B-G Bladh     , SETFO/TS
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*      Revised: 02-07-09 by W. Henning    , fixed out of bounds 4 XREA
*
************************************************************************
*
       SUBROUTINE WX146
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       include'cominp.h'
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ0181(1)    ,  BARR   (1)    ),
     &  (XZ0231(1)    ,  DARR   (1)    ),
     &  (XZ0301(1)    ,  IARR   (1)    ),
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ0581(1)    ,  UARR   (1)    )
       EQUIVALENCE
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1366(1)    ,  BBVR   (1)    ),
     &  (XZ1375       ,  BBDLI         ),
     &  (XZ1378       ,  BBERR         )
       EQUIVALENCE
     &  (XZ1382       ,  BBFLU         ),
     &  (XZ1384       ,  BBHLI         ),
     &  (XZ1389       ,  BBREA         ),
     &  (XZ1391       ,  BBTS          )
       EQUIVALENCE
     &  (XZ1479       ,  ISAM          ),
     &  (XZ1481(1)    ,  NCONDU (1)    ),
     &  (XZ1483       ,  MNLY          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1499       ,  NG            ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1558       ,  TYPCOR        ),
     &  (XZ1596       ,  FILECH        ),
     &  (XZ1604(1)    ,  IDENTX (1)    ),
     &  (XZ1624       ,  MNL           ),
     &  (XZ1626(1)    ,  OBJE   (1)    )
       EQUIVALENCE
     &  (XZ2567       ,  BBLAY         ),
     &  (XZ3289(1)    ,  FARR   (1)    ),
     &  (XZ3390(1)    ,  WDGFUN (1)    )
*
       EQUIVALENCE
     &  (XZ2727       ,  MABOOS        ),
     &  (XZ2728       ,  VOBOOS        ),
     &  (XZ2729       ,  REBOOS        ),
     &  (XZ2730       ,  TRBOOS        ),
     &  (XZ2732       ,  BBOOS         ),
     &  (XZ2734       ,  TELOSS        ),
     &  (XZ2735       ,  STKFAC        ),
     &  (XZ2799       ,  SPFADJ        ),
     &  (MNLCOD       ,  MNC    (1)    )
*
       REAL      AARR(200),BARR(100),DARR(100),FARR(100),
     &           CPUMIN,CRAZY,COV,TYPCOR,YOKDIS,
     &           MABOOS, VOBOOS,REBOOS, TRBOOS, TELOSS, STKFAC
*
       INTEGER   KGROUP(9),KCODE(9),IARR(100),ISAM,NCONDU(9),I,IHLP,
     &           ITML,ITRACE,IWDG,JFC,MNLY,NG,NHELP,NWILI,NWOULI
*
       DIMENSION IDENTX(18),OBJE(6),
     &           UARR(100),KCON1(4),MNC(2),
     &           TARR(100),WDGFUN(9),
     &           CWDG(9)
       CHARACTER TEST2*4,IDENTX*4,COBJ*20,OBJE*4,KEY3*8,
     &           UARR*4,FILECH*24,KCON1*6,MNC*4,
     &           TARR*8,MNL*8,TAPS*4,CTAN*3,WDGFUN*4,
     &           CBLTX*6,CTRACE*40,CTR3*3,CWDG*1
*
       LOGICAL   BBERR,BBTS,BBDLI,BBHLI,BBFLU,BBREA,
     &           BBVR(4),BB79,FBB,BBHELP(10),BBLAY,BBOOS
*
       EXTERNAL FBB
*
       print*,'Inside wx146.f'
       DATA    CRAZY/1.e-20/
       DATA    TEST2/'    '/
       DATA    CWDG/'A','B','C','D','E','F','G','H','I'/
*
CC*SEBL End of the declarations block.
*
C... Initialisation of certain variables
*
CC*SBBL Start of the initialisation block
*
C... Initialise BBHELP(*)
*
       DO 5 I=1,10
    5  BBHELP(I)=.FALSE.
*
       BBERR=.FALSE.
*
CC*SEBL End of the initialisation block
*
C... Analyse the data
*
       KEY3='       1'
       NWILI=RWILI
*
C... Get XSTRRD from the data-base
*
cc       CALL GETRD('WINDING ','NSTRRD  ',KEY3,XSTRRD,'X1* ',NWILI,8,1)
*
C... Initialise the indata
*
CC*SBBL Start of the initialisation block
*
C... For each winding
*
CD      write(*,*) 'in wx146 b4 DO 20'
       DO 20 IWDG=1,9
       IF(ACOND0(IWDG).EQ.CRAZY) ACOND0(IWDG)=0.
       IF(BPART (IWDG).EQ.CRAZY) BPART (IWDG)=0.
       IF(BWIND (IWDG).EQ.CRAZY) BWIND (IWDG)=0.
       IF(COVCBL(IWDG).EQ.CRAZY) COVCBL(IWDG)=0.
       IF(COVSTR(IWDG).EQ.CRAZY) COVSTR(IWDG)=0.
       IF(CURD0 (IWDG).EQ.CRAZY) CURD0 (IWDG)=0.
       IF(BDUCT (IWDG).EQ.CRAZY) BDUCT (IWDG)=0.
       IF(DUYOKE(IWDG).EQ.CRAZY) DUYOKE(IWDG)=0.
       IF(DLYOKE(IWDG).EQ.CRAZY) DLYOKE(IWDG)=0.
       IF(FILLF (IWDG).EQ.CRAZY) FILLF (IWDG)=0.
       IF(HPART (IWDG).EQ.CRAZY) HPART (IWDG)=0.
       IF(MXCURD(IWDG).EQ.CRAZY) MXCURD(IWDG)=4.5
       IF(NCDUCT(IWDG).EQ.CRAZY) NCDUCT(IWDG)=-1.
       IF(PRESSM(IWDG).EQ.CRAZY) PRESSM(IWDG)=0.
       IF(TENSM (IWDG).EQ.CRAZY) TENSM (IWDG)=0.
       IF(XOILGR(IWDG).EQ.CRAZY) XOILGR(IWDG)=-1.
CD      ! ---debug
CD      write(*,*) 'in DO 20 above bad line.'
CD      write(*,*) 'IWDG=',IWDG
CD      write(*,*) 'XOILGR(IWDG)=', XOILGR(IWDG)
CD      write(*,*) 'XREA(IWDG)=', XREA(IWDG)
       IF (IWDG.LE. 4) THEN
            IF(XREA  (IWDG).EQ.CRAZY) XREA  (IWDG)=0.
       END IF
       IF(XSTRRD(IWDG).EQ.CRAZY) XSTRRD(IWDG)=0.
   20  CONTINUE
CD      write(*,*)'in wx146 aft DO 20'
*
       IF(BLIMB.EQ.CRAZY) BLIMB=0.
       IF(CPUFIX.EQ.CRAZY) CPUFIX=0.
       IF(CPUIND.EQ.CRAZY) CPUIND=0.
       IF(CPUPT.EQ.CRAZY) CPUPT=0.
       IF(DCORE.EQ.CRAZY) DCORE=0.
       IF(EVALPK.EQ.CRAZY) EVALPK=0.
       IF(EVALP0.EQ.CRAZY) EVALP0=0.
       IF(FONAN.EQ.CRAZY) FONAN=0.
       IF(HLIMB.EQ.CRAZY) HLIMB=0.
       write(*,*) 'wx146 line 192', HLIMB
       IF(USHORE.EQ.CRAZY) USHORE=0.
       IF(BLTOPM.EQ.CRAZY) BLTOPM=0.
       IF(BTANK.EQ.CRAZY) BTANK=0.
       IF(BTANKM.EQ.CRAZY) BTANKM=0.
       IF(CALCT.EQ.CRAZY) CALCT=0.
       IF(DCOCOV.EQ.CRAZY) DCOCOV=0.
       IF(DLMBMA.EQ.CRAZY) DLMBMA=0.
       IF(DLMBMI.EQ.CRAZY) DLMBMI=0.
       IF(DPHAS.EQ.CRAZY) DPHAS=0.
       IF(DPHSL.EQ.CRAZY) DPHSL=0.
       IF(EXTANK.EQ.CRAZY) EXTANK=0.
       IF(GTRPM.EQ.CRAZY) GTRPM=0.
       IF(HLIMBM.EQ.CRAZY) HLIMBM=0.
       IF(HLIMBN.EQ.CRAZY) HLIMBN=0.
       IF(HTANK.EQ.CRAZY) HTANK=0.
       IF(HTANKM.EQ.CRAZY) HTANKM=0.
       IF(HYOKAD.EQ.CRAZY) HYOKAD=0.
       IF(LTANK.EQ.CRAZY) LTANK=0.
       IF(MAXPK.EQ.CRAZY) MAXPK=0.
       IF(MAXP0.EQ.CRAZY) MAXP0=0.
       IF(QMONT.EQ.CRAZY) QMONT=0.
       IF(RLTANM.EQ.CRAZY) RLTANM=0.
       IF(SOUNDM.EQ.CRAZY) SOUNDM=0.
       IF(TOPDTO.EQ.CRAZY) TOPDTO=0.
       IF(TTOILM.EQ.CRAZY) TTOILM=0.
       IF(TWINDM.EQ.CRAZY) TWINDM=0.
       IF(UMAXPU.EQ.CRAZY) UMAXPU=0.
       IF(UTURN.EQ.CRAZY) UTURN=0.
*
CC*SEBL End of the initialisation block
*
C... Initialise certain array variables
*
CC*SBBL Start of the initialisation block
*
C... Initialise AARR(*)
*
       DO 31 I=1,200
   31  AARR(I)=0.
*
C... Initialise BARR(*),DARR(*),IARR(*),TARR(*),UARR(*)
*
       DO 32 I=1,100
       BARR(I)=0.
       DARR(I)=0.
       IARR(I)=0
       TARR(I)='        '
   32  UARR(I)='    '
*
CC*SEBL End of the initialisation block
*
C... Write indata to vectors : AARR,IARR,TARR & BARR
*
       IF(MNC(2).EQ.' ') MNC(2)=TEST2
       MNL='        '
       READ (MNLCOD,88) MNL
*
C... Read the costfile data
*
       CALL EXTIN(MNL,JFC,FILECH,BB79,MNLY)
*
C... Object
*
CC*SBBL Start of the object block
*
C... Initialise the Object string
*
       DO 40 I=1,6
   40  OBJE(I)='    '
*
       KEY3='        '
*
C... Read in the object name from the dialog area
*
cc       CALL DMOVC(COBJ,'DSYOBJ ',1,20)
cc       READ(COBJ,41)(OBJE(I),I=1,5)
*
CC*SEBL End of the object block
*
C... Identification
*
CC*SBBL Start of the identification block
*
C... Initialise the Identification string
*
cc       DO 50 I=1,18
cc   50  IDENTX(I)=' '
cc*
cc       READ(IDENT,51)(IDENTX(I),I=1,10)
*
CC*SEBL End of the identification block
*
C... Trace
*
CC*SBBL Start of the Trace block
*
cc       ITRACE=INDEX(IDENT,'TRACE')
cc       IF (ITRACE.GE.1) THEN
cc         CTRACE='  '
cc         CTRACE(1:40-(ITRACE+4))=IDENT(1+(ITRACE+4):40)
*
cc         DO 3300 I=1,9
cc           WRITE(CTR3,90)I
cc           BBHELP(I)=.FALSE.
cc           IF (INDEX(CTRACE,CTR3).GE.1) BBHELP(I)=.TRUE.
cc 3300    CONTINUE

cc        BBHELP(10)=.FALSE.
cc         IF (INDEX(CTRACE,' 10 ').GE.1) BBHELP(10)=.TRUE.

cc       END IF
*
CC*SEBL End of the Trace block
*
       IF (MAXP0.GT.0.) EVALP0=(-1.)*MAXP0
       IF (MAXPK.GT.0.) EVALPK=(-1.)*MAXPK
       IARR(1)=NPHAS
*
C...   IARR(3) = Maximum number of optimisation calculations
*
       IARR(3)=2500
       AARR(2)=FREQ
       AARR(4)=USHORE
       AARR(6)=EVALP0
       AARR(7)=EVALPK
       TARR(60)=OPERAT
       AARR(13)=0.
       AARR(14)=0.
*
C... AARR(25) = Monthly inflation rate for costing purposes
*
       AARR(25)=1.
*
C... AARR(34) = Loss scale factor
*
       AARR(34)=1.
*
C... AARR(35) = Cost scale factor
*
       AARR(35)=1.
*
C... AARR(60) = Mass-factor
*
       AARR(60)=1.
*
C... Determine the No. of terminals
*
       ITML=1
*
C... Check upto and including 4 terminals
*
C,,, If still less than or equal to 4
*
  600  IF (ITML .LE. 4) THEN
*
C... Check the terminal data
*
C,,, If data has been entered
*
          IF (TNODE(ITML).EQ.' ') GOTO 699
          NG=ITML
          TAPS='NO'
          IF (KTYPRW(ITML).NE.'NO') TAPS='YES'
          KCON1(ITML)=KCON(ITML)
          IF (KCON(ITML).EQ.'YA ') KCON(ITML)='Y/A'
          IF (KCON(ITML).EQ.'IA ') KCON(ITML)='I/A'
          IF (KCON(ITML).EQ.'YN')  KCON1(ITML)='Y'
          IF (KCON(ITML).EQ.'I/A') KCON1(ITML)='I/AUTO'
          IF (KCON(ITML).EQ.'Y/A') KCON1(ITML)='Y/AUTO'
          AARR(10+ITML)=SRATE(ITML)
          AARR(15+ITML)=UNLINE(ITML)
          IF (ITML.LE.3) TARR(5+ITML)=TAPS
          BBVR(ITML)=.FALSE.
          IF (TAPS.EQ.'YES') BBVR(ITML)=.TRUE.
*
C... Allocate tapping details
*
C,,, If taps
*
          IF (TAPS.EQ.'YES') THEN
              IARR(5+ITML)=NINT(NPSTEP(ITML))
              IARR(10+ITML)=NINT(NMSTEP(ITML))
              AARR(20+ITML)=PUSTEP(ITML)
              TARR(10+ITML)=KTYPRW(ITML)
              FARR(52+ITML)=CALCT
*
C... IF NO TAPS
*
          ELSE
              IARR(5+ITML)=0
              IARR(10+ITML)=0
              AARR(20+ITML)=0.
              TARR(10+ITML)='NO'
              FARR(52+ITML)=0.
          END IF
*
          TARR(20+ITML)=KCON1(ITML)
          AARR(25+ITML)=USURG(ITML)
          AARR(30+ITML)=SLINE(ITML)
          ITML=ITML+1
          GOTO 600
  699     CONTINUE
       END IF
*
C... Search for a common node for an auto-transformer
C... and allocate terminal numbers to each winding
*
       CALL OMF013(NG,NWILI,WNOD1,WNOD2,TNODE,KCON,UNLINE,
     &             KGROUP,KWIFUN,KCODE)
*
C... Automatic layout specification
*
       BBLAY=.FALSE.
       IF (LAYOUT(1:1).EQ.'Y') BBLAY=.TRUE.
*
C... Allocate winding data
*
       YOKDIS=0.
*
       DO 999 IWDG=1,NWILI
       WDGFUN(IWDG)=KWIFUN(IWDG)
       IARR(20+IWDG)=KGROUP(IWDG)
       TARR(60+IWDG)=CBLTYP(IWDG)
       TARR(30+IWDG)=KWITYP(IWDG)
       IARR(30+IWDG)=KCODE(IWDG)
*
C... Allocate current densities, etc.
*
C,,, Optimise
*
       IF (WNDOPT(IWDG)(1:1).EQ.'Y') THEN
          AARR(50+IWDG)=(-1.)*MXCURD(IWDG)
          AARR(60+IWDG)=(-1.)*ACOND0(IWDG)
*
C... Space factor has not been stated
*
          IF(FILLF(IWDG).LT.0.001) THEN
              CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &  'WX146:Space factor is missing for winding '//CWDG(IWDG)//' .')
          ENDIF
*
C... WINDING fixed
*
       ELSE
          IF (ACOND0(IWDG).LT.1.E-8) AARR(50+IWDG)=MXCURD(IWDG)
          AARR(60+IWDG)=ACOND0(IWDG)
          AARR(90+IWDG)=BWIND(IWDG)
       END IF
*
       AARR(70+IWDG)=BDUCT(IWDG)
       AARR(80+IWDG)=FILLF(IWDG)
       AARR(100+IWDG)=DUYOKE(IWDG)+DLYOKE(IWDG)
       IF (AARR(100+IWDG).GT.YOKDIS) YOKDIS=AARR(100+IWDG)
       DARR(40+IWDG)=EXTRAR(IWDG)
  999  DARR(50+IWDG)=HCLAC(IWDG)
*
       NWOULI=3
       IF (NPHAS.EQ.1.) NWOULI=2
       IF (CORTYP.EQ.'TY-1'.AND.NPHAS.EQ.1.) NWOULI=3
       IF (CORTYP.EQ.'EY  ') NWOULI=1
       IARR(51)=NWOULI
*
C... Store the now established No. of wound limbs
*
cc       CALL PUTR('CORE    ','NWOULI  ',KEY3, FLOAT(NWOULI), 1)
*
C... Sidelimbed core ?
*
       TARR(51)='YES     '
       IF (CORTYP.EQ.'T   '.OR.CORTYP.EQ.'D   ') TARR(51)='NO      '
*
       UARR(22) = CORTYP
       TYPCOR=0.
       IF (CORTYP.EQ.'D   ') TYPCOR=1.
       IF (CORTYP.EQ.'T   ') TYPCOR=3.
       IF (CORTYP.EQ.'EY  ') TYPCOR=7.
       IF (CORTYP.EQ.'DY  ') TYPCOR=8.
       IF (CORTYP.EQ.'TY-1') TYPCOR=9.
       IF (CORTYP.EQ.'TY-3') TYPCOR=10.
*
       TYPCOR = ANINT(TYPCOR)
*
*      Step lap joint in core
       UARR(23) = STEPLA
*
       AARR(141)=DPHAS
       AARR(142)=DPHSL
       BARR(31)=QMONT
       BARR(30)=TWSUP
       AARR(143)=DCORE
       AARR(146)=BLIMB
*
C... EPS Optimisation constant
*
       BARR(32)=0.
*
C... FEPS Optimisation constant
*
       BARR(33)=-1.
*
C... DRV Optimisation constant
*
       BARR(34)=0.
*
C... AF Optimisation constant
*
       BARR(35)=0.
*
C... PARAM(3) Optimisation constant
*
       BARR(36)=1.9
*
C... PARAM(13) Optimisation constant
*
       BARR(37)=0.
*
       UARR(20)=OPTTNK
*
C... Allocate tank dimensions
*
C,,, If tank dimensions fixed
*
       IF (OPTTNK(1:1).EQ.'N') THEN
          EXTANK=LTANK
          DCOCOV=HTANK
          DWITA=BTANK
       END IF
*
       IARR(53)=3
       READ(CKTANK,98) CTAN,NHELP
       IF(NHELP.EQ.0) NHELP=2
       IF(CTAN.EQ.'TMY') IARR(53)=NHELP
       IF(CTAN.EQ.'TAA') IARR(53)=9+NHELP
       IF(CTAN.EQ.'TAD') IARR(53)=11
       IF(CTAN.EQ.'TBA') IARR(53)=11+NHELP
       IF(CTAN.EQ.'NO ') IARR(53)=0
       IF (CKTANK.EQ.' ') IARR(53)=-1
       AARR(151)=DWITA
       AARR(152)=EXTANK
       AARR(153)=DCOCOV
       TARR(56)=KCOOL
       AARR(155)=65.
       IF(TWINDM.GT.25.) AARR(155)=TWINDM
       AARR(156)=60.
       IF(TTOILM.GT.25.) AARR(156)=TTOILM
*
C...   AARR(80) = Core space factor with NO additional varnish
*
       AARR(80)=0.96
*
C...   AARR(90) = Core space factor with additional varnish
*
       AARR(90)=0.935
*
C...   Booster data
       MABOOS = BOOSMA
       VOBOOS = BOOSVO
       REBOOS = BOOSRE
       BBOOS  = BBBOOS
       TRBOOS = BOOSTR
*
       TELOSS = TEMPLO
       STKFAC = STACKF
       SPFADJ = SPFADU

       BBHLI=.FALSE.
       print *,REOPT,'WX146'
       BBHLI=FBB('WX146 ',BBERR,BBHELP,BBTS,JFC,REOPT(1))
       BBDLI=.FALSE.
       BBDLI=FBB('WX146 ',BBERR,BBHELP,BBTS,JFC,REOPT(2))
       BBFLU=.FALSE.
       BBFLU=FBB('WX146 ',BBERR,BBHELP,BBTS,JFC,REOPT(3))
       BBREA=.FALSE.
       BBREA=FBB('WX146 ',BBERR,BBHELP,BBTS,JFC,REOPT(4))
*
C... Limb height has not been stated
*
       IF(.NOT.BBHLI.AND.HLIMB.LT.100.)
     &   CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &               'WX146:Limb height is missing.')
*
C... Core diameter has not been stated
*
       IF(.NOT.BBDLI.AND.DCORE.LT.50.)
     &   CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &               'WX146:Core diameter is missing.')
*
C... Flux density has not been stated
*
       IF(.NOT.BBFLU.AND.BLIMB.LT.0.1)
     &   CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &               'WX146:Flux density is missing.')
*
C... Reactance has not been stated
*
       IF(.NOT.BBREA.AND.USHORE.LT.0.1.AND.(BBHLI.OR.BBDLI))
     &   CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &               'WX146:Reactance is missing.')
*
C... Optimisation variables adjusted depending on YES/NO answers
*
CC*SBBL Start of the YES/NO block
*
       IF(BBHLI) HLIMB=-1.*HLIMB
       write(*,*) 'wx146 line 597', HLIMB
       IF(BBDLI) AARR(143)=-1.*DCORE
       IF(BBFLU) AARR(146)=-1.*BLIMB
       IF(BBREA) AARR(4)=0.
*
CC*SEBL End of the YES/NO block
*
C... For each winding
*
       DO 1299 IWDG=1,NWILI
*
CC*SOFF Switch off the structure analyser (Not all calls need be shown)
*
       IARR(40+IWDG)=1
       IF (NGROUP(IWDG).NE.1.)  IARR(40+IWDG)=NINT(NGROUP(IWDG))
       AARR(40+IWDG)=1.
       IF (FRACT(IWDG).NE.1.)   AARR(40+IWDG)=FRACT(IWDG)
       AARR(120+IWDG)=120.
       IF (TENSM(IWDG).GT.1.)   AARR(120+IWDG)=TENSM(IWDG)
       AARR(130+IWDG)=55.
       IF (PRESSM(IWDG).GT.1.)  AARR(130+IWDG)=PRESSM(IWDG)
*
CC*SON Switch on the structure analyser (Next 3 lines will be analysed)
*
C... Encode a CHARACTER alteration
*
      TARR(40+IWDG)='CU      '
       IF (CONMAT(IWDG)(1:1).EQ.'A') TARR(40+IWDG)='AL      '
*
       BARR(IWDG)=-1.
       IF (NCDUCT(IWDG).GE.0.)  BARR(IWDG)=ANINT(NCDUCT(IWDG))
*
C... Encode a REAL alteration
*
       BARR(10+IWDG)=-1.
       IF (HPART(IWDG).GT.0.)   BARR(10+IWDG)=HPART(IWDG)
*
CC*SOFF Switch off the structure analyser (Not all calls need be shown)
*
       AARR(110+IWDG)=2.44
       IF (BPART(IWDG).GT.0.)    AARR(110+IWDG)=BPART(IWDG)
       DARR(30+IWDG)=COVSTR(IWDG)
       DARR(IWDG)=0.
       IF(XOILGR(IWDG).GE.0.)   DARR(IWDG)=ANINT(XOILGR(IWDG))
       COV=COVSTR(IWDG)+COVCBL(IWDG)
       BARR(20+IWDG)=-1.
       IF (COV.GT.0.)            BARR(20+IWDG)=COV
       CBLTX=CBLTYP(IWDG)
       IF(CBLTYP(IWDG).EQ.'SP  ')  TARR(60+IWDG)='SP      '
       IF(CBLTYP(IWDG).EQ.'OR  ')  TARR(60+IWDG)='OR      '
       IF(CBLTYP(IWDG).EQ.'LR  ')  TARR(60+IWDG)='LR      '
       IF(CBLTYP(IWDG).EQ.'TC  ')  TARR(60+IWDG)='TC      '
       UARR(IWDG)='NO  '
       IF(COMBWI(IWDG)(1:1).EQ.'Y')UARR(IWDG)='YES '
       UARR(10+IWDG)='NO  '
       IF(CBLTYP(IWDG).EQ.'TC  ')  UARR(10+IWDG)='YES '
       IF(XSTRRD(IWDG).GT.1.5)    DARR(20+IWDG)=XSTRRD(IWDG)
       UARR(30+IWDG)=CONVAR(IWDG)
       UARR(40+IWDG)=TCEPOX(IWDG)
*
       ISAM=1
       IF (FBB('WX146 ',BBERR,BBHELP,BBTS,JFC,UARR(IWDG))) ISAM=2
*
       NCONDU(IWDG)=1
       IF(TARR(40+IWDG).EQ.'AL') NCONDU(IWDG)=2
       IF (FBB('WX146 ',BBERR,BBHELP,BBTS,JFC,UARR(10+IWDG)))
     &           NCONDU(IWDG)=3
*
CC*SON Switch on the structure analyser (Not all calls need be shown)
*
 1299  CONTINUE
*
CC*SOFF Switch off the structure analyser (Not all calls need be shown)
*
       DO 1399 I=1,NG
       IF(XREA(I).GT.0.)        AARR(35+I)=XREA(I)
 1399  CONTINUE
*
       IF (ABS(HLIMB).GT. 100.) DARR(20)=HLIMB
       DARR(10)=5000.
       IF (HLIMBM.GT.0.)        DARR(10)=HLIMBM
       DARR(40)=AMAX1(600.,(YOKDIS+350.))
       IF (HLIMBN.GT.DARR(40))  DARR(40)=HLIMBN
       AARR(5)=5.
       IF (UMAXPU.GE.0.) AARR(5)=UMAXPU
       IF (BLTOPM.NE.0.) THEN
          IF (.NOT.(COREMA.EQ.'HI-B'.AND.BLTOPM.EQ.1.98)) THEN
             IF (.NOT.(COREMA.EQ.'ZDKH'.AND.BLTOPM.EQ.1.98)) THEN
                IF (.NOT.(COREMA.EQ.'PLJT'.AND.BLTOPM.EQ.1.98)) THEN
                   IF (.NOT.(COREMA.EQ.'M5'  .AND.BLTOPM.EQ.1.95)) THEN
                      AARR(147)=BLTOPM
                   END IF
                END IF
             END IF
          END IF
       END IF
*
       AARR(154)=60.
       IF (FONAN.GT.0.)         AARR(154)=FONAN
       TARR(2)='NO      '
       IF (TYPREG.EQ.'VFR')     TARR(2)='YES     '
       IF (UTURN.GT.0)          AARR(145)=UTURN
       TARR(73)='NO      '
                                TARR(1)='M5      '
       IF (COREMA.EQ.'HI-B')    TARR(1)='HI-B    '
       IF (COREMA.EQ.'ZDKH')    TARR(1)='ZDKH    '
       IF (COREMA.EQ.'PLJT')    TARR(1)='PLJT    '
       IF (COREMA.EQ.'EXT ')    TARR(1)='EXT     '
*
       UARR(21)= CORESE
*
       IF (CORESE.EQ.'TA1     ') THEN
                  XZ2752 = LAMTH /1000.
                  XZ2753 = LAMSPF
                  XZ2754 = LAMMW /1000.
                  XZ2755 = LAMOVL/1000.
                  XZ2756 = ACCLON
                  XZ2757 = ACCTRA
                  XZ2758 = ACCVER
                  XZ2759 = AMBTEM
                  XZ2760 = LAMSTP/1000.
                  XZ2761 = LOSCOR
                  XZ2762 = TA1HOL/1000.
                  XZ2770 = FLPLMA
       ENDIF
       XZ2780 = NCOOLI
*
*
       AARR(173)=500.
       IF (GTRPM.GT.0.)         AARR(173)=GTRPM
       AARR(174)=4700.
       IF (HTANKM.GE.0.)        AARR(174)=HTANKM
       AARR(175)=3660.
       IF (BTANKM.GE.0.)        AARR(175)=BTANKM
       AARR(176)=13900.
       IF (RLTANM.GE.0.)        AARR(176)=RLTANM
       IF (SOUNDM.GE.0.)        AARR(177)=SOUNDM
CCC    CPUMIN=0.03
CCC    AARR(161)=CPUMIN
CCC    IF (CPUIND.GE.CPUMIN)    AARR(161)=CPUIND
       AARR(161)=CPUIND
       AARR(168)=-1.
       IF (CPUFIX.NE.0.)        AARR(168)=CPUFIX
       TARR(3)='NO      '
       IF (ALSHLD(1:1).EQ.'Y')  TARR(3)='YES     '
*
C--- T146AC(1) is spare until further notice
*
       TARR(4)='NO      '
       IF (T146AC(2)(1:1).EQ.'Y') TARR(4)='YES     '
*
C... TARR(95) was used for double shell windings in ADDTXT
C... as well as TARR(4)
C... At a convenient moment check if this is necessary
*
       TARR(95)='NO      '
       IF (T146AC(2)(1:1).EQ.'Y') TARR(95)='YES     '
*
       TARR(78)='NO      '
       IF (DETPRE(1:1).EQ.'Y')   TARR(78)='YES     '
       TARR(82)='NO      '
*****  IF (CAL009(1:1).EQ.'Y')   TARR(82)='YES     '
*
       TARR(75)='NO      '
       IF (SNDSCR(1:1).EQ.'Y')   TARR(75)='YES     '
       TARR(80)='ASEC    '
       IF (KBAND(1:1).EQ.'S')     TARR(80)='STEEL   '
       IF (KBAND(1:1).EQ.'G')     TARR(80)='GLUED   '
       IF (KBAND(1:1).EQ.'P')     TARR(80)='PRESS   '
       AARR(140)=2.
       IF(BLDING.EQ.'4/4 ')  AARR(140)=4.
       IF(TOPDTO.GT.0.1)  AARR(100)=TOPDTO
       DARR(30)=2.
       IF(CVARN.EQ.'YES ')  DARR(30)=1.
       IF(CVARN.EQ.'PART')  DARR(30)=0.
       IF(DLMBMA.GT.0.1)  AARR(171)=DLMBMA
       IF(DLMBMI.GT.0.1)  AARR(170)=DLMBMI
*
       IF(ABS(HYOKAD).GE.1.) THEN
          IF(CORESE.EQ. 'TBA ')  AARR(160)=(-1.)*HYOKAD
          IF(CORESE.EQ. 'TCA ')  AARR(150)=HYOKAD
       END IF
*
CC*SON Switch on the structure analyser (Not all calls need be shown)
*

       print*,'coming out of wx146.f'
       RETURN
*
   41  FORMAT(5A4)
   51  FORMAT(10A4)
   88  FORMAT(A8)
   90  FORMAT(' ',I1,' ')
   98  FORMAT(A3,I1)
*
C... List of definitions of the array items used in WX146
*
*    TEXT(  1) = I/O CORE ROUTINE             H'/,
*    TEXT(  2) = CALC.IN LMIN                 H'/,
*    TEXT(  3) = CALC.IN ADDAT                H'/,
*    TEXT(  4) = CYCLES IN LMIN               H'/,
*    TEXT(  5) = 10 CYCL.IN LMIN              H'/,
*    TEXT(  6) =                               : <- - - - - - - Not used
*    TEXT(  7) = LINK CHANGES                 H'/,
*    TEXT(  8) = CALL FROM INPUT              H'/,
*    TEXT(  9) = CALL FROM FUNK               H'/,
*    TEXT( 10) = MASS/COST 6                  H'/
*
C* START OF IARR ***********************************
*
*    IARR( 01) = Number of phases           0  : Input    value
*    IARR( 02) = Trnsp.prof. code           0  : Standard value
*    IARR( 03) = Max No of calcs.        2500  : Standard value
*    IARR( 04) =                            0  : <- - - - - - - Not used
*    IARR( 05) =                            0  : <- - - - - - - Not used
*    IARR( 06) = No.of + steps T1           0  : Input    value
*    IARR( 07) = No.of + steps T2           0  : Input    value
*    IARR( 08) = No.of + steps T3           0  : Input    value
*    IARR( 09) =                            0  : <- - - - - - - Not used
*    IARR( 10) =                            0  : <- - - - - - - Not used
*    IARR( 11) = No.of - steps T1           0  : Input    value
*    IARR( 12) = No.of - steps T2           0  : Input    value
*    IARR( 13) = No.of - steps T3           0  : Input    value
*    IARR( 14) =                            0  : <- - - - - - - Not used
*    IARR( 15) =                            0  : <- - - - - - - Not used
*    IARR( 16) =                            0  : <- - - - - - - Reserved
*    IARR( 17) =                            0  : <- - - - - - - Reserved
*    IARR( 18) =                            0  : <- - - - - - - Reserved
*    IARR( 19) =                            0  : <- - - - - - - Reserved
*    IARR( 20) =                            0  : <- - - - - - - Not used
*    IARR( 21) = Term.connect. W1           0  : Input    value
*    IARR( 22) = Term.connect. W2           0  : Input    value
*    IARR( 23) = Term.connect. W3           0  : Input    value
*    IARR( 24) = Term.connect. W4           0  : Input    value
*    IARR( 25) = Term.connect. W5           0  : Input    value
*    IARR( 26) = Term.connect. W6           0  : Input    value
*    IARR( 27) = Term.connect. W7           0  : Input    value
*    IARR( 28) = Term.connect. W8           0  : Input    value
*    IARR( 29) = Term.connect. W9           0  : Input    value
*    IARR( 30) =                            0  : <- - - - - - - Not used
*    IARR( 31) = Function code W1           0  : Input    value
*    IARR( 32) = Function code W2           0  : Input    value
*    IARR( 33) = Function code W3           0  : Input    value
*    IARR( 34) = Function code W4           0  : Input    value
*    IARR( 35) = Function code W5           0  : Input    value
*    IARR( 36) = Function code W6           0  : Input    value
*    IARR( 37) = Function code W7           0  : Input    value
*    IARR( 38) = Function code W8           0  : Input    value
*    IARR( 39) = Function code W9           0  : Input    value
*    IARR( 40) =                            0  : <- - - - - - - Not used
*    IARR( 41) = No.Par.groups W1           1  : Standard value
*    IARR( 42) = No.Par.groups W2           1  : Standard value
*    IARR( 43) = No.Par.groups W3           1  : Standard value
*    IARR( 44) = No.Par.groups W4           1  : Standard value
*    IARR( 45) = No.Par.groups W5           1  : Standard value
*    IARR( 46) = No.Par.groups W6           1  : Standard value
*    IARR( 47) = No.Par.groups W7           1  : Standard value
*    IARR( 48) = No.Par.groups W8           1  : Standard value
*    IARR( 49) = No.Par.groups W9           1  : Standard value
*    IARR( 50) =                            0  : <- - - - - - - Not used
*    IARR( 51) = No. Wound limbs            3  : Input    value
*    IARR( 52) =                            0  : <- - - - - - - Not used
*    IARR( 53) = Tank type-code             0  : Input    value
*    IARR( 54) =                            0  : <- - - - - - - Not Used
*    IARR( 55) =                            0  : <- - - - - - - Not used
*    IARR( 56) =                            0  : <- - - - - - - Not used
*    IARR( 57) =                            0  : <- - - - - - - Not used
*    IARR( 58) =                            0  : <- - - - - - - Not used
*    IARR( 59) =                            0  : <- - - - - - - Not used
*    IARR( 60) =                            0  : <- - - - - - - Not used
*    IARR( 61) =                            0  : <- - - - - - - Reserved
*    IARR( 62) =                            0  : <- - - - - - - Reserved
*    IARR( 63) =                            0  : <- - - - - - - Reserved
*    IARR( 64) =                            0  : <- - - - - - - Reserved
*    IARR( 65) =                            0  : <- - - - - - - Reserved
*    IARR( 66) =                            0  : <- - - - - - - Reserved
*    IARR( 67) =                            0  : <- - - - - - - Reserved
*    IARR( 68) =                            0  : <- - - - - - - Reserved
*    IARR( 69) =                            0  : <- - - - - - - Reserved
*    IARR( 70) =                            0  : <- - - - - - - Reserved
*    IARR( 71) =                            0  : <- - - - - - - Reserved
*    IARR( 72) =                            0  : <- - - - - - - Reserved
*    IARR( 73) =                            0  : <- - - - - - - Reserved
*    IARR( 74) =                            0  : <- - - - - - - Reserved
*    IARR( 75) =                            0  : <- - - - - - - Reserved
*    IARR( 76) =                            0  : <- - - - - - - Reserved
*    IARR( 77) =                            0  : <- - - - - - - Reserved
*    IARR( 78) =                            0  : <- - - - - - - Reserved
*    IARR( 79) =                            0  : <- - - - - - - Reserved
*    IARR( 80) =                            0  : <- - - - - - - Reserved
*    IARR( 81) =                            0  : <- - - - - - - Not used
*    IARR( 82) =                            0  : <- - - - - - - Not used
*    IARR( 83) =                            0  : <- - - - - - - Not used
*    IARR( 84) =                            0  : <- - - - - - - Not used
*    IARR( 85) =                            0  : <- - - - - - - Not used
*    IARR( 86) =                            0  : <- - - - - - - Not used
*    IARR( 87) =                            0  : <- - - - - - - Not used
*    IARR( 88) =                            0  : <- - - - - - - Not used
*    IARR( 89) =                            0  : <- - - - - - - Not used
*    IARR( 90) =                            0  : <- - - - - - - Not used
*    IARR( 91) =                            0  : <- - - - - - - Not used
*    IARR( 92) =                            0  : <- - - - - - - Not used
*    IARR( 93) =                            0  : <- - - - - - - Not used
*    IARR( 94) =                            0  : <- - - - - - - Not used
*    IARR( 95) =                            0  : <- - - - - - - Not used
*    IARR( 96) =                            0  : <- - - - - - - Not used
*    IARR( 97) =                            0  : <- - - - - - - Not used
*    IARR( 98) =                            0  : <- - - - - - - Not used
*    IARR( 99) =                            0  : <- - - - - - - Not used
*    IARR(100) =                            0  : <- - - - - - - Not used
*
C* END OF IARR  ************************************
*
C* START OF AARR  **********************************
*
*    AARR( 01) = Reactance T 1-3        0.000  : Standard value
*    AARR( 02) = Frequency (Hz)        50.000  : Input    value
*    AARR( 03) = Reactance T 2-3        0.000  : Standard value
*    AARR( 04) = Reactance T 1-2        0.000  : Input    value
*    AARR( 05) = Overvoltage (%)        5.000  : Standard value
*    AARR( 06) = Eval.Noload loss       0.000  : Input    value
*    AARR( 07) = Eval.Load loss         0.000  : Input    value
*    AARR( 08) =                        0.000  : <- - - - - - - Not used
*    AARR( 09) = Eval.Sum loss          0.000  : Standard value
*    AARR( 10) =                        0.000  : <- - - - - - - Not used
*    AARR( 11) = Rated power T1         0.000  : Input    value
*    AARR( 12) = Rated power T2         0.000  : Input    value
*    AARR( 13) = Rated power T3         0.000  : Input    value
*    AARR( 14) = Rated power T4         0.000  : Input    value
*    AARR( 15) = Loss ratio             0.000  : Standard value
*    AARR( 16) = Main voltage T1        0.000  : Input    value
*    AARR( 17) = Main voltage T2        0.000  : Input    value
*    AARR( 18) = Main voltage T3        0.000  : Input    value
*    AARR( 19) = Main voltage T4        0.000  : Input    value
*    AARR( 20) = Max comp.price         0.000  : Standard value
*    AARR( 21) = Reg.volt.T1 (%)        0.000  : Input    value
*    AARR( 22) = Reg.volt.T2 (%)        0.000  : Input    value
*    AARR( 23) = Reg.volt.T3 (%)        0.000  : Input    value
*    AARR( 24) = Product class          0.000  : Standard value
*    AARR( 25) = Monthly Infl.(%)       1.000  : Standard value
*    AARR( 26) = Impulse volt. T1       0.000  : Input    value
*    AARR( 27) = Impulse volt. T2       0.000  : Input    value
*    AARR( 28) = Impulse volt. T3       0.000  : Input    value
*    AARR( 29) = Impulse volt. T4       0.000  : Input    value
*    AARR( 30) = Trnsp.dist in SW       0.000  : Standard value
*    AARR( 31) = Scpower T1 (GVA)       0.000  : Input    value
*    AARR( 32) = Scpower T2 (GVA)       0.000  : Input    value
*    AARR( 33) = Scpower T3 (GVA)       0.000  : Input    value
*    AARR( 34) = Loss Scale fact        1.000  : Standard value
*    AARR( 35) = Cost Scale fact        1.000  : Standard value
*    AARR( 36) = Int.reactor (%)        0.000  : Standard value
*    AARR( 37) = Int.reactor (%)        0.000  : Standard value
*    AARR( 38) = Int.reactor (%)        0.000  : Standard value
*    AARR( 39) = K1 in PT calc.         0.000  : Standard value
*    AARR( 40) = K2 in PT calc.         0.000  : Standard value
*    AARR( 41) = Fraction W1 (pu)       1.000  : Standard value
*    AARR( 42) = Fraction W2 (pu)       1.000  : Standard value
*    AARR( 43) = Fraction W3 (pu)       1.000  : Standard value
*    AARR( 44) = Fraction W4 (pu)       1.000  : Standard value
*    AARR( 45) = Fraction W5 (pu)       1.000  : Standard value
*    AARR( 46) = Fraction W6 (pu)       1.000  : Standard value
*    AARR( 47) = Fraction W7 (pu)       1.000  : Standard value
*    AARR( 48) = Fraction W8 (pu)       1.000  : Standard value
*    AARR( 49) = Fraction W9 (pu)       1.000  : Standard value
*    AARR( 50) =                        0.000  : <- - - - - - - Not used
*    AARR( 51) = Current dens. W1       0.000  : Input    value
*    AARR( 52) = Current dens. W2       0.000  : Input    value
*    AARR( 53) = Current dens. W3       0.000  : Input    value
*    AARR( 54) = Current dens. W4       0.000  : Input    value
*    AARR( 55) = Current dens. W5       0.000  : Input    value
*    AARR( 56) = Current dens. W6       0.000  : Input    value
*    AARR( 57) = Current dens. W7       0.000  : Input    value
*    AARR( 58) = Current dens. W8       0.000  : Input    value
*    AARR( 59) = Current dens. W9       0.000  : Input    value
*    AARR( 60) = Mass-factor            1.000  : Standard value
*    AARR( 61) = Conduct area W1        0.000  : Input    value
*    AARR( 62) = Conduct area W2        0.000  : Input    value
*    AARR( 63) = Conduct area W3        0.000  : Input    value
*    AARR( 64) = Conduct area W4        0.000  : Input    value
*    AARR( 65) = Conduct area W5        0.000  : Input    value
*    AARR( 66) = Conduct area W6        0.000  : Input    value
*    AARR( 67) = Conduct area W7        0.000  : Input    value
*    AARR( 68) = Conduct area W8        0.000  : Input    value
*    AARR( 69) = Conduct area W9        0.000  : Input    value
*    AARR( 70) =                        0.000  : <- - - - - - - Not used
*    AARR( 71) = Main duct ins.W1       0.000  : Input    value
*    AARR( 72) = Main duct ins.W2       0.000  : Input    value
*    AARR( 73) = Main duct ins.W3       0.000  : Input    value
*    AARR( 74) = Main duct ins.W4       0.000  : Input    value
*    AARR( 75) = Main duct ins.W5       0.000  : Input    value
*    AARR( 76) = Main duct ins.W6       0.000  : Input    value
*    AARR( 77) = Main duct ins.W7       0.000  : Input    value
*    AARR( 78) = Main duct ins.W8       0.000  : Input    value
*    AARR( 79) = Main duct ins.W9       0.000  : Input    value
*    AARR( 80) = Fillf.n.ext.varn       0.960  : Standard value
*    AARR( 81) = Space factor W1        0.000  : Input    value
*    AARR( 82) = Space factor W2        0.000  : Input    value
*    AARR( 83) = Space factor W3        0.000  : Input    value
*    AARR( 84) = Space factor W4        0.000  : Input    value
*    AARR( 85) = Space factor W5        0.000  : Input    value
*    AARR( 86) = Space factor W6        0.000  : Input    value
*    AARR( 87) = Space factor W7        0.000  : Input    value
*    AARR( 88) = Space factor W8        0.000  : Input    value
*    AARR( 89) = Space factor W9        0.000  : Input    value
*    AARR( 90) = Fillf.ext.varn         0.935  : Standard value
*    AARR( 91) = Winding width W1       0.000  : Input    value
*    AARR( 92) = Winding width W2       0.000  : Input    value
*    AARR( 93) = Winding width W3       0.000  : Input    value
*    AARR( 94) = Winding width W4       0.000  : Input    value
*    AARR( 95) = Winding width W5       0.000  : Input    value
*    AARR( 96) = Winding width W6       0.000  : Input    value
*    AARR( 97) = Winding width W7       0.000  : Input    value
*    AARR( 98) = Winding width W8       0.000  : Input    value
*    AARR( 99) = Winding width W9       0.000  : Input    value
*    AARR(100) = Temprise topoil        0.000  : Standard value
*    AARR(101) = Yoke-distance W1       0.000  : Input    value
*    AARR(102) = Yoke-distance W2       0.000  : Input    value
*    AARR(103) = Yoke-distance W3       0.000  : Input    value
*    AARR(104) = Yoke-distance W4       0.000  : Input    value
*    AARR(105) = Yoke-distance W5       0.000  : Input    value
*    AARR(106) = Yoke-distance W6       0.000  : Input    value
*    AARR(107) = Yoke-distance W7       0.000  : Input    value
*    AARR(108) = Yoke-distance W8       0.000  : Input    value
*    AARR(109) = Yoke-distance W9       0.000  : Input    value
*    AARR(110) = Temprise MO ONAN       0.000  : Input    value
*    AARR(111) = Cond. width W1         2.440  : Standard value
*    AARR(112) = Cond. width W2         2.440  : Standard value
*    AARR(113) = Cond. width W3         2.440  : Standard value
*    AARR(114) = Cond. width W4         2.440  : Standard value
*    AARR(115) = Cond. width W5         2.440  : Standard value
*    AARR(116) = Cond. width W6         2.440  : Standard value
*    AARR(117) = Cond. width W7         2.440  : Standard value
*    AARR(118) = Cond. width W8         2.440  : Standard value
*    AARR(119) = Cond. width W9         2.440  : Standard value
*    AARR(120) = Temprise MO ONAF       0.000  : Standard value
*    AARR(121) = Tens.stress W1       120.000  : Standard value
*    AARR(122) = Tens.stress W2       120.000  : Standard value
*    AARR(123) = Tens.stress W3       120.000  : Standard value
*    AARR(124) = Tens.stress W4       120.000  : Standard value
*    AARR(125) = Tens.stress W5       120.000  : Standard value
*    AARR(126) = Tens.stress W6       120.000  : Standard value
*    AARR(127) = Tens.stress W7       120.000  : Standard value
*    AARR(128) = Tens.stress W8       120.000  : Standard value
*    AARR(129) = Tens.stress W9       120.000  : Standard value
*    AARR(130) = Temprise MO OFAF       0.000  : Standard value
*    AARR(131) = Max. Press. W1        55.000  : Standard value
*    AARR(132) = Max. Press. W2        55.000  : Standard value
*    AARR(133) = Max. Press. W3        55.000  : Standard value
*    AARR(134) = Max. Press. W4        55.000  : Standard value
*    AARR(135) = Max. Press. W5        55.000  : Standard value
*    AARR(136) = Max. Press. W6        55.000  : Standard value
*    AARR(137) = Max. Press. W7        55.000  : Standard value
*    AARR(138) = Max. Press. W8        55.000  : Standard value
*    AARR(139) = Max. Press. W9        55.000  : Standard value
*    AARR(140) = Core blading           2.000  : Standard value
*    AARR(141) = Dist.betw.phases       0.000  : Input    value
*    AARR(142) = Dist.to.sidelimb       0.000  : Input    value
*    AARR(143) = Core diameter          0.000  : Input    value
*    AARR(144) =                        0.000  : <- - - - - - - Not used
*    AARR(145) = Turn-voltage           0.000  : Standard value
*    AARR(146) = Flux density           0.000  : Input    value
*    AARR(147) = Max.flux density       0.000  : Standard value
*    AARR(148) =                        0.000  : <- - - - - - - Not used
*    AARR(149) =                        0.000  : <- - - - - - - Not used
*    AARR(150) = Yokeht.incr.TCA        0.000  : Standard value
*    AARR(151) = Dist.wind.-tank        0.000  : Input    value
*    AARR(152) = Extens. of tank        0.000  : Input    value
*    AARR(153) = Dist.core-cover        0.000  : Input    value
*    AARR(154) = Self-cool.ratio       60.000  : Standard value
*    AARR(155) = Max.windtemprise      55.000  : Input    value
*    AARR(156) = Max.TO.temprise       50.000  : Input    value
*    AARR(157) =                        0.000  : <- - - - - - - Not used
*    AARR(158) =                        0.000  : <- - - - - - - Not used
*    AARR(159) =                        0.000  : <- - - - - - - Not used
*    AARR(160) = Yokeht .red. TBA       0.000  : Standard value
*    AARR(161) = Re-opt.index(%)        0.000  : Input    value
*    AARR(162) =                        0.000  : <- - - - - - - Not used
*    AARR(163) =                        0.000  : <- - - - - - - Not used
*    AARR(164) =                        0.000  : <- - - - - - - Not used
*    AARR(165) =                        0.000  : <- - - - - - - Not used
*    AARR(166) =                        0.000  : <- - - - - - - Not used
*    AARR(167) =                        0.000  : <- - - - - - - Not used
*    AARR(168) = Fixed Price (%)       -1.000  : Standard value
*    AARR(169) =                        0.000  : <- - - - - - - Not used
*    AARR(170) = Min.Core diam.         0.000  : Standard value
*    AARR(171) = Max.Core diam.         0.000  : Standard value
*    AARR(172) =                        0.000  : <- - - - - - - Not used
*    AARR(173) = Max.Trnsp.mass       500.000  : Standard value
*    AARR(174) = Max.tank height     4700.000  : Standard value
*    AARR(175) = Max,tank width      3660.000  : Standard value
*    AARR(176) = Max.tank length    13900.000  : Standard value
*    AARR(177) = Max.sound level        0.000  : Standard value
*    AARR(178) =                        0.000  : <- - - - - - - Not used
*    AARR(179) =                        0.000  : <- - - - - - - Not used
*    AARR(180) =                        0.000  : <- - - - - - - Not used
*    AARR(181) =                        0.000  : <- - - - - - - Not used
*    AARR(182) =                        0.000  : <- - - - - - - Not used
*    AARR(183) =                        0.000  : <- - - - - - - Not used
*    AARR(184) =                        0.000  : <- - - - - - - Not used
*    AARR(185) =                        0.000  : <- - - - - - - Not used
*    AARR(186) =                        0.000  : <- - - - - - - Not used
*    AARR(187) =                        0.000  : <- - - - - - - Not used
*    AARR(188) =                        0.000  : <- - - - - - - Not used
*    AARR(189) =                        0.000  : <- - - - - - - Not used
*    AARR(190) =                        0.000  : <- - - - - - - Not used
*    AARR(191) =                        0.000  : <- - - - - - - Not used
*    AARR(192) =                        0.000  : <- - - - - - - Not used
*    AARR(193) =                        0.000  : <- - - - - - - Not used
*    AARR(194) =                        0.000  : <- - - - - - - Not used
*    AARR(195) =                        0.000  : <- - - - - - - Not used
*    AARR(196) =                        0.000  : <- - - - - - - Not used
*    AARR(197) =                        0.000  : <- - - - - - - Not used
*    AARR(198) =                        0.000  : <- - - - - - - Not used
*    AARR(199) =                        0.000  : <- - - - - - - Not used
*    AARR(200) =                        0.000  : <- - - - - - - Not used
*
C* END OF AARR  ***********************************
*
C* START OF TARR  **********************************
*
*    TARR( 01) = Core-steel          NO        : Standard value
*    TARR( 02) = Var.flux regul.     NO        : Standard value
*    TARR( 03) = Highcond.Fld.Scr    NO        : Standard value
*    TARR( 04) = Dshell H.cur.wdg    NO        : Standard value
*    TARR( 05) =                               : <- - - - - - - Not used
*    TARR( 06) = Regulation T1                 : Input    value
*    TARR( 07) = Regulation T2                 : Input    value
*    TARR( 08) = Regulation T3                 : Input    value
*    TARR( 09) =                               : <- - - - - - - Not used
*    TARR( 10) =                               : <- - - - - - - Not used
*    TARR( 11) = Type of regul.T1              : Input    value
*    TARR( 12) = Type of regul.T2              : Input    value
*    TARR( 13) = Type of regul.T3              : Input    value
*    TARR( 14) =                               : <- - - - - - - Not used
*    TARR( 15) =                               : <- - - - - - - Not used
*    TARR( 16) =                               : <- - - - - - - Not used
*    TARR( 17) =                               : <- - - - - - - Not used
*    TARR( 18) =                               : <- - - - - - - Not used
*    TARR( 19) =                               : <- - - - - - - Not used
*    TARR( 20) =                               : <- - - - - - - Not used
*    TARR( 21) = Connection T1                 : Input    value
*    TARR( 22) = Connection T2                 : Input    value
*    TARR( 23) = Connection T3                 : Input    value
*    TARR( 24) = Connection T4                 : Input    value
*    TARR( 25) =                               : <- - - - - - - Not used
*    TARR( 26) =                               : <- - - - - - - Not used
*    TARR( 27) =                               : <- - - - - - - Not used
*    TARR( 28) =                               : <- - - - - - - Not used
*    TARR( 29) =                               : <- - - - - - - Not used
*    TARR( 30) =                               : <- - - - - - - Not used
*    TARR( 31) = Winding type W1               : Input    value
*    TARR( 32) = Winding type W2               : Input    value
*    TARR( 33) = Winding type W3               : Input    value
*    TARR( 34) = Winding type W4               : Input    value
*    TARR( 35) = Winding type W5               : Input    value
*    TARR( 36) = Winding type W6               : Input    value
*    TARR( 37) = Winding type W7               : Input    value
*    TARR( 38) = Winding type W8               : Input    value
*    TARR( 39) = Winding type W9               : Input    value
*    TARR( 40) =                               : <- - - - - - - Not used
*    TARR( 41) = Cond.material W1    CU        : Standard value
*    TARR( 42) = Cond.material W2    CU        : Standard value
*    TARR( 43) = Cond.material W3    CU        : Standard value
*    TARR( 44) = Cond.material W4    CU        : Standard value
*    TARR( 45) = Cond.material W5    CU        : Standard value
*    TARR( 46) = Cond.material W6    CU        : Standard value
*    TARR( 47) = Cond.material W7    CU        : Standard value
*    TARR( 48) = Cond.material W8    CU        : Standard value
*    TARR( 49) = Cond.material W9    CU        : Standard value
*    TARR( 50) =                               : <- - - - - - - Not used
*    TARR( 51) = Side limb (Y/N)               : Input    value
*    TARR( 52) =                               : <- - - - - - - Not used
*    TARR( 53) =                               : <- - - - - - - Not used
*    TARR( 54) =                               : <- - - - - - - Not used
*    TARR( 55) =                               : <- - - - - - - Not used
*    TARR( 56) = Type of cooling               : Input    value
*    TARR( 57) =                               : <- - - - - - - Not used
*    TARR( 58) =                               : <- - - - - - - Not used
*    TARR( 59) =                               : <- - - - - - - Not used
*    TARR( 60) =                               : <- - - - - - - Not used
*    TARR( 61) = Type of cable W1    SP        : Input    value
*    TARR( 62) = Type of cable W2    SP        : Input    value
*    TARR( 63) = Type of cable W3    SP        : Input    value
*    TARR( 64) = Type of cable W4    SP        : Input    value
*    TARR( 65) = Type of cable W5    SP        : Input    value
*    TARR( 66) = Type of cable W6    SP        : Input    value
*    TARR( 67) = Type of cable W7    SP        : Input    value
*    TARR( 68) = Type of cable W8    SP        : Input    value
*    TARR( 69) = Type of cable W9    SP        : Input    value
*    TARR( 70) =                               : <- - - - - - - Not used
*    TARR( 71) =                               : <- - - - - - - Not used
*    TARR( 72) =                               : <- - - - - - - Not used
*    TARR( 73) = Tank mass stated    NO        : Standard value
*    TARR( 74) =                               : <- - - - - - - Not used
*    TARR( 75) = Sound screens       NO        : Standard value
*    TARR( 76) =                               : <- - - - - - - Not used
*    TARR( 77) =                               : <- - - - - - - Not used
*    TARR( 78) = Detailed precalc    NO        : Standard value
*    TARR( 79) =                               : <- - - - - - - Not used
*    TARR( 80) = Core-banding        ASEC      : Standard value
*    TARR( 81) =                               : <- - - - - - - Not used
*    TARR( 82) = TAD cooling calc    NO        : Standard value
*    TARR( 83) =                               : <- - - - - - - Not used
*    TARR( 84) =                               : <- - - - - - - Not used
*    TARR( 85) =                               : <- - - - - - - Not used
*    TARR( 86) =                               : <- - - - - - - Not used
*    TARR( 87) =                               : <- - - - - - - Not used
*    TARR( 88) =                               : <- - - - - - - Not used
*    TARR( 89) =                               : <- - - - - - - Not used
*    TARR( 90) =                               : <- - - - - - - Not used
*    TARR( 91) =                               : <- - - - - - - Not used
*    TARR( 92) =                               : <- - - - - - - Not used
*    TARR( 93) =                               : <- - - - - - - Not used
*    TARR( 94) =                               : <- - - - - - - Not used
*    TARR( 95) = Dshell H.cur.wdg    NO        : Standard value
*    TARR( 96) =                               : <- - - - - - - Not used
*    TARR( 97) =                               : <- - - - - - - Not used
*    TARR( 98) =                               : <- - - - - - - Not used
*    TARR( 99) =                               : <- - - - - - - Not used
*    TARR(100) =                               : <- - - - - - - Not used
*
C* END OF TARR  ************************************
*
C* START OF BARR  **********************************
*
*    BARR( 01) = No.cool.ducts W1      -1.000  : Standard value
*    BARR( 02) = No.cool.ducts W2      -1.000  : Standard value
*    BARR( 03) = No.cool.ducts W3      -1.000  : Standard value
*    BARR( 04) = No.cool.ducts W4      -1.000  : Standard value
*    BARR( 05) = No.cool.ducts W5      -1.000  : Standard value
*    BARR( 06) = No.cool.ducts W6      -1.000  : Standard value
*    BARR( 07) = No.cool.ducts W7      -1.000  : Standard value
*    BARR( 08) = No.cool.ducts W8      -1.000  : Standard value
*    BARR( 09) = No.cool.ducts W9      -1.000  : Standard value
*    BARR( 10) =                        0.000  : <- - - - - - - Not used
*    BARR( 11) = Cond.height W1        -1.000  : Standard value
*    BARR( 12) = Cond.height W2        -1.000  : Standard value
*    BARR( 13) = Cond.height W3        -1.000  : Standard value
*    BARR( 14) = Cond.height W4        -1.000  : Standard value
*    BARR( 15) = Cond.height W5        -1.000  : Standard value
*    BARR( 16) = Cond.height W6        -1.000  : Standard value
*    BARR( 17) = Cond.height W7        -1.000  : Standard value
*    BARR( 18) = Cond.height W8        -1.000  : Standard value
*    BARR( 19) = Cond.height W9        -1.000  : Standard value
*    BARR( 20) =                        0.000  : <- - - - - - - Not used
*    BARR( 21) = Tot.paper cov.W1      -1.000  : Standard value
*    BARR( 22) = Tot.paper cov.W2      -1.000  : Standard value
*    BARR( 23) = Tot.paper cov.W3      -1.000  : Standard value
*    BARR( 24) = Tot.paper cov.W4      -1.000  : Standard value
*    BARR( 25) = Tot.paper cov.W5      -1.000  : Standard value
*    BARR( 26) = Tot.paper cov.W6      -1.000  : Standard value
*    BARR( 27) = Tot.paper cov.W7      -1.000  : Standard value
*    BARR( 28) = Tot.paper cov.W8      -1.000  : Standard value
*    BARR( 29) = Tot.paper cov.W9      -1.000  : Standard value
*    BARR( 30) = Winding support        0.000  : Input    value
*    BARR( 31) = Assembly press         0.000  : Input    value
*    BARR( 32) = EPS  Opt.const         0.000  : Standard value
*    BARR( 33) = FEPS Opt.const        -1.000  : Standard value
*    BARR( 34) = DRV  Opt.const         0.000  : Standard value
*    BARR( 35) = AF   Opt.const         0.000  : Standard value
*    BARR( 36) = P(3) Opt.const         1.900  : Standard value
*    BARR( 37) = P(13)Opt.const         0.000  : Standard value
*    BARR( 38) =                        0.000  : <- - - - - - - Not used
*    BARR( 39) =                        0.000  : <- - - - - - - Reserved
*    BARR( 40) =                        0.000  : <- - - - - - - Reserved
*    BARR( 41) =                        0.000  : <- - - - - - - Not used
*    BARR( 42) =                        0.000  : <- - - - - - - Not used
*    BARR( 43) =                        0.000  : <- - - - - - - Reserved
*    BARR( 44) =                        0.000  : <- - - - - - - Reserved
*    BARR( 45) =                        0.000  : <- - - - - - - Reserved
*    BARR( 46) =                        0.000  : <- - - - - - - Reserved
*    BARR( 47) =                        0.000  : <- - - - - - - Reserved
*    BARR( 48) =                        0.000  : <- - - - - - - Reserved
*    BARR( 49) =                        0.000  : <- - - - - - - Reserved
*    BARR( 50) =                        0.000  : <- - - - - - - Reserved
*    BARR( 51) =                        0.000  : <- - - - - - - Not used
*    BARR( 52) =                        0.000  : <- - - - - - - Not used
*    BARR( 53) =                        0.000  : <- - - - - - - Not used
*    BARR( 54) =                        0.000  : <- - - - - - - Not used
*    BARR( 55) =                        0.000  : <- - - - - - - Not used
*    BARR( 56) =                        0.000  : <- - - - - - - Not used
*    BARR( 57) =                        0.000  : <- - - - - - - Not used
*    BARR( 58) =                        0.000  : <- - - - - - - Not used
*    BARR( 59) =                        0.000  : <- - - - - - - Not used
*    BARR( 60) =                        0.000  : <- - - - - - - Not used
*    BARR( 61) =                        0.000  : <- - - - - - - Not used
*    BARR( 62) =                        0.000  : <- - - - - - - Not used
*    BARR( 63) =                        0.000  : <- - - - - - - Not used
*    BARR( 64) =                        0.000  : <- - - - - - - Not used
*    BARR( 65) =                        0.000  : <- - - - - - - Not used
*    BARR( 66) =                        0.000  : <- - - - - - - Not used
*    BARR( 67) =                        0.000  : <- - - - - - - Not used
*    BARR( 68) =                        0.000  : <- - - - - - - Not used
*    BARR( 69) =                        0.000  : <- - - - - - - Not used
*    BARR( 70) =                        0.000  : <- - - - - - - Not used
*    BARR( 71) =                        0.000  : <- - - - - - - Not used
*    BARR( 72) =                        0.000  : <- - - - - - - Not used
*    BARR( 73) =                        0.000  : <- - - - - - - Not used
*    BARR( 74) =                        0.000  : <- - - - - - - Not used
*    BARR( 75) =                        0.000  : <- - - - - - - Not used
*    BARR( 76) =                        0.000  : <- - - - - - - Not used
*    BARR( 77) =                        0.000  : <- - - - - - - Not used
*    BARR( 78) =                        0.000  : <- - - - - - - Not used
*    BARR( 79) =                        0.000  : <- - - - - - - Not used
*    BARR( 80) =                        0.000  : <- - - - - - - Not used
*    BARR( 81) =                        0.000  : <- - - - - - - Not used
*    BARR( 82) =                        0.000  : <- - - - - - - Not used
*    BARR( 83) =                        0.000  : <- - - - - - - Not used
*    BARR( 84) =                        0.000  : <- - - - - - - Not used
*    BARR( 85) =                        0.000  : <- - - - - - - Not used
*    BARR( 86) =                        0.000  : <- - - - - - - Not used
*    BARR( 87) =                        0.000  : <- - - - - - - Not used
*    BARR( 88) =                        0.000  : <- - - - - - - Not used
*    BARR( 89) =                        0.000  : <- - - - - - - Not used
*    BARR( 90) =                        0.000  : <- - - - - - - Not used
*    BARR( 91) =                        0.000  : <- - - - - - - Not used
*    BARR( 92) =                        0.000  : <- - - - - - - Not used
*    BARR( 93) =                        0.000  : <- - - - - - - Not used
*    BARR( 94) =                        0.000  : <- - - - - - - Not used
*    BARR( 95) =                        0.000  : <- - - - - - - Not used
*    BARR( 96) =                        0.000  : <- - - - - - - Not used
*    BARR( 97) =                        0.000  : <- - - - - - - Not used
*    BARR( 98) =                        0.000  : <- - - - - - - Not used
*    BARR( 99) =                        0.000  : <- - - - - - - Not used
*    BARR(100) =                        0.000  : <- - - - - - - Not used
*
C* END OF BARR  ************************************
*
C* START OF UARR  **********************************
*
*    UARR( 01) = Comb.wdg.prod W1        NO    : Standard value
*    UARR( 02) = Comb.wdg.prod W2        NO    : Standard value
*    UARR( 03) = Comb.wdg.prod W3        NO    : Standard value
*    UARR( 04) = Comb.wdg.prod W4        NO    : Standard value
*    UARR( 05) = Comb.wdg.prod W5        NO    : Standard value
*    UARR( 06) = Comb.wdg.prod W6        NO    : Standard value
*    UARR( 07) = Comb.wdg.prod W7        NO    : Standard value
*    UARR( 08) = Comb.wdg.prod W8        NO    : Standard value
*    UARR( 09) = Comb.wdg.prod W9        NO    : Standard value
*    UARR( 10) =                               : <- - - - - - - Not used
*    UARR( 11) = Transp.cable W1         NO    : Input    value
*    UARR( 12) = Transp.cable W2         NO    : Input    value
*    UARR( 13) = Transp.cable W3         NO    : Input    value
*    UARR( 14) = Transp.cable W4         NO    : Input    value
*    UARR( 15) = Transp.cable W5         NO    : Input    value
*    UARR( 16) = Transp.cable W6         NO    : Input    value
*    UARR( 17) = Transp.cable W7         NO    : Input    value
*    UARR( 18) = Transp.cable W8         NO    : Input    value
*    UARR( 19) = Transp.cable W9         NO    : Input    value
*    UARR( 20) = Optimise tank                 : Input    value
*    UARR( 21) =                               : <------------- Not used
*    UARR( 22) = Core type   (jmf TYPCOR)      : Input value
*    UARR( 23) = Step Lap core (YES/NO)        : Input value
*    UARR( 24) =                               : <- - - - - - - Not used
*    UARR( 25) =                               : <- - - - - - - Not used
*    UARR( 26) =                               : <- - - - - - - Not used
*    UARR( 27) =                               : <- - - - - - - Not used
*    UARR( 28) =                               : <- - - - - - - Not used
*    UARR( 29) =                               : <- - - - - - - Not used
*    UARR( 30) =                               : <- - - - - - - Not used
*    UARR( 31) = Varnish copper W1         NO  : Standard value
*    UARR( 32) = Varnish copper W2         NO  : Standard value
*    UARR( 33) = Varnish copper W3         NO  : Standard value
*    UARR( 34) = Varnish copper W4         NO  : Standard value
*    UARR( 35) = Varnish copper W5         NO  : Standard value
*    UARR( 36) = Varnish copper W6         NO  : Standard value
*    UARR( 37) = Varnish copper W7         NO  : Standard value
*    UARR( 38) = Varnish copper W8         NO  : Standard value
*    UARR( 39) = Varnish copper W9         NO  : Standard value
*    UARR( 40) =                               : <- - - - - - - Not used
*    UARR( 41) = Epoxy glue on TC W1       NO  : Standard value
*    UARR( 42) = Epoxy glue on TC W2       NO  : Standard value
*    UARR( 43) = Epoxy glue on TC W3       NO  : Standard value
*    UARR( 44) = Epoxy glue on TC W4       NO  : Standard value
*    UARR( 45) = Epoxy glue on TC W5       NO  : Standard value
*    UARR( 46) = Epoxy glue on TC W6       NO  : Standard value
*    UARR( 47) = Epoxy glue on TC W7       NO  : Standard value
*    UARR( 48) = Epoxy glue on TC W8       NO  : Standard value
*    UARR( 49) = Epoxy glue on TC W9       NO  : Standard value
*    UARR( 50) =                               : <- - - - - - - Not used
*    UARR( 51) =                               : <- - - - - - - Not used
*    UARR( 52) =                               : <- - - - - - - Not used
*    UARR( 53) =                               : <- - - - - - - Not used
*    UARR( 54) =                               : <- - - - - - - Not used
*    UARR( 55) =                               : <- - - - - - - Not used
*    UARR( 56) =                               : <- - - - - - - Not used
*    UARR( 57) =                               : <- - - - - - - Not used
*    UARR( 58) =                               : <- - - - - - - Not used
*    UARR( 59) =                               : <- - - - - - - Not used
*    UARR( 60) =                               : <- - - - - - - Not used
*    UARR( 61) =                               : <- - - - - - - Not used
*    UARR( 62) =                               : <- - - - - - - Not used
*    UARR( 63) =                               : <- - - - - - - Not used
*    UARR( 64) =                               : <- - - - - - - Not used
*    UARR( 65) =                               : <- - - - - - - Not used
*    UARR( 66) =                               : <- - - - - - - Not used
*    UARR( 67) =                               : <- - - - - - - Not used
*    UARR( 68) =                               : <- - - - - - - Not used
*    UARR( 69) =                               : <- - - - - - - Not used
*    UARR( 70) =                               : <- - - - - - - Not used
*    UARR( 71) =                               : <- - - - - - - Not used
*    UARR( 72) =                               : <- - - - - - - Not used
*    UARR( 73) =                               : <- - - - - - - Not used
*    UARR( 74) =                               : <- - - - - - - Not used
*    UARR( 75) =                               : <- - - - - - - Not used
*    UARR( 76) =                               : <- - - - - - - Not used
*    UARR( 77) =                               : <- - - - - - - Not used
*    UARR( 78) =                               : <- - - - - - - Not used
*    UARR( 79) =                               : <- - - - - - - Not used
*    UARR( 80) =                               : <- - - - - - - Not used
*    UARR( 81) =                               : <- - - - - - - Not used
*    UARR( 82) =                               : <- - - - - - - Not used
*    UARR( 83) =                               : <- - - - - - - Not used
*    UARR( 84) =                               : <- - - - - - - Not used
*    UARR( 85) =                               : <- - - - - - - Not used
*    UARR( 86) =                               : <- - - - - - - Not used
*    UARR( 87) =                               : <- - - - - - - Not used
*    UARR( 88) =                               : <- - - - - - - Not used
*    UARR( 89) =                               : <- - - - - - - Not used
*    UARR( 90) =                               : <- - - - - - - Not used
*    UARR( 91) =                               : <- - - - - - - Not used
*    UARR( 92) =                               : <- - - - - - - Not used
*    UARR( 93) =                               : <- - - - - - - Not used
*    UARR( 94) =                               : <- - - - - - - Not used
*    UARR( 95) =                               : <- - - - - - - Not used
*    UARR( 96) =                               : <- - - - - - - Not used
*    UARR( 97) =                               : <- - - - - - - Not used
*    UARR( 98) =                               : <- - - - - - - Not used
*    UARR( 99) =                               : <- - - - - - - Not used
*    UARR(100) =                               : <- - - - - - - Not used
*
C* END OF UARR  ************************************
*
C* START OF DARR  **********************************
*
*    DARR( 01) = No.guid.rings W1       0.000  : Standard value
*    DARR( 02) = No.guid.rings W2       0.000  : Standard value
*    DARR( 03) = No.guid.rings W3       0.000  : Standard value
*    DARR( 04) = No.guid.rings W4       0.000  : Standard value
*    DARR( 05) = No.guid.rings W5       0.000  : Standard value
*    DARR( 06) = No.guid.rings W6       0.000  : Standard value
*    DARR( 07) = No.guid.rings W7       0.000  : Standard value
*    DARR( 08) = No.guid.rings W8       0.000  : Standard value
*    DARR( 09) = No.guid.rings W9       0.000  : Standard value
*    DARR( 10) = Max.limb height     5000.000  : Standard value
*    DARR( 11) = Tapered clack W1       0.000  : Standard value
*    DARR( 12) = Tapered clack W2       0.000  : Standard value
*    DARR( 13) = Tapered clack W3       0.000  : Standard value
*    DARR( 14) = Tapered clack W4       0.000  : Standard value
*    DARR( 15) = Tapered clack W5       0.000  : Standard value
*    DARR( 16) = Tapered clack W6       0.000  : Standard value
*    DARR( 17) = Tapered clack W7       0.000  : Standard value
*    DARR( 18) = Tapered clack W8       0.000  : Standard value
*    DARR( 19) = Tapered clack W9       0.000  : Standard value
*    DARR( 20) = Limb height            0.000  : Input    value
*    DARR( 21) = Strands/cable W1       0.000  : Standard value
*    DARR( 22) = Strands/cable W2       0.000  : Standard value
*    DARR( 23) = Strands/cable W3       0.000  : Standard value
*    DARR( 24) = Strands/cable W4       0.000  : Standard value
*    DARR( 25) = Strands/cable W5       0.000  : Standard value
*    DARR( 26) = Strands/cable W6       0.000  : Standard value
*    DARR( 27) = Strands/cable W7       0.000  : Standard value
*    DARR( 28) = Strands/cable W8       0.000  : Standard value
*    DARR( 29) = Strands/cable W9       0.000  : Standard value
*    DARR( 30) = Core-varnishing        2.000  : Standard value
*    DARR( 31) = Cover,strand W1        0.000  : Standard value
*    DARR( 32) = Cover,strand W2        0.000  : Standard value
*    DARR( 33) = Cover,strand W3        0.000  : Standard value
*    DARR( 34) = Cover,strand W4        0.000  : Standard value
*    DARR( 35) = Cover,strand W5        0.000  : Standard value
*    DARR( 36) = Cover,strand W6        0.000  : Standard value
*    DARR( 37) = Cover,strand W7        0.000  : Standard value
*    DARR( 38) = Cover,strand W8        0.000  : Standard value
*    DARR( 39) = Cover,strand W9        0.000  : Standard value
*    DARR( 40) = Min.limb height      600.000  : Standard value
*    DARR( 41) = Extra Ax.ins W1        0.000  : Input    value
*    DARR( 42) = Extra Ax.ins W2        0.000  : Input    value
*    DARR( 43) = Extra Ax.ins W3        0.000  : Input    value
*    DARR( 44) = Extra Ax.ins W4        0.000  : Input    value
*    DARR( 45) = Extra Ax.ins W5        0.000  : Input    value
*    DARR( 46) = Extra Ax.ins W6        0.000  : Input    value
*    DARR( 47) = Extra Ax.ins W7        0.000  : Input    value
*    DARR( 48) = Extra Ax.ins W8        0.000  : Input    value
*    DARR( 49) = Extra Ax.ins W9        0.000  : Input    value
*    DARR( 50) =                        0.000  : <- - - - - - - Not used
*    DARR( 51) = Clack height W1        0.000  : Input    value
*    DARR( 52) = Clack height W2        0.000  : Input    value
*    DARR( 53) = Clack height W3        0.000  : Input    value
*    DARR( 54) = Clack height W4        0.000  : Input    value
*    DARR( 55) = Clack height W5        0.000  : Input    value
*    DARR( 56) = Clack height W6        0.000  : Input    value
*    DARR( 57) = Clack height W7        0.000  : Input    value
*    DARR( 58) = Clack height W8        0.000  : Input    value
*    DARR( 59) = Clack height W9        0.000  : Input    value
*    DARR( 60) =                        0.000  : <- - - - - - - Not used
*    DARR( 61) =                        0.000  : <- - - - - - - Not used
*    DARR( 62) =                        0.000  : <- - - - - - - Not used
*    DARR( 63) =                        0.000  : <- - - - - - - Not used
*    DARR( 64) =                        0.000  : <- - - - - - - Not used
*    DARR( 65) =                        0.000  : <- - - - - - - Not used
*    DARR( 66) =                        0.000  : <- - - - - - - Not used
*    DARR( 67) =                        0.000  : <- - - - - - - Not used
*    DARR( 68) =                        0.000  : <- - - - - - - Not used
*    DARR( 69) =                        0.000  : <- - - - - - - Not used
*    DARR( 70) =                        0.000  : <- - - - - - - Not used
*    DARR( 70) =                        0.000  : <- - - - - - - Not used
*    DARR( 71) =                        0.000  : <- - - - - - - Not used
*    DARR( 72) =                        0.000  : <- - - - - - - Not used
*    DARR( 73) =                        0.000  : <- - - - - - - Not used
*    DARR( 74) =                        0.000  : <- - - - - - - Not used
*    DARR( 75) =                        0.000  : <- - - - - - - Not used
*    DARR( 76) =                        0.000  : <- - - - - - - Not used
*    DARR( 77) =                        0.000  : <- - - - - - - Not used
*    DARR( 78) =                        0.000  : <- - - - - - - Not used
*    DARR( 79) =                        0.000  : <- - - - - - - Not used
*    DARR( 80) =                        0.000  : <- - - - - - - Not used
*    DARR( 81) =                        0.000  : <- - - - - - - Not used
*    DARR( 82) =                        0.000  : <- - - - - - - Not used
*    DARR( 83) =                        0.000  : <- - - - - - - Not used
*    DARR( 84) =                        0.000  : <- - - - - - - Not used
*    DARR( 85) =                        0.000  : <- - - - - - - Not used
*    DARR( 86) =                        0.000  : <- - - - - - - Not used
*    DARR( 87) =                        0.000  : <- - - - - - - Not used
*    DARR( 88) =                        0.000  : <- - - - - - - Not used
*    DARR( 89) =                        0.000  : <- - - - - - - Not used
*    DARR( 90) =                        0.000  : <- - - - - - - Not used
*    DARR( 91) =                        0.000  : <- - - - - - - Not used
*    DARR( 92) =                        0.000  : <- - - - - - - Not used
*    DARR( 93) =                        0.000  : <- - - - - - - Not used
*    DARR( 94) =                        0.000  : <- - - - - - - Not used
*    DARR( 95) =                        0.000  : <- - - - - - - Not used
*    DARR( 96) =                        0.000  : <- - - - - - - Not used
*    DARR( 97) =                        0.000  : <- - - - - - - Not used
*    DARR( 98) =                        0.000  : <- - - - - - - Not used
*    DARR( 99) =                        0.000  : <- - - - - - - Not used
*    DARR(100) =                        0.000  : <- - - - - - - Not used
*
C* END OF DARR  ************************************
*
       END
