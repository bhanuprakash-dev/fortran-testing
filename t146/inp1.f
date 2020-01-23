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
*      Subroutine INP1
*      ---------------
*
C...   Title:    Read indata for panel 1 ( P146-1 )
*
*      Written: 83-01-25 by SE Jansson     , ZKB
*      Revised: 85-10-18 by Ron Bell       , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 89-04-21 by Per S�derberg  , SETFO/IK
*      Revised: 91-12-17 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*             Tap changer type and Winding lay out switch have moved in.
************************************************************************
*
       SUBROUTINE INP1(BBDISP,SCR)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'cominp.h'
*
       INTEGER   I,IRC,K,rnv
*
       CHARACTER KEY3*8,SCR*4,DVAR*8,RUNFLG*8
*
       LOGICAL BBDISP
       
cccc       *************************  START OF COMINP   **************************
cc       REAL            USHORE,HLIMB,DCORE,BLIMB,MAXP0,MAXPK,SOUNDM,
cc     &                 RLTANM,BTANKM,HTANKM,GTRPM,HLIMBM,DLMBMA,DLMBMI,
cc     &                 NPHAS,FREQ,EVALP0,EVALPK,DPHAS,DPHSL,DWITA,
cc     &                 DCOCOV,EXTANK,TWSUP,UMAXPU,QMONT,FONAN,TOPDTO,
cc     &                 TTOILM,TWINDM,BLTOPM,HYOKAD,UTURN,HLIMBN,
cc     &                 LTANK,BTANK,HTANK,
cc     &                 CURD0(9),MXCURD(9),ACOND0(9),PRESSM(9),TENSM(9)
cc       COMMON /CDAIN/  USHORE,HLIMB,DCORE,BLIMB,MAXP0,MAXPK,SOUNDM,
cc     &                 RLTANM,BTANKM,HTANKM,GTRPM,HLIMBM,DLMBMA,DLMBMI,
cc     &                 NPHAS,FREQ,EVALP0,EVALPK,DPHAS,DPHSL,DWITA,
cc     &                 DCOCOV,EXTANK,TWSUP,UMAXPU,QMONT,FONAN,TOPDTO,
cc     &                 TTOILM,TWINDM,BLTOPM,HYOKAD,UTURN,HLIMBN,
cc     &                 LTANK,BTANK,HTANK,
cc     &                 CURD0,MXCURD,ACOND0,PRESSM,TENSM
cc*
cc       CHARACTER       OPTTNK*4,CORESE*4,CKTANK*4,CORTYP*4,COREMA*4,
cc     &                 BLDING*4,CVARN*4,KBAND*5,KCOOL*4,COEQIP*4,
cc     &                 TYPREG*4,ETCPL*4,CONSP*4,SNDSCR*4,
cc     &                 ALSHLD*4,T146AC(2)*4,REOPT(4)*3,WNDOPT(9)*4,
cc     &                 OLTC*5,LAYOUT*3,STEPLA*4
cc       COMMON /CDAINC/ OPTTNK,CORESE,CKTANK,CORTYP,COREMA,
cc     &                 BLDING,CVARN,KBAND,KCOOL,COEQIP,
cc     &                 TYPREG,ETCPL,CONSP,SNDSCR,
cc     &                 ALSHLD,T146AC,REOPT,WNDOPT,
cc     &                 OLTC,LAYOUT,STEPLA
c*
cc       REAL            SRATE(4),UNLINE(4),NPSTEP(4),NMSTEP(4),
cc     &                 PUSTEP(4),USURG(4),SLINE(4),XREA(4),
cc     &                 DUYOKE(9),DLYOKE(9),BDUCT(9),NGROUP(9),FILLF(9),
cc     &                 FRACT(9),BWIND(9),NCDUCT(9),XOILGR(9),
cc     &                 XTRNAX(9),XTRNRD(9),XCBLAX(9),
cc    &                 XCBLRD(9),XSTRAX(9),XSTRRD(9),HPART(9),BPART(9),
cc     &                 COVSTR(9),COVCBL(9),CGRAD(4)
cc       COMMON /CDBIN/  SRATE,UNLINE,NPSTEP,NMSTEP,
cc     &                 PUSTEP,USURG,SLINE,XREA,
cc     &                 DUYOKE,DLYOKE,BDUCT,NGROUP,FILLF,
cc     &                 FRACT,BWIND,NCDUCT,XOILGR,
cc     &                 XTRNAX,XTRNRD,XCBLAX,
cc     &                 XCBLRD,XSTRAX,XSTRRD,HPART,BPART,
cc     &                 COVSTR,COVCBL,CGRAD
cc*
cc       CHARACTER       TNODE(4)*4,KCON(4)*4,KTYPRW(4)*4,WNOD1(9)*4,
cc     &                 WNOD2(9)*4,KWITYP(9)*4,KWIFUN(9)*4,COMBWI(9)*4,
cc     &                 CONMAT(9)*4,CBLTYP(9)*4,CONVAR(9)*4,TCEPOX(9)*4
cc      COMMON /CDBINC/ TNODE,KCON,KTYPRW,WNOD1,
cc     &                 WNOD2,KWITYP,KWIFUN,COMBWI,
cc     &                 CONMAT,CBLTYP,CONVAR,TCEPOX
cc*
cc       REAL            ZTRANS,YDELIV,MDELIV,TREVAL,CIMPT,
cc     &                 CPUPT,CPUFN,CPUIND,CPUFIX,EXTRAR(9)
cc       COMMON /CDCIN/  ZTRANS,YDELIV,MDELIV,TREVAL,CIMPT,
cc     &                 CPUPT,CPUFN,CPUIND,CPUFIX,EXTRAR
cc*
cc       CHARACTER       MNLCOD*8,OPERAT*4,DETPRE*4,IDENT*40,PKLCOD*4,
cc     &                 OPTLYR(9)*3,OPTTRN(9)*3,OPTCLD(9)*3,CAL009*4
cc       COMMON /CDCINC/ MNLCOD,OPERAT,DETPRE,IDENT,PKLCOD,
cc     &                 OPTLYR,OPTTRN,OPTCLD,CAL009
cc*
cc       REAL            RWILI,CALCT,OFFIN(9),OFFOUT(9),DSPOOL(9),
cc     &                 NLAYER(9),NTURNS(9),HCLAC(9)
cc       COMMON /CCDNEW/ RWILI,CALCT,OFFIN,OFFOUT,DSPOOL,
cc     &                 NLAYER,NTURNS,HCLAC
cc*
cc       REAL            BOOSMA, BOOSVO, BOOSRE, BOOSTR, TEMPLO, STACKF, SPFADU
cc       LOGICAL         BBBOOS
cc       COMMON/BOOS/    BOOSMA, BOOSVO, BOOSRE, BOOSTR, BBBOOS
cc       COMMON/RTE/     TEMPLO, STACKF, SPFADU
cc
cc      COMMON /TA1COC/ FLPLMA
cc       CHARACTER       FLPLMA*8
cc       COMMON /TA1COR/ LAMTH, LAMSPF, LAMMW, LAMOVL, ACCLON, ACCTRA,
cc     &                 ACCVER, AMBTEM, LAMSTP, LOSCOR, TA1HOL
cc       REAL            LAMTH, LAMSPF, LAMMW, LAMOVL, ACCLON, ACCTRA,
cc     &                 ACCVER, AMBTEM, LAMSTP, LOSCOR, TA1HOL
cc       COMMON /TA1COI/ NCOOLI
cc       INTEGER         NCOOLI
cc*************************  END   OF COMINP   **************************
*
C... Initialisations
*
       IDENT=' '
       TYPREG=' '
       MNLCOD=' '
       KCOOL=' '
       OPERAT=' '
       DETPRE=' '
****       CAL009=' '
       KEY3='       1'
*
C... Initialise terminal input data.
*
       DO 10 ITML=1,4
       TNODE (ITML)=' '
       KCON  (ITML)=' '
   10  KTYPRW(ITML)=' '
*
C... READ INDATA FROM THE input file FOR PANEL 1 P146-1
 
       open(2,file='DATA_9220_10_inp1',status='old',action='read')

       read(2,*)RWILI
       
       read(2,*)IDENT

       read(2,*)(TNODE(i),i=1,4)
   
       read(2,*)(SRATE(i),i=1,4)
  
       read(2,*)CALCT
   
c       write(*,*)'No of Windings=',RWILI
c       write(*,*)'IDENT=',IDENT
c       write(*,*)'TNODE(1),TNODE(2),TNODE(3),TNODE(4) ',(TNODE(i),i=1,4)
c       write(*,*)'SRATE(1),SRATE(2),SRATE(3),SRATE(4) ',(SRATE(i),i=1,4)
c       write(*,*)'Loss calculation at step=',CALCT

*
C... Initialisation before adjusting the decimals
*
cc       K=1
*
C... Adjust the decimals in a Power rating if it exists
*
C,,, The Power rating exists
*
cc  200  IF (SRATE(K).GT.1.E-4.AND.K.LE.4) THEN
*
C... Adjust the decimals
*
cc          I=1
cc          IF(SRATE(K).LT.100.) I=2
cc          IF(SRATE(K).LT.10.)  I=3
cc          WRITE(DVAR,1000) 10*K+2
*
C... Re-read the value from the data-base with the correct No. decimals

*
cc          CALL GETRD
cc     &           ('TERM    ', 'SRATED  ', KEY3, SRATE(K), DVAR ,1 ,6,I)
*
C... Check if there are more values to adjust
*
cc          K=K+1
cc          WRITE(KEY3,1010) K
cc          GOTO 200
cc       END IF
*
C... Initialisation before adjusting the decimals
*
cc       KEY3='       1'
*
C... Read the Voltage ratings from the data-base

        read(2,*)(UNLINE(i),i=1,4)
c        write(*,*)'RATV(1),RATV(2),RATV(3),RATV(4)',(UNLINE(i),i=1,4)
*
*
C... Initialisation before adjusting the decimals
*
cc       K=1
*
C... Adjust the decimals in a Voltage rating if it exists
*
C,,, The Voltage rating exists
*
cc  300  IF (UNLINE(K).GT.1.E-4.AND.K.LE.4) THEN
*
C... Adjust the decimals
*
cc          I=1
cc          IF (UNLINE(K).LT.1000.) I=2
cc          IF (UNLINE(K).LT.100.) I=3
cc          IF (UNLINE(K).LT.10.)  I=4
cc          WRITE(DVAR,1000) K*10+3
*
C... Re-read the value from the data-base with the correct No. decimals
*
cc          CALL GETRD
cc     &         ('TERM    ', 'URATED  ', KEY3, UNLINE(K), DVAR,1 ,6,I)
*
C... Check if there are more values to adjust
*
cc          K=K+1
cc          WRITE(KEY3,1010) K
cc          GOTO 300
cc       END IF
*
C... Read CHARACTER indata from the data base for panel 1 P146-1

        read(2,*)(KCON(i),i=1,4)
c        write(*,*)'KCON(1),KCON(2),KCON(3),KCON(4) ',(KCON(i),i=1,4)
        read(2,*)(KTYPRW(i),i=1,4)
c        write(*,*)'RegT(1),RegT(2),RegT(3),RegT(4) ',(KTYPRW(i),i=1,4)

        read(2,*)(NPSTEP(i),i=1,4)
c        write(*,*)'PSTP(1),PSTP(2),PSTP(3),PSTP(4)',(NPSTEP(i),i=1,4)
       
        read(2,*)(NMSTEP(i),i=1,4)
c        write(*,*)'NSTP(1),NSTP(2),NSTP(3),NSTP(4)',(NMSTEP(i),i=1,4)
       
        read(2,*)(PUSTEP(i),i=1,4)
c        write(*,*)'STPS(1),STPS(2),STPS(3),STPS(4)',(PUSTEP(i),i=1,4)
       
        read(2,*)(USURG(i),i=1,4)
c        write(*,*)'USURG(1),USURG(2),USURG(3),USURG(4)',
c     &             (USURG(i),i=1,4)
       
        read(2,*)(SLINE(i),i=1,4)
c        write(*,*)'SLINE(1),SLINE(2),SLINE(3),SLINE(4)',
c     &                      (SLINE(i),i=1,4)
     
        read(2,*)(CGRAD(i),i=1,4)
c        write(*,*)'CGRAD(1),CGRAD(2),CGRAD(3),CGRAD(4)',
c     &                      (CGRAD(i),i=1,4)
     
        read(2,*)FREQ
c        write(*,*)'FREQUENCY=',FREQ
       
        read(2,*)NPHAS
c        write(*,*)'No. of Phases=',NPHAS
       
        read(2,*)TYPREG
c        write(*,*)'TYPREG=',TYPREG
       
       read(2,*)EVALP0
c       write(*,*)'No load loss=',EVALP0
       
       read(2,*)EVALPK
c       write(*,*)'Load Loss=',EVALPK
       
       read(2,*)MNLCOD
c       write(*,*)'MNLCOD=',MNLCOD
       
       read(2,*)CPUIND
c       write(*,*)'CPUIND=',CPUIND
       
       read(2,*)CPUFIX
c       write(*,*)'CPUFIX=',CPUFIX
       
       read(2,*)KCOOL
c      write(*,*)'KCOOL=',KCOOL
       
       read(2,*)FONAN
c       write(*,*)'FONAN=',FONAN
       
       read(2,*)OPERAT
c       write(*,*)'OPERAT=',OPERAT
       
       read(2,*)DETPRE
c       write(*,*)'DETPRE=',DETPRE
       
       read(2,*)(XREA(i),i=1,3)
c       write(*,*)'XREA(1),XREA(2),XREA(3)',
c     &                      (XREA(i),i=1,3)
       
	   close(2)
*
cc       KEY3='       1'
cc*****       CALL GETCD('T146    ', 'CALC009 ', KEY3, CAL009,'B48 ',1 ,3)
*
C... Display the panel ( if required ) and check the indata
*
cc       if(.not.bbdisp)irc=isplnk('control','nondispl')
cc       irc=isplnk('display','p146-1 ')
*
C... Transfer SCROLL from dialog to CHARACTER variable
*
cc       CALL DMOVC(SCR,'SCROLL ',1,4)
*
cc       KEY3='       1'
cc       CALL PUTRD('WINDING ', 'NWIND   ', KEY3, RWILI,'WILI ',1 ,1,0)
cc       CALL PUTCD('HEADING ', 'IDENT   ', KEY3, IDENT,'ID ',1 ,40)
cc       CALL PUTCD('TERM    ', 'TERMNODE', KEY3, TNODE,'A* ',4 ,3)
cc       CALL PUTRD('TERM    ', 'SRATED  ', KEY3, SRATE,'A*2 ',4 ,6,1)
cc       CALL PUTRD('TERM    ', 'CALCTAP ', KEY3, CALCT,'A110 ',1 ,4,0)
*
C... Initialisation before adjusting the decimals
*
cc       K=1
*
C... Adjust the decimals in a Power rating if it exists
*
C... The Power rating exists
*
cc  400  IF (SRATE(K).GT.1.E-4.AND.K.LE.4) THEN
*
C... Adjust the decimals
*
cc          I=1
cc          IF(SRATE(K).LT.100.) I=2
cc          IF(SRATE(K).LT.10.)  I=3
cc          WRITE(DVAR,1000) 10*K+2
*
C... Re-store the value onto the data-base with the correct No. decimals
*
cc          CALL PUTRD
cc     &           ('TERM    ', 'SRATED  ', KEY3, SRATE(K), DVAR ,1 ,6,I)
*
C... Check if there are more values to adjust
*
cc          K=K+1
cc          WRITE(KEY3,1010) K
cc          GOTO 400
cc       END IF
*
C... Initialisation before adjusting the decimals
*
cc       KEY3='       1'
*
C... Store the Voltage ratings onto the data-base
*
cc       CALL PUTRD('TERM    ', 'URATED  ', KEY3, UNLINE,'A*3 ',4 ,7,1)
cc       K=1
*
C... Adjust the decimals in a Voltage rating
*
C... If the Voltage rating exists
*
cc  500  IF (UNLINE(K).GT.1.E-4.AND.K.LE.4) THEN
*
C... Adjust the decimals
*
cc       I=1
cc       IF (UNLINE(K).LT.1000.) I=2
cc       IF(UNLINE(K).LT.100.) I=3
cc       IF(UNLINE(K).LT.10.)  I=4
cc       WRITE(DVAR,1000) K*10+3
*
C... Re-store the value onto the data-base with the correct No. decimals
*
cc          CALL PUTRD
cc     &           ('TERM    ', 'URATED  ', KEY3, UNLINE(K), DVAR,1 ,6,I)
*
C... Check if there are more values to adjust
*
cc          K=K+1
cc          WRITE(KEY3,1010) K
cc          GOTO 500
cc       END IF
*
cc       KEY3='       1'
*
cc       CALL PUTCD('TERM    ', 'CONNEC  ', KEY3, KCON,  'A*4 ',4 ,3)
cc       CALL PUTCD('TERM    ', 'REGTYPE ', KEY3, KTYPRW,'A*5 ',4 ,3)
cc       CALL PUTRD('TERM    ', 'NREGPOS ', KEY3, NPSTEP,'A*6 ',4 ,3,0)
cc       CALL PUTRD('TERM    ', 'NREGNEG ', KEY3, NMSTEP,'A*7 ',4 ,3,0)
cc       CALL PUTRD('TERM    ', 'NREGPRO ', KEY3, PUSTEP,'A*8 ',4 ,5,3)
cc       CALL PUTRD('TERM    ', 'BILTERM ', KEY3, USURG, 'A*9 ',4 ,5,0)
cc       CALL PUTRD('TERM    ', 'SCPTERM ', KEY3, SLINE, 'A*0 ',4 ,5,1)
cc       CALL PUTRD('TERM    ', 'GRADINS ', KEY3, CGRAD, 'A*1 ',4 ,5,1)
cc       CALL PUTRD('TRANSF  ', 'FREQ    ', KEY3, FREQ,  'B11 ',1 ,5,1)
cc       CALL PUTRD('TRANSF  ', 'NPHASES ', KEY3, NPHAS, 'B12 ',1 ,3,0)
cc       CALL PUTCD('TRANSF  ', 'TYPEREG ', KEY3, TYPREG,'B13 ',1 ,3)
cc       CALL PUTRD('TRANSF  ', 'EVALP0  ', KEY3, EVALP0,'B14 ',1 ,6,0)
cc       CALL PUTRD('TRANSF  ', 'EVALPK  ', KEY3, EVALPK,'B15 ',1 ,6,0)
cc       CALL PUTCD('TRANSF  ', 'COSTCODE', KEY3, MNLCOD,'B18 ',1 ,6)
cc       CALL PUTRD('TRANSF  ', 'INDEX   ', KEY3, CPUIND,'B22 ',1 ,4,2)
cc       CALL PUTRD('TRANSF  ', 'FIXPRIC ', KEY3, CPUFIX,'B23 ',1 ,3,1)
cc       CALL PUTCD('COOLING ', 'COOLTYPE', KEY3, KCOOL, 'B31 ',1 ,4)
cc       CALL PUTRD('COOLING ', 'SELFCRAT', KEY3, FONAN, 'B32 ',1 ,4,2)
cc       CALL PUTCD('T146    ', 'OPERCHAR', KEY3, OPERAT,'B34 ',1 ,3)
cc       CALL PUTCD('T146    ', 'DETPREC ', KEY3, DETPRE,'B35 ',1 ,3)
cc*****       CALL PUTCD('T146    ', 'CALC009 ', KEY3, CAL009,'B48 ',1 ,3)
cc       CALL PUTRD('TRANSF  ', 'REACCLIM', KEY3, XREA,  'B4* ',3 ,5,1)
*
C... Define RUNFLG to show that data has been altered
*
cc       RUNFLG='T146ALT '
*
C... Store RUNFLG onto the data base
*
cc       CALL PUTC('T146    ','RUNFLAG ',KEY3, RUNFLG, 1,8)
*
C... Return
*
       RETURN
*
 1000  FORMAT('A',I2)
 1010  FORMAT('       ',I1)
*
       END
