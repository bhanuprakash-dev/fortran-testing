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
*      Subroutine INP2
*      ---------------
*
C...   Title:    Read indata for panel 2 ( P146-2 )
*
*      Written: 83-01-25 by SE Jansson     , ZKB
*      Revised: 85-10-18 by Ron Bell       , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell       , TRAFO/IK
*      Revised: 90-01-08 by Per S�derberg  , SETFO/IK
*      Revised: 90-08-16 by Per S�derberg  , SETFO/IK
*      Revised: 91-06-10 by Per S�derberg  , SETFO/TS
*      Revised: 91-09-17 by B-G Bladh      , SETFO/TS
*              Check for booster is introduced for KWIFUN (RB, RCB)
*      Revised: 91-12-17 by B-G Bladh      , SETFO/TS
*              Complete new panel design
************************************************************************
*
       SUBROUTINE INP2(BBDISP,SCR)
*
C... Declarations
*
       include'cominp.h'
*
       INTEGER IRC,IWDG,NW,KRWDG,TRWDG1,TRWDG2
*
       LOGICAL BBDISP,MVWAR
*
       DIMENSION TEST(9),TEST1(9)
       CHARACTER KEY3*8,SCR*4,TEST*3,TEST1*3
       CHARACTER*60 MSG,HLPMSG,HLPMS1
*
       DATA HLPMSG /
     & ' Warning: R winding in main field, please check losses'
     & /
       DATA HLPMS1 /
     & 'Warn: Tap in MV+parallel strands => Tap outer cross-overs.'
     & /
*
C... End of the declarations block.
*
C... Initialisations
       DO 10 IWDG=1,9
          WNOD1 (IWDG)=' '
          WNOD2 (IWDG)=' '
          KWITYP(IWDG)=' '
          KWIFUN (IWDG)=' '
   10     CONMAT(IWDG)=' '
*
       NW=NINT(RWILI)
       KEY3='       1'
*
*
C... Initialise REOPT(*)
*
       DO 11 I= 1,4
   11     REOPT(I) = '   '
*
C... Read indata from the data base for panel 2 P146-2
*

       open(2,file='DATA_9220_10_inp2',status='old',action='read')

       read(2,*)(WNOD1(i),i=1,9)
c       write(*,*)
c     & 'WTO(1),WTO(2),WTO(3),WTO(4),WTO(5),WTO(6),WTO(7),WTO(8),WTO(9)'
c     &   ,(WNOD1(i),i=1,9)

       read(2,*)(WNOD2(i),i=1,9)
c       write(*,*)
c     & 'WBM(1),WBM(2),WBM(3),WBM(4),WBM(5),WBM(6),WBM(7),WBM(8),WBM(9)'
c     &   ,(WNOD2(i),i=1,9)

       read(2,*)(KWIFUN(i),i=1,9)
c       write(*,*)
c     & 'FCD(1),FCD(2),FCD(3),FCD(4),FCD(5),FCD(6),FCD(7),FCD(8),FCD(9)'
c     &   ,(KWIFUN(i),i=1,9)

        read(2,*)(DUYOKE(i),i=1,9)
c        write(*,*)
c     & 'YTO(1),YTO(2),YTO(3),YTO(4),YTO(5),YTO(6),YTO(7),YTO(8),YTO(9)'
c     &,(DUYOKE(i),i=1,9)

        read(2,*)(DLYOKE(i),i=1,9)
c        write(*,*)
c     & 'YBM(1),YBM(2),YBM(3),YBM(4),YBM(5),YBM(6),YBM(7),YBM(8),YBM(9)'
c     &,(DLYOKE(i),i=1,9)

        read(2,*)(BDUCT(i),i=1,9)
c        write(*,*)
c     & 'DWT(1),DWT(2),DWT(3),DWT(4),DWT(5),DWT(6),DWT(7),DWT(8),DWT(9)'
c     &,(BDUCT(i),i=1,9)

        read(2,*)(FRACT(i),i=1,9)
c        write(*,*)
c     & 'WFR(1),WFR(2),WFR(3),WFR(4),WFR(5),WFR(6),WFR(7),WFR(8),WFR(9)'
c     &,(FRACT(i),i=1,9)

       read(2,*)(CBLTYP(i),i=1,9)
c       write(*,*)
c     & 'CTY(1),CTY(2),CTY(3),CTY(4),CTY(5),CTY(6),CTY(7),CTY(8),CTY(9)'
c     &   ,(CBLTYP(i),i=1,9)

        read(2,*)(BPART(i),i=1,9)
c        write(*,*)
c     & 'BPT(1),BPT(2),BPT(3),BPT(4),BPT(5),BPT(6),BPT(7),BPT(8),BPT(9)'
c     &,(BPART(i),i=1,9)

        read(2,*)(MXCURD(i),i=1,9)
c        write(*,*)
c     & 'MCD(1),MCD(2),MCD(3),MCD(4),MCD(5),MCD(6),MCD(7),MCD(8),MCD(9)'
c     &,(MXCURD(i),i=1,9)

        read(2,*)(TENSM(i),i=1,9)
c        write(*,*)
c     & 'MTE(1),MTE(2),MTE(3),MTE(4),MTE(5),MTE(6),MTE(7),MTE(8),MTE(9)'
c     &,(TENSM(i),i=1,9)

        read(2,*)(PRESSM(i),i=1,9)
c        write(*,*)
c     & 'MPR(1),MPR(2),MPR(3),MPR(4),MPR(5),MPR(6),MPR(7),MPR(8),MPR(9)'
c     &,(PRESSM(i),i=1,9)

       read(2,*)(WNDOPT(i),i=1,9)
c       write(*,*)
c     & 'WOP(1),WOP(2),WOP(3),WOP(4),WOP(5),WOP(6),WOP(7),WOP(8),WOP(9)'
c     &   ,(WNDOPT(i),i=1,9)

        read(2,*)(FILLF(i),i=1,9)
c        write(*,*)
c     & 'SPF(1),SPF(2),SPF(3),SPF(4),SPF(5),SPF(6),SPF(7),SPF(8),SPF(9)'
c     &,(FILLF(i),i=1,9)

        read(2,*)(BWIND(i),i=1,9)
c        write(*,*)
c     & 'BWD(1),BWD(2),BWD(3),BWD(4),BWD(5),BWD(6),BWD(7),BWD(8),BWD(9)'
c     &,(BWIND(i),i=1,9)

        read(2,*)(ACOND0(i),i=1,9)
c        write(*,*)
c     & 'ACD(1),ACD(2),ACD(3),ACD(4),ACD(5),ACD(6),ACD(7),ACD(8),ACD(9)'
c     &,(ACOND0(i),i=1,9)

       read(2,*)HLIMB
c       write(*,*)'HLIMB=',HLIMB
        write(*,*)'inp2 line 162', HLIMB

       read(2,*)DCORE
c       write(*,*)'DCORE=',DCORE

       read(2,*)BLIMB
c       write(*,*)'BLIMB=',BLIMB

       read(2,*)USHORE
c       write(*,*)'USHORE=',USHORE

       read(2,*)LAYOUT
c       write(*,*)'LAYOUT=',LAYOUT

       read(2,*)(REOPT(i),i=1,4)
c       write(*,*)'ROPT(1),ROPT(2),ROPT(3),ROPT(4)= ',(REOPT(i),i=1,4)

       close(2)

*
C... Display the panel ( if required ) and check the indata
*
cc       if(.not.bbdisp) irc=isplnk('control','nondispl')
cc       irc=isplnk('display','p146-2 ')
*
C... Change 90-08-16 additional warning if Reg. winding in mainfield.
*
cc       CALL DMOVC(TEST,'C*4 ',NW,3)
cc       TRWDG1=0
cc       TRWDG2=0
cc       KRWDG=1
cc       MSG=' '
cc       DO 100 IWDG=1,NW
cc          IF (TEST(IWDG).EQ.'R') THEN
cc             IF (KRWDG.EQ.1) THEN
cc                TRWDG1=IWDG
cc                KRWDG=2
cc             ELSE
cc                TRWDG2=IWDG
cc             ENDIF
cc          ENDIF
cc  100  CONTINUE
cc       IF (KRWDG.EQ.1) THEN
cc          IF (TRWDG1.GE.2.AND.TRWDG1.LT.NW) THEN
cc             MSG=HLPMSG
cc             CALL CMOVD(MSG,'MSG ',1,60)
cc              if(.not.bbdisp) irc=isplnk('control','nondispl')
cc              irc=isplnk('display','p146-2 ')
cc          ENDIF
cc       ELSE
cc          IF (TRWDG1.GE.2.AND.TRWDG2.LT.NW.AND.TRWDG1.LT.TRWDG2) THEN
cc             MSG=HLPMSG
cc             CALL CMOVD(MSG,'MSG ',1,60)
cc             if(.not.bbdisp) irc=isplnk('control','nondispl')
cc             irc=isplnk('display','p146-2 ')
cc          ENDIF
cc       ENDIF
*
C... Change 91-06-10 Additional warning for tapping in main Winding.
*
cc       CALL DMOVC(TEST1,'C*4 ',NW,3)
cc       MVWAR=.FALSE.
cc       MSG=' '
cc       DO 200 IWDG=1,NW
cc          IF (TEST1(IWDG).EQ.'MV  ') THEN
cc             MVWAR=.TRUE.
cc          ENDIF
cc 200   CONTINUE
cc       IF (MVWAR) THEN
cc          MSG=HLPMS1
cc          CALL CMOVD(MSG,'MSG ',1,60)
cc          if (.not.bbdisp) irc=isplnk('control','nondispl')
CCC       irc=isplnk('display','p146-2 ')
cc       ENDIF
cc       MSG=' '
cc       CALL CMOVD(MSG,'MSG ',1,60)
*
C... Transfer SCROLL from dialog to CHARACTER variable
*
cc      CALL DMOVC(SCR,'SCROLL ',1,4)
C... Write the indata on the data base
c*
cc       KEY3='       1'
cc       CALL PUTCD('WINDING ', 'WINDNOD1', KEY3, WNOD1, 'C*1 ',NW, 3)
cc       CALL PUTCD('WINDING ', 'WINDNOD2', KEY3, WNOD2, 'C*2 ',NW, 3)
cc       CALL PUTCD('WINDING ', 'FUNCCODE', KEY3, KWIFUN,'C*4 ',NW, 3)
c*
C...   Check for BOOSTER (function code RB  or RCB instead of R - RC)
c*
cc       BBBOOS = .FALSE.
cc       DO 300 IWDG =  1, NW
cc           IF(KWIFUN(IWDG).EQ. 'RB ') BBBOOS = .TRUE.
cc           IF(KWIFUN(IWDG).EQ. 'RCB') BBBOOS = .TRUE.
cc 300   CONTINUE
c*
cc       CALL PUTRD('WINDING ', 'TYOKETOP', KEY3, DUYOKE,'C*5 ',NW, 5,0)
cc       CALL PUTRD('WINDING ', 'TYOKEBOT', KEY3, DLYOKE,'C*6 ',NW, 5,0)
cc       CALL PUTRD('WINDING ', 'BDUCT   ', KEY3, BDUCT, 'C*7 ',NW, 5,0)
cc       CALL PUTRD('WINDING ', 'WINDFRAC', KEY3, FRACT, 'C1*0 ',NW, 5,3)
cc       CALL PUTCD('WINDING ', 'CBLTYP  ', KEY3, CBLTYP,'C1*1 ',NW,4)
cc       CALL PUTRD('WINDING ', 'BSTRAND ', KEY3, BPART, 'E*2 ',NW, 5,2)
cc       CALL PUTRD('WINDING ', 'MAXCURD ', KEY3, MXCURD,'E*7 ',NW, 5,2)
cc       CALL PUTRD('WINDING ', 'MAXSIGWP', KEY3, TENSM, 'E*8 ',NW, 4,0)
cc       CALL PUTRD('WINDING ', 'MAXSIGWN', KEY3, PRESSM,'E*9 ',NW, 5,1)
cc       CALL PUTCD('WINDING ', 'REOPTW  ', KEY3, WNDOPT,'D*1 ',NW, 3)
cc       CALL PUTRD('WINDING ', 'SPACEFAC', KEY3, FILLF, 'D*2 ',NW, 6,4)
cc       CALL PUTRD('WINDING ', 'BWIND   ', KEY3, BWIND, 'D*3 ',NW, 6,1)
cc       CALL PUTRD('WINDING ', 'ACOND   ', KEY3, ACOND0,'D*4 ',NW, 8,2)
cc       CALL PUTRD('CORE    ', 'HLIMB   ', KEY3, HLIMB, 'D5 ',1, 5,0)
cc       CALL PUTRD('CORE    ', 'DLIMB   ', KEY3, DCORE, 'D6 ',1, 5,0)
cc       CALL PUTRD('CORE    ', 'BLIMB   ', KEY3, BLIMB, 'D7 ',1, 6,4)
cc       CALL PUTRD('CORE    ', 'REACHX0 ', KEY3, USHORE,'D8 ',1, 6,2)
cc       CALL PUTCD('T146    ', 'REOPT   ', KEY3, REOPT, 'D9* ',4, 3)
cc       CALL PUTCD('WINDING ', 'LAYOUT  ', KEY3, LAYOUT,'D111 ',1, 3)

*
       RETURN
       END
