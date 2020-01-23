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
*      Subroutine INP3
*      ---------------
*
C...   Title:    Read indata for panel 3 ( P146-3 )
*
*      Written: 83-01-25 by SE Jansson , ZKB
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-12-17 by B-G Bladh  , SETFO/TS
*               Complete new panel lay out and new content.
************************************************************************
*
       SUBROUTINE INP3(BBDISP,SCR)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'cominp.h'
*
       INTEGER I,IRC,NW
*
       LOGICAL BBDISP
*
       CHARACTER KEY3*8, SCR*4
C... Initialisations
*
       DO 10 IWDG=1,9
         CONMAT(IWDG)=' '
 10    CONTINUE
*
       NW=RWILI
       KEY3='       1'
*
C... Read indata from the data base for panel 3 P146-3

       open(2,file='DATA_9220_10_inp3',status='old',action='read')

       read(2,*)(KWITYP(i),i=1,9)
c       write(*,*)
c     & 'KTP(1),KTP(2),KTP(3),KTP(4),KTP(5),KTP(6),KTP(7),KTP(8),KTP(9)'
c     &   ,(KWITYP(i),i=1,9)

        read(2,*)(NGROUP(i),i=1,9)
c        write(*,*)
c     & 'NGP(1),NGP(2),NGP(3),NGP(4),NGP(5),NGP(6),NGP(7),NGP(8),NGP(9)'
c     &,(NGROUP(i),i=1,9)

        read(2,*)(CONMAT(i),i=1,9)
c        write(*,*)
c     & 'CMT(1),CMT(2),CMT(3),CMT(4),CMT(5),CMT(6),CMT(7),CMT(8),CMT(9)'
c     &,(CONMAT(i),i=1,9)

        read(2,*)(HCLAC(i),i=1,9)
c        write(*,*)
c     & 'HCL(1),HCL(2),HCL(3),HCL(4),HCL(5),HCL(6),HCL(7),HCL(8),HCL(9)'
c     &,(HCLAC(i),i=1,9)


        read(2,*)(COVSTR(i),i=1,9)
c        write(*,*)
c     & 'CVS(1),CVS(2),CVS(3),CVS(4),CVS(5),CVS(6),CVS(7),CVS(8),CVS(9)'
c     &,(COVSTR(i),i=1,9)


        read(2,*)(COVCBL(i),i=1,9)
c        write(*,*)
c     & 'CVC(1),CVC(2),CVC(3),CVC(4),CVC(5),CVC(6),CVC(7),CVC(8),CVC(9)'
c     &,(COVCBL(i),i=1,9)

        read(2,*)(CONVAR(i),i=1,9)
c        write(*,*)
c     & 'COR(1),COR(2),COR(3),COR(4),COR(5),COR(6),COR(7),COR(8),COR(9)'
c     &,(CONVAR(i),i=1,9)

        read(2,*)(TCEPOX(i),i=1,9)
c        write(*,*)
c     & 'TPX(1),TPX(2),TPX(3),TPX(4),TPX(5),TPX(6),TPX(7),TPX(8),TPX(9)'
c     &,(TCEPOX(i),i=1,9)

        read(2,*)(EXTRAR(i),i=1,9)
c        write(*,*)
c     & 'EXR(1),EXR(2),EXR(3),EXR(4),EXR(5),EXR(6),EXR(7),EXR(8),EXR(9)'
c     &,(EXTRAR(i),i=1,9)

        read(2,*)(HPART(i),i=1,9)
c        write(*,*)
c     & 'HPT(1),HPT(2),HPT(3),HPT(4),HPT(5),HPT(6),HPT(7),HPT(8),HPT(9)'
c     &,(HPART(i),i=1,9)

        read(2,*)(NCDUCT(i),i=1,9)
c        write(*,*)
c     & 'NDT(1),NDT(2),NDT(3),NDT(4),NDT(5),NDT(6),NDT(7),NDT(8),NDT(9)'
c     &,(NCDUCT(i),i=1,9)


        read(2,*)(XOILGR(i),i=1,9)
c        write(*,*)
c     & 'XOG(1),XOG(2),XOG(3),XOG(4),XOG(5),XOG(6),XOG(7),XOG(8),XOG(9)'
c     &,(XOILGR(i),i=1,9)

        read(2,*)(COMBWI(i),i=1,9)
c        write(*,*)
c     & 'CBI(1),CBI(2),CBI(3),CBI(4),CBI(5),CBI(6),CBI(7),CBI(8),CBI(9)'
c     &,(COMBWI(i),i=1,9)

       close(2)
*
cc       CALL GETCD('WINDING ', 'WINDTYPE', KEY3, KWITYP,'C*3 ',NW, 4)
cc       CALL GETRD('WINDING ', 'NGROUPS ', KEY3, NGROUP,'C*8 ',NW, 4,0)
cc       CALL GETCD('WINDING ', 'CONDMAT ', KEY3, CONMAT,'C*9 ',NW, 3)
cc       CALL GETCD('WINDING ', 'CONDVARN', KEY3, CONVAR,'C1*2 ',NW, 3)
cc       CALL GETCD('WINDING ', 'TCEPOXY ', KEY3, TCEPOX,'C1*3 ',NW,3)
cc*
cc       CALL GETRD('WINDING ', 'EXTINSAR', KEY3, EXTRAR,'E*10 ',NW, 5,0)
cc       CALL GETRD('WINDING ', 'HCLACK  ', KEY3, HCLAC ,'E*11 ',NW, 4,1)
cc*
cc       CALL GETRD('WINDING ', 'HSTRAND ', KEY3, HPART, 'E*1 ',NW, 5,2)
cc*
cc       CALL GETRD('WINDING ', 'COVSTR  ', KEY3, COVSTR,'E*3 ',NW, 5,2)
cc       CALL GETRD('WINDING ', 'COVCBL  ', KEY3, COVCBL,'E*4 ',NW, 5,2)
cc*
cc       CALL GETRD('WINDING ', 'NCOOLD  ', KEY3, NCDUCT,'E*5 ',NW, 3,0)
cc       CALL GETRD('WINDING ', 'NOILGRGC', KEY3, XOILGR,'E*6 ',NW, 3,0)
cc*
cc       CALL GETCD('WINDING ', 'COMBINED', KEY3, COMBWI,'E20* ',NW, 4)
cc*
C... Display the panel ( if required ) and check the indata
cc*
cc       if (.not.bbdisp) irc=isplnk('control','nondispl')
cc       irc=isplnk('display','p146-3 ')
cc*
C... Transfer SCROLL from dialog to CHARACTER variable
*
cc       CALL DMOVC(SCR,'SCROLL ',1,4)
*
C... Write the indata on the data base
*
       KEY3='       1'
*
cc       CALL PUTCD('WINDING ', 'WINDTYPE', KEY3, KWITYP,'C*3 ',NW, 4)
cc       CALL PUTRD('WINDING ', 'NGROUPS ', KEY3, NGROUP,'C*8 ',NW, 4,0)
cc       CALL PUTCD('WINDING ', 'CONDMAT ', KEY3, CONMAT,'C*9 ',NW, 3)
cc       CALL PUTCD('WINDING ', 'CONDVARN', KEY3, CONVAR,'C1*2 ',NW, 3)
cc       CALL PUTCD('WINDING ', 'TCEPOXY ', KEY3, TCEPOX,'C1*3 ',NW,3)
cc*
cc       CALL PUTRD('WINDING ', 'EXTINSAR', KEY3, EXTRAR,'E*10 ',NW, 5,0)
cc       CALL PUTRD('WINDING ', 'HCLACK  ', KEY3, HCLAC ,'E*11 ',NW, 4,1)
cc*
cc       CALL PUTRD('WINDING ', 'HSTRAND ', KEY3, HPART, 'E*1 ',NW, 5,2)
cc*
cc       CALL PUTRD('WINDING ', 'COVSTR  ', KEY3, COVSTR,'E*3 ',NW, 5,2)
cc       CALL PUTRD('WINDING ', 'COVCBL  ', KEY3, COVCBL,'E*4 ',NW, 5,2)
cc*
cc       CALL PUTRD('WINDING ', 'NCOOLD  ', KEY3, NCDUCT,'E*5 ',NW, 3,0)
cc       CALL PUTRD('WINDING ', 'NOILGRGC', KEY3, XOILGR,'E*6 ',NW, 3,0)
cc*
cc       CALL PUTCD('WINDING ', 'COMBINED', KEY3, COMBWI,'E20* ',NW, 4)
cc*
       RETURN
       END
