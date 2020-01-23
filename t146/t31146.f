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
*      Program T31146
*      --------------
*
C...   Title:  Main program for optimisation of power transformers.
*
*      Written: 83-01-25 by SE Jansson , ZKB
*      Revised: 83-01-25 by SE Jansson , ZKB
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-12-02 by B-G Bladh  , SETFO/TS; call of DBPREP added
*      Revised: 92-02-11 by B-G Bladh  , SETFO/TS; DSYOBJ moved to
*                                                  dialog variable
*
************************************************************************
*
       PROGRAM T31146
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1391       ,  BBTS          )
       EQUIVALENCE
     &  (XZ1485       ,  IVERS         ),
     &  (XZ1506       ,  NUMN          ),
     &  (XZ1507       ,  NUYR          ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ1589       ,  DATE          ),
     &  (XZ1379       ,  BBERR1        ),
     &  (XZ2551       ,  DVERS         ),
     &  (XZ1486       ,  JFC           )
*
       INTEGER   IND,IRC,IVERS,NUMN,NUDY,NUYR,JFC,ISTOP
*
       LOGICAL   BBDISP,BBSTOR,BBRUN,BBTS,BBRES,BBERR1
*
       CHARACTER OPTION*1,SCR*4,DVERS*10,DATE*26

        integer getprf, scall, lopt,lname
        character DSOBJ*20


c#ifdef SUNOS
cc       external getprf ]·pragma C ( getprf )
cc       external scall ]·scall C ( scall )
c#endif

*
C... Set the version number and date
*
cd      write(*,*) 't21146 at very top'   !dbug
cc       IVERS=55
cc       DVERS='1993-01-25'
*
C...   ISTOP = 1 indicates fatal error and backtracking
       ISTOP = 0
*
C... Read the current date and time

*
cc       CALL DATIMS(NUMN,NUDY,NUYR,DATE,2)
*
cc       CALL CMOVD(DATE ,'DAIT ', 1,26)
cc       CALL RMOVD(FLOAT(IVERS),'VER ',1,3,0)
cc       CALL CMOVD(DVERS,'DVERS ',1,10)
cd      write(*,*) '******* T31146 after CALL CMOVD(DVERS)**' !dbug
*
C... Select the running environment
*
       BBTS=.TRUE.
*
       BBRES=.FALSE.
       BBSTOR=.FALSE.
       BBRUN=.TRUE.
*
C... Read the option selected ( 1 = Input , 2 = Run , 3 = Output )
*
       lname = 3
ccd      write(*,*) 'in T31146 B4 call getprt'   !dbug
cc       iret = getprf('OPT ', lname, OPTION, lopt)
cd      write(*,*) 'in T31146 AFT call getprt'   !dbug

c      CALL DMOVC(OPTION,'OPT ',1,1)
cd      write(*,*) '******* T31146 after CALL DMOVC(OPTION)*'  !dbug
cc       BBDISP=.FALSE.
cc       IF(OPTION.EQ.'1') BBDISP=.TRUE.
cc       IF(OPTION.EQ.'2') BBRUN=.TRUE.
*
C...   Move object name from SHELL-variable to DIALOG-variable
cc       lname = 11
cc       irc = getprf("BAT_CUR_OBJ ",lname,DSOBJ,lopt)
cc       if(lopt.lt.20) then
cc          do 15 i= lopt+1, 20
cc 15        DSOBJ(i:i) = ' '
cc      endif
cc       call cmovd(DSOBJ,"DSYOBJ ", 1, 20)
*
C... Read the data base
*

       CALL INX146(BBDISP,SCR)

      write(*,*) '**** T31146 After CALL INX146 ***********' !dbug
*
C... Repeat until exit ( Option = X )
*

cc  300  IF ((OPTION.NE.'X').AND.(OPTION.NE.'1')) THEN
*
C... If no exit has been asked for
*
cc          IF(SCR(1:1).NE.'X') THEN
*
C... Display the Results Panel P146
*
cc             IF (.NOT.BBRUN) THEN
*
C... Transfer results to dialog variables for the panel display
*
cc                CALL MOVEDB
cd       write(*,*) '**** T31146 Before CALL DSP146 ***********' !dbug
*
C... Display the Results Panel P146
*

cc                CALL DSP146(OPTION,BBRES)
cc             END IF
*
C... Read the current date and time
*
cd       write(*,*) '**** T31146 Before CALL DATIMS ***********' !dbug
cc             CALL DATIMS(NUMN,NUDY,NUYR,DATE,2)
*
cc             CALL CMOVD(DATE ,'DAIT ', 1,26)
*
C... Select options
*
cc             IND=INDEX('1234X',OPTION)
*
C,,, 'INPUT' has been selected
*
cc             IF (IND.EQ.1) THEN
cc                BBDISP=.TRUE.
*
C... Read the data base
*

cd        write(*,*) '**** T31146 Before CALL INX146 ....*******' !dbug
cc                CALL INX146(BBDISP,SCR)
cd       write(*,*) '**** T31146 After  CALL INX146 ....*******'  !dbug
*
C... 'Run' has been selected
*
cc             ELSE IF (IND.EQ.2) THEN
                OPEN(JFC, FILE='t146.result' , FORM = 'formatted')
                REWIND(JFC)
*
C... Write input data on neutral file
*

cd       write(*,*) '**** T31146 before CALL DBPREP ....*******' !dbug
cc                IF(ISTOP.NE.1) CALL DBPREP
*
C... Analyse the input data
*
cD      write(*,*) '**** T31146 before CALL WX146  ....*******' !dbug
cD      write(*,*) 'ISTOP=',ISTOP
                IF(ISTOP.NE.1) CALL WX146

*
C... Perform the calculations
*
cd       write(*,*) '**** T31146 before CALL SUB146 ....*******' !dbug
               IF(ISTOP.NE.1) CALL SUB146(BBRES)
               BBSTOR=.FALSE.
                BBRUN=.FALSE.
                REWIND(JFC)
                CLOSE(JFC)
*
C... 'Output of results' has been selected
*
cc             ELSE IF (IND.EQ.3) THEN
ccc             irc=isplnk('select',13,'cmd(%t146wot )')
cc                irc = scall("browse t146.result "//char(0), k)
*
C... 'Store calculated results and modified indata' has been selected
*
cc             ELSE IF (IND.EQ.4) THEN
*
C... Store calculated results and modified indata
*
cc                CALL PUTDB(BBSTOR)
cc             END IF

*
cc          END IF
*
cc          IF(ISTOP.EQ.1) THEN
cc                REWIND(JFC)
cc                CLOSE(JFC)
cc                BBERR1 = .TRUE.
ccc             irc=isplnk('select', 13,'cmd(%t146wot )')
cc                irc = scall("browse t146.result "//char(0), k)
cc                ISTOP = 0
cc          ENDIF
*
cc          GOTO 300
*
C... Exit
*
cc       END IF
*
C... Clear the screen of displays made during the run of the program
*
ccc    irc=isplnk('select',13,'cmd(clrscrn )')
*
        CALL PRINTXZ

       END
