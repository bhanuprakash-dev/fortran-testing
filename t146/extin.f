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
*********************************************************************
*      Subroutine Extin
*      -----------------
C...   Title:  Read cost files from external files.
*
*      Written: XX-XX-XX by B-G Bladh  , SETFO/TS
*      Moved from OPTSUB to T146 and rewritten
*               92-01-07 by B-G Bladh  , SETFO/TS
*      Revised: XX-XX-XX ............  , .........
*********************************************************************
C
*
       SUBROUTINE EXTIN(MNL,JFC,FILECH,BB79,MNLY)
*
       include'com1.h'
*
       CHARACTER CHFILE*24,FILECH*24,MNLKOD*8,MNL*8,
     &           cstnam*50, cont*50
*
       INTEGER   IMNL, ISTOP

       EQUIVALENCE
     & (XZ2782,   ISTOP )
*
       LOGICAL   BB79
*

       integer getprf

cc#ifdef SUNOS
cc       external getprf !$pragma C ( getprf )
cc#endif
*
cc       iret = getprf("BAT_LIB", 7, cont, lcont)

       print*,'inside extin.f'
cc       cstnam = 'costfr'
       open(10, file='costfr',status='old',action='read')
*
  100  REWIND 10
       READ(10,20,END=23) DUMMY
       GO TO 24
   23  WRITE (*,30) 'EXTIN:  File 10 is empty'
*
   24  MNLKOD = '        '
       FILECH = '                        '
       IMNL   = 0
*
c       write(*,*)'MNL given =',MNL
       NRINDT=0
       DO 21 I=1,100
          READ(10,10,END=22) MNLKOD,CHFILE,IMNL
c          write(*,*)'MNLKOD=',MNLKOD
c          write(*,*)'CHFILE=',CHFILE
c          write(*,*)'IMNL=',IMNL
          IF (MNL.EQ.MNLKOD) THEN
             FILECH=CHFILE
             NRINDT=I
             BB79=.FALSE.
             MNLY=IMNL
             IF(MNLY.LT.91 .AND. MNLY.GT.79) BB79= .TRUE.
          END IF
  21   CONTINUE
*
       WRITE (*,30) 'EXTIN: More than 99 MNL-codes on file 10'
*
   22  REWIND 10
       CLOSE(10)
*
       IF (NRINDT.LE.0) THEN
          WRITE(*,30) 'EXTIN: The MNL-code given is not allowed'
          WRITE(JFC,30) 'EXTIN: The MNL-code given is not allowed'
          ISTOP = 1
          RETURN
       END IF
*
       print*,'coming out of extin.f'
*
  199  RETURN
*
   10  FORMAT(8X,A8,4X,A24,15X,I3,6X,A4)
   20  FORMAT(F8.2)
   30  FORMAT(' ',A,35(/))
       END
