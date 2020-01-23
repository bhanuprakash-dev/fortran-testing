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
*      Subroutine INP5
*      ----------------
*
C...   Title:    Read indata for panel 5 ( P146-5 )
*
*      Written: 91-09-16 by B-G Bladh       , SETFO/TS
*      Revised:
*
************************************************************************
*
       SUBROUTINE INP5(BBDISP,SCR)
*
C... Declarations
*
       CHARACTER SCR*4, KEY3*8
       INTEGER   I, IRC
       LOGICAL   BBDISP
*
       include'cominp.h'
*
       KEY3 = '       1'
*
C... Read indata from the data base for panel 5 P146-5
*
*
		open(2,file='DATA_9220_10_inp5',status='old',action='read')

		read(2,*) BOOSMA
		read(2,*) BOOSVO
		read(2,*) BOOSRE
		read(2,*) BOOSTR
*
C PRINT
c		write(*,*) BOOSMA
c		write(*,*) BOOSVO
c		write(*,*) BOOSRE
c		write(*,*) BOOSTR
c		
                close(2)
       RETURN
       END
