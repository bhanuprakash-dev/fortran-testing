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
*      Subroutine INP6
*      ----------------
*
C...   Title:    Read indata for panel 6( P146-6 )
*
*      Written: 92-04-10 by B-G Bladh       , SETFO/TS
*      Revised:
*
************************************************************************
*
       SUBROUTINE INP6(BBDISP,SCR)
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
C... Read indata from the data base for panel 6  P146-6
*
*
       CALL GETRD('CORETA1 ','LAMTH   ',KEY3,LAMTH  ,'LAMTH   ',1,10, 2)
       CALL GETRD('CORETA1 ','LAMSPF  ',KEY3,LAMSPF ,'LAMSPF  ',1,10, 3)
       CALL GETRD('CORETA1 ','LAMMW   ',KEY3,LAMMW  ,'LAMMW   ',1,10, 0)
       CALL GETRD('CORETA1 ','LAMOVL  ',KEY3,LAMOVL ,'LAMOVL  ',1,10, 0)
       CALL GETRD('CORETA1 ','ACCLON  ',KEY3,ACCLON ,'ACCLON  ',1,10, 2)
       CALL GETRD('CORETA1 ','ACCTRA  ',KEY3,ACCTR  ,'ACCTRA  ',1,10, 2)
       CALL GETRD('CORETA1 ','ACCVER  ',KEY3,ACCVER ,'ACCVER  ',1,10, 2)
       CALL GETRD('CORETA1 ','AMBTEM  ',KEY3,AMBTEM ,'AMBTEM  ',1,10, 0)
       CALL GETRD('CORETA1 ','LAMSTP  ',KEY3,LAMSTP ,'LAMSTP  ',1,10, 0)
       CALL GETRD('CORETA1 ','LOSCOR  ',KEY3,LOSCOR ,'LOSCOR  ',1,10, 3)
       CALL GETRD('CORETA1 ','TA1HOL  ',KEY3,TA1HOL ,'TA1HOL  ',1,10, 0)
       CALL GETCD('CORETA1 ','FLPLMA  ',KEY3,FLPLMA ,'FLPLMA  ',1,8)
*
C... Display the panel ( if required ) and check the indata
*
       if(.not.bbdisp)irc=isplnk('control','nondispl')
       irc=isplnk('display','p146-6 ')
*
C... Transfer SCROLL from dialog to CHARACTER variable
*
       CALL DMOVC(SCR,'SCROLL ',1,4)
*
*
C... Write the indata on the data base
*
*
       CALL PUTRD('CORETA1 ','LAMTH   ',KEY3,LAMTH  ,'LAMTH   ',1,10, 2)
       CALL PUTRD('CORETA1 ','LAMSPF  ',KEY3,LAMSPF ,'LAMSPF  ',1,10, 3)
       CALL PUTRD('CORETA1 ','LAMMW   ',KEY3,LAMMW  ,'LAMMW   ',1,10, 0)
       CALL PUTRD('CORETA1 ','LAMOVL  ',KEY3,LAMOVL ,'LAMOVL  ',1,10, 0)
       CALL PUTRD('CORETA1 ','ACCLON  ',KEY3,ACCLON ,'ACCLON  ',1,10, 2)
       CALL PUTRD('CORETA1 ','ACCTRA  ',KEY3,ACCTR  ,'ACCTRA  ',1,10, 2)
       CALL PUTRD('CORETA1 ','ACCVER  ',KEY3,ACCVER ,'ACCVER  ',1,10, 2)
       CALL PUTRD('CORETA1 ','AMBTEM  ',KEY3,AMBTEM ,'AMBTEM  ',1,10, 0)
       CALL PUTRD('CORETA1 ','LAMSTP  ',KEY3,LAMSTP ,'LAMSTP  ',1,10, 0)
       CALL PUTRD('CORETA1 ','LOSCOR  ',KEY3,LOSCOR ,'LOSCOR  ',1,10, 3)
       CALL PUTRD('CORETA1 ','TA1HOL  ',KEY3,TA1HOL ,'TA1HOL  ',1,10, 0)
       CALL PUTCD('CORETA1 ','FLPLMA  ',KEY3,FLPLMA ,'FLPLMA  ',1,8)
*
       RETURN
       END
