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
*      Subroutine INX146
*      -----------------
*
C...   Title:    Read the indata from the data base
*
*      Written: 83-01-25 by SE Jansson , ZKB
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-10-09 by B-G Bladh  , SETFO/TS
*               Panel 5 for boosters is included.
*      Revised: 92-04-14 by B-G Bladh  , SETFO/TS
*               Panel 6 for TA1 is included.
************************************************************************
*
       SUBROUTINE INX146(BBDISP,SCR)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       INTEGER   IND,IPNL
*
       LOGICAL   BBDISP
*
       CHARACTER SCR*4
*
CC*SEBL End of the declarations block.
*
       include'cominp.h'
*
*
C... Read the indata
*
       IND=1
*
C... Generate the panel
*
C,,, While needed
*
cc  200  IF(IND.NE.0.AND.IND.NE.7) THEN
*
cc          SCR='D   '
cc          CALL CMOVD(SCR,'SCROLL ',1,4)
*
C... Panel Selection
*
C,,, P146-1
*
cc       IF (IND.EQ.1) THEN
*
C... P146-1 - the first panel
*
          CALL INP1(BBDISP,SCR)
          IPNL=1
*
C... P146-2
*
cc       ELSE IF (IND.EQ.2) THEN
*
C... P146-2 - the second panel
*
          CALL INP2(BBDISP,SCR)
          IPNL=2
*
C... P146-3
*
cc       ELSE IF (IND.EQ.3) THEN
*
C... P146-3 - the third panel
*
          CALL INP3(BBDISP,SCR)
          IPNL=3
*
C... P146-4
*
cc       ELSE IF (IND.EQ.4) THEN
*
C... P146-4 - the fourth panel
*
          CALL INP4(BBDISP,SCR)
          IPNL=4
*
C... P146-5 - the fifth panel, only for booster trafos.
*
cc       ELSE IF (IND.EQ.5) THEN
*
          CALL INP5(BBDISP,SCR)
          IPNL=5
*
C... P146-6 - the sixth panel, only for TA1 cores
*
cc       ELSE IF (IND.EQ.6) THEN
*
cc          CALL INP6(BBDISP,SCR)
cc          IPNL=6
cc       ENDIF
*

cc       IND=INDEX('123456XUD',SCR(1:1))
cc       IF (IND.LT.1 .OR. IND.GT.9) IND = 9

cc          IF(IND.GT.6 ) THEN
cc               IF(IND.EQ.8) IND= IPNL-1
cc               IF(IND.EQ.9) IND= IPNL+1
cc               IF(.NOT.BBBOOS      .AND. IND.EQ.5) IND = 6
cc               IF(CORESE.NE.'TA1 ' .AND. IND.EQ.6) IND = 7
cc          ENDIF
*
C..   The user will reach panel 5 for boosters in two cases:
*     - In normal sequence, and booster windings are declared
*       from panel 2.
*     - When panel 5 is directly ordered from the "INDEX"-variable
*     The same rules applies for panel 6  and TA1 cores.
cc       GOTO 200
cc       END IF
*
       RETURN
       END
