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
*      Subroutine FPRINT
*      -----------------
*
C...   Title:  Routine for error printouts
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE FPRINT(IND,BBERR,BBHELP,BBTS,JFC,TEXT2)
*
C... Declarations
*
        include'com1.h'
*
        EQUIVALENCE
     &  (XZ2782       ,  ISTOP         )
C
*
       INTEGER   IND,IRC,JFC,ISTOP
*
       DIMENSION TEXT1(2)
       CHARACTER TEXT1*5,ANSW*4
*
       CHARACTER*(*) TEXT2
*
       LOGICAL   BBERR,BBHELP(10),BBTS
*
       DATA  TEXT1/ ' Note', 'Error'/
*
CC*SEBL End of the declarations block.
*
C... Prepare the message.
*
CC*SBBL Start of the message block.
*
       WRITE(JFC,60) TEXT1(IND),TEXT2
       WRITE(*,60) TEXT1(IND),TEXT2
*
*
C... Write the message.
*
       IF(IND.NE.1) THEN
            BBERR=.TRUE.
            STOP
       ENDIF
       RETURN
*
CC*SEBL End of the message block.
*
   40  FORMAT(1X,3(/),10X,A)
   45  FORMAT(A)
   60  FORMAT(6X,A5,' : ',A)
*
       END
