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
*      Logical Function  FBB
*      ---------------------
*
C...   Title:  Prompts for a Yes or No answer
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       FUNCTION FBB(SUB,BBERR,BBHELP,BBTS,JFC,TEXT)
*
          include'com1.h'
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       EQUIVALENCE
     &  (XZ2782       ,  ISTOP         )
       INTEGER   ICODE,JFC, ISTOP
*
       LOGICAL   FBB,BBTS,BBERR,BBHELP(10)
*
       CHARACTER SUB*6,TEXT*(*)
*
CC*SEBL End of the declarations block.
*
C... Determine whether a Yes or No has been given
*
CC*SBBL Start of the declarations block.
*
c       print *,TEXT,'FBB'
       ICODE=0
       IF(TEXT(1:1).EQ.'Y') ICODE=1
       IF(TEXT(1:1).EQ.'N') ICODE=2
*
C... Print a message if YES or NO has not been given
*
       IF(ICODE.EQ.0) THEN
           CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &                        SUB//'(FBB):Y,Yes,N or No is required')
* ...      BACKTRACKING
            IF(ISTOP.EQ.1) RETURN
       ENDIF
*
       FBB=.FALSE.
       IF (ICODE.EQ.1) FBB=.TRUE.
*
CC*SEBL End of the declarations block.
*
       RETURN
       END
