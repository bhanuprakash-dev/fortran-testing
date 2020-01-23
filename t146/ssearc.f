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
*      Subroutine SSEARC
*      -----------------
*
C...   Title:    Search for a string in an array of strings
*
C... Routine gives back index (IND) for that array element than is equal
C... to ARG. If match FOUND = .TRUE.  else FOUND = .FALSE. and IND = 0.
*
*      Written: 91-11-21 by B-G Bladh      , SETFO/TS
*      Revised: ......................     , .........
*
************************************************************************
*

*
       SUBROUTINE SSEARC(ARG, ARRAY, MAX, IND, FOUND)
*
       CHARACTER ARG*(*), ARRAY*(*)
       DIMENSION ARRAY(MAX)
       LOGICAL   FOUND
       INTEGER   MAX, IND, I
*
       FOUND =.FALSE.
       IND   = 0
       I     = 1
*
 100   IF ((.NOT.FOUND) .AND. I.LE. MAX) THEN
          IF (ARG .EQ. ARRAY(I)) THEN
             IND = I
             FOUND = .TRUE.
          ELSE
             I = I+1
          ENDIF
          GO TO 100
       ENDIF
*
       RETURN
       END
