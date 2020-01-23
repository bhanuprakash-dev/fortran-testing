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
*
************************************************************************
*
*      Subroutine PREACT
*      -----------------
*
C...   Title:  Set values for AARR for future needs.
*
*      Written: 88-11-14 by Per Sãderberg  , TRAFO/IK
*      Revised:
*
************************************************************************
*
       SUBROUTINE PREACT
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    )
*
       REAL      AARR(200)
*
CC*SEBL End of the declarations block.
*
       RETURN
       END
