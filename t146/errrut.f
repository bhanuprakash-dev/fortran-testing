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
*      Subroutine ERRRUT
*      -----------------
*
C...   Title:  Prints warnings when the optimisation goes wrong
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE ERRRUT(BBERR1,BBFREQ,JFC)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       INTEGER  JFC
*
       LOGICAL  BBERR1,BBFREQ
*
CC*SEBL End of the declarations block.
*
C... Print the appropriate error message if necessary.
*
CC*SBBL Start of the Printing block.
*
       WRITE(JFC,600)
       IF(BBERR1) WRITE(JFC,610)
       IF(BBFREQ) WRITE(JFC,620)
       WRITE(JFC,630)
*
CC*SEBL End of the Printing block.
*
       RETURN
 600   FORMAT(1X,'**************'/1X,'***')
 610   FORMAT(1X,'***  WARNING: Optimization not complete.')
 620   FORMAT(1X,'***  WARNING: Unreliable calculation of P0 because'/
     &        1X,'***           frequency not equal to 50 or 60 Hz')
 630   FORMAT(1X,'***'/1X,'**************')
       END
