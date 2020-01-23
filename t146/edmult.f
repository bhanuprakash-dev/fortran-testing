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
*      Function EDMULT
*      ---------------
*
C...   Title:  Compute the eddy loss in winding IWDG.
*
*      Written: 88-11-04 by Per Sãderberg, TRAFO/IK
*      Revised:
*
************************************************************************
*
       REAL FUNCTION EDMULT(FW1,FW2,EDCONW)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL FW1,FW2,EDCONW
*
CC*SEBL End of the declarations block.
*
       EDMULT=EDCONW*(3.*FW2*(FW1+FW2)+(FW1*FW1))
*
       RETURN
       END
