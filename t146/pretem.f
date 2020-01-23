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
*      Subroutine PRETEM
*      -----------------
*
C...   Title: Assigns values to topoil temp.rise over 30 deg ambient
C...          which is used for calculation of the core.
C...          No. of cooling ducts set to 0 if guided oil flow used.
C...          ( Mean oil temperature rises )
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PRETEM(AARR,DTSNN,DTSNF,DTSFF)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL      AARR(200),DTSNN,DTSNF,DTSFF
*
CC*SEBL End of the declarations block.
*
C... Allocation of values to certain temperature variables.
*
CC*SBBL Start of the allocation block.
*
       DTSNN=(AARR(156)-18.)
       DTSNF=(AARR(156)-25.)
       DTSFF=(AARR(156)-18.)
       IF(AARR(110).GT.0.1) DTSNN=AARR(110)
       IF(AARR(120).GT.0.1) DTSNF=AARR(120)
       IF(AARR(130).GT.0.1) DTSFF=AARR(130)
*
CC*SEBL End of the allocation block.
*
       RETURN
       END
