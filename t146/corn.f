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
*      Function CORN
*      -------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 87-04-27 by Ron Bell     , TRAFO/IK
*
*  1.  Description
C...       Title:  Determine the corner radius of a conductor
*
************************************************************************
*
       FUNCTION CORN(THICKN)
*
       IF (                     THICKN.LT.0.0017) AREA=0.215
       IF (THICKN.GE.0.0017.AND.THICKN.LT.0.0022) AREA=0.36
       IF (THICKN.GE.0.0022.AND.THICKN.LT.0.0037) AREA=0.55
       IF (THICKN.GE.0.0037                     ) AREA=0.86
*
       CORN=AREA
*
       RETURN
       END
