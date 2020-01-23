*********************************************************************
*                                                                   *
*      The copyright to the computer program herein is  the         *
*      property of ABB  TRANSFORMERS, Sweden.  The  program         *
*      may be used or copied only with the written  permis-         *
*      sion of ABB  TRANSFORMERS or in accordance with  the         *
*      terms of agreement under which the program has  been         *
*      supplied.                                                    *
*                                                                   *
*      In no event shall ABB  TRANSFORMERS  be  liable  for         *
*      incidental or consequential damages arising from use         *
*      of this program.                                             *
*                                                                   *
*********************************************************************

************************************************************************
*
*      Function CORVAR
*      -----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Determines whether the core is
C...               fully or partly varnished or not at all
C...               According to LT-TA 4610-117
*
************************************************************************
*
       LOGICAL FUNCTION CORVAR(TYP,DCOR,BPL)
*
       REAL     TYP,DCOR,BPL

C---   TYP = Core Type ( D  =  1,
C...                     T  =  3,
C...                     EY =  7,
C...                     DY =  8,
C...             TY 1-PHASE =  9,
C...             TY 3-PHASE = 10)
*
C... If the whole core is to be varnished then TYP=0
C... DCOR   = Nominal core diameter
C... BPL  = Core plate width in mm
*
       CORVAR = .FALSE.
*
C... Whole core varnished
*
       IF (NINT(TYP).LT.1) THEN
          CORVAR=.TRUE.
*
C... Part of core varnished
C
       ELSE IF (NINT(TYP).LT.7) THEN
*
C... Undivided cores
*
          IF (DCOR.GE.750..AND.BPL.GE.500.) CORVAR=.TRUE.
*
       ELSE
*
C... Divided cores
*
          IF ((NINT(TYP).LE. 9.AND.DCOR.GE.750..AND.BPL.GE.350.).OR.
     &        (NINT(TYP).EQ.10.AND.DCOR.GE.600..AND.BPL.GE.250.))
     &       CORVAR=.TRUE.
       END IF
*
       RETURN
       END
