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
*      Function FTJBR
*      --------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculation of Gross Thickness of Core Plate
C...               at the current Width of Packet
*
************************************************************************
*
       REAL FUNCTION FTJBR(TNCORS,ETA,ETAL,ILACK,TYP,DCOR,B)
*
       REAL       TNCORS,ETA,ETAL,TYP,DCOR,B
*
       INTEGER    ILACK
*
       LOGICAL    CORVAR
*      
       WRITE(*,*) 'FTJBR.F TNCORS, ETA', TNCORS, ETA
       WRITE(*,*) 'FTJBR.F ILACK, TYP', ILACK, TYP
       FTJBR = TNCORS/ETA
*
       IF (ILACK.NE.2) THEN
          IF (ILACK.EQ.1) THEN
             FTJBR = TNCORS/ETAL
          ELSE
             IF (CORVAR(TYP,DCOR,B)) FTJBR = TNCORS/ETAL
          END IF
       END IF
*
       RETURN
       END
