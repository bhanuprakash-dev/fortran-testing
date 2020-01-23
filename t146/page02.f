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
*      Subroutine PAGE02
*      -----------------
*
C...   Title: Output of detailed losses, no-load losses,
C...          sound level, reactances, voltage drop and efficiency,
C...        & check of short circuit stresses.
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE PAGE02(JFC,BBOUT,BBRES)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       INTEGER  JFC
*
       LOGICAL  BBOUT,BBRES
*
CC*SEBL End of the declarations block.
*
       BBOUT=.TRUE.
*
C... Load losses
*
CC*SBBL Start of the load losses block.
*
C... New page
*
       CALL NPAGE(0)
       WRITE(JFC, 80)
*
C... Calculation routine
*      
       print *,"PAGE02 > OPCALC"
       CALL OPCALC(BBOUT)
*
CC*SEBL End of the load losses block.
*
C... No-load losses and efficiency
*
       CALL PRNLD(BBRES)
*
C... Reactances
*
       CALL PRUX
*
C... Voltage cases and efficiency
*
       CALL PREFF
*
C... Short-circuit voltages
*
C... New page
*
       CALL NPAGE(0)
       WRITE(JFC,85)
*
C... Short circuit dimensioning routine
*
       CALL SCDIME(BBOUT)
*
       BBOUT = .FALSE.
*
       RETURN
*
   80  FORMAT(1X,'     O P E R A T I N G   ',
     &                'C H A R A C T E R I S T I C S')
   85  FORMAT(1X,'    ')
*
       END
