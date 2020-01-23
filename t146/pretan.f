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
*      Subroutine PRETAN
*      -----------------
*
C...   Title: Assigns values to diverse variables
C...          for calculation of the tank
C...                                                                .
C...  TANK PARAMETERS
C... -----------------------------------------------------
C... KTAN79  .  TYPE                  .  KTANK
C... .......................................................
C... 1       .  Oval                  .  3, 4
C... 2       .  Rectangular           .  2
C... 3       .  TAA - Vacuum proof    .  10
C... 4       .  TAA - Non-vacuum proof.  11
C... 5       .  TBA - Rectangular     .  12
C... 5       .  TBA - Oval            .  13
C... 5       .  TBA - One straight end.  14
C... 6       .  Curved + others       .  1, 5, 6, 7, 8, 9
C... .......................................................
C...
C... KTAN79 is used for the cost calculation in MNL79
C... F1TANK,F2TANK,F3TANK are geometrical factors used to
C... calculate tank volume & areas based on total dimensions.
*
*      Written: 1979     by B-G Bladh  , TRAFO/KB
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*
************************************************************************
*
       SUBROUTINE PRETAN(F1TANK,F2TANK,F3TANK,KTAN79,KTANK)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       REAL     F1(14),F2(14),F3(14),F1TANK,F2TANK,F3TANK
*
       INTEGER  IHLP(14),KT,KTANK,KTAN79
*
       DATA F1/1., 1., 1., 1. ,0.97, 0.97 ,0.97, 0.97, 0.97,
     &         1., 1., 1., 1., 1./
     &      F2/0.,0.,-0.214,-0.104,-0.101,-0.208,-0.208,-0.208,-0.208,
     &         -0.172, -0.172, 0., -0.215, -0.107/
     &      F3/0.57,1.,0.57,0.785,0.785,0.57,0.57,0.57,0.57,
     &         0.657, 0.657, 1., 0.570, 0.785/
*
*-------------   1  2  3  4  5  6  7  8  9  10 11 12 13 14   ----------
       DATA IHLP/6 ,2 ,1 ,1 ,6 ,6 ,6 ,6 ,6 , 3 ,4 ,5 ,5 ,5/
*
CC*SEBL End of the declarations block.
*
C... Allocation of values to certain tank variables.
*
CC*SBBL Start of the allocation block.
*
       KT=KTANK
       IF(KT.GT.14) KT=13
       IF(KT.LE. 0) KT=13
       F1TANK=F1(KT)
       F2TANK=F2(KT)
       F3TANK=F3(KT)
       KTAN79=IHLP(KT)
*
CC*SEBL End of the allocation block.
*
       RETURN
*
       END
