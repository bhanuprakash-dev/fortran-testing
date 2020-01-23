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
*      Subroutine Covma
*      -----------------
*
C...   Title:  Calculation of masses for cover
*
*      Written: 91-12-19 by B-G Bladh  , SETFO/TS
************************************************************************
*
       SUBROUTINE COVMA(ACOV,GCOVER,KTAN79)
*
*      COVER
*      ACOV =     COVER AREA               (M2)
*      GCOVER =   COVER MASS
*
*      KTAN79 =   TANK CODE
*                 1 = OVAL TANK         TMY
*                 2 = RECTANGULAR TANK  TMY
*                 3 = TAA, VACUUM-PROOF
*                 4 = TAA, NOT VAKUUM-PROOF
*                 5 = TBA
*                 6 = OTHER TYPES , TMY
*
*------------------------------------------------------------------
       REAL    CA, CB
       DIMENSION CA(6),CB(6)
*
       LOGICAL BB51,BB07,BBPRNT
*
       DATA CA/172.6, 121.4, 160.9, 59.71, 110.85, 96.99 / ,
     &      CB/1.040, 1.148, 1.172, 1.394, 1.200, 1.230 /
*

*
       GCOVER =  (CA(KTAN79) * ACOV**CB(KTAN79))*1.02
*
       RETURN
       END
