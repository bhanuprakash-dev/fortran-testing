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
*      Function NCDLTB
*      ---------------
*
C...   Title:  Determines the number of cooling ducts in the
C...           LTB and TAA-M core based on the Core Diameter
*
*      Written: 91-12-20 by Ron Bell         , ABB Power T & D, Muncie
*      Revised:   -  -   by                  ,
*
************************************************************************
*
       INTEGER FUNCTION NCDLTB(TYP,DCOR)
*
C...   DCOR      Core diameter  (mm)
*
       REAL      TYP,DCOR
*
       INTEGER   NCD
*
       NCD=0
*
       IF (NINT(TYP).EQ.10) THEN
          IF (DCOR.GE.720. ) NCD=1
          IF (DCOR.GE.800. ) NCD=2
          IF (DCOR.GE.860. ) NCD=3
          IF (DCOR.GE.960. ) NCD=4
          IF (DCOR.GE.1025.) NCD=5
          IF (DCOR.GE.1125.) NCD=6
          IF (DCOR.GE.1225.) NCD=7
          IF (DCOR.GE.1325.) NCD=8
          IF (DCOR.GE.1365.) NCD=9
       ELSE
          IF (DCOR.GE.800. ) NCD=1
          IF (DCOR.GE.1025.) NCD=2
          IF (DCOR.GE.1145.) NCD=3
          IF (DCOR.GE.1325.) NCD=4
       END IF
*
       NCDLTB=NCD
*
       RETURN
       END
