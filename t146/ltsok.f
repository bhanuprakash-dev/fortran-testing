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
*      Subroutine LTSOK
*      ----------------
*
*  0.  Written: XX-XX-XX by S-E Jansson       , XXXX
*      Revised: 86-12-09 by J. Akesson        , Z1KDC
*      Revised: 91-11-27 by Ron Bell          , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the Yoke reduction for the LTS core
*
************************************************************************
*
       SUBROUTINE LTSOK (B,AN)
*
       REAL       AN,B(100)
*
       INTEGER    N

       N=NINT(AN)
*
       IF (B(N  ).EQ.B(N-1).OR.
     &     B(N-1).EQ.B(N-2)) THEN
          B(N)=B(N-3)
          B(N-1)=B(N-3)
          B(N-2)=B(N-3)
       ELSE
          B(N)=B(N-2)
          B(N-1)=B(N-2)
       END IF
*
       RETURN
       END
