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
*      Subroutine WMESS
*      ----------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 83-01-04 by H.Westberg   , ZKAB
*      Revised: 87-04-27 by Ron Bell     , TRAFO/IK
*
*  1.  Description
C...       Title:  Print an error message relating to
C...               the automatic winding layout
C...               The message will appear only twice
*
************************************************************************
*
       SUBROUTINE WMESS(JFC,TEXT,NWARN)
*
       INTEGER   NWARN,JFC
*
       DIMENSION CTIMES(2)
       CHARACTER TEXT*70,CTIMES*6
*
       DATA      CTIMES /'First ','Second'/
*
       NWARN=NWARN+1
       IF (NWARN.LE.2) THEN
          WRITE (*,2) CTIMES(NWARN),TEXT
          WRITE (JFC,2) CTIMES(NWARN),TEXT
       ELSE IF (NWARN.EQ.2) THEN
          WRITE (*,3)
          WRITE (JFC,3)
       END IF
       RETURN
    1  FORMAT(1X,'  ')
    2  FORMAT(1X,A6,' warning : ',A50)
    3  FORMAT(1X,'Check the final result')
    4  FORMAT(A)
       END
