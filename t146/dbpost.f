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
*      Subroutine DBPOST
*      ------------------
C...   Title:  Writing data on neutral file.
*
*      Written: 91-11-12 by B-G Bladh      , SETFO/TS
*      Revised: ....................       , ........
************************************************************************
*
       SUBROUTINE DBPOST(UTF, TABLE, TITXT,  NK, KEY, KEYV )
*
C... Declarations
       INTEGER
     &      NK, UTF, I
*
       CHARACTER
     &      PARAM*12, BLANK*2, TAB*12, DESC*12,
     &      KEY*12,KEYV*(*),TABLE*(*),TITXT*(*)
*
       DIMENSION
     &      KEY(NK), KEYV(NK)

       DATA
     &      PARAM/'PARAMETER   '/,
     &      DESC /'DESCRIPTION '/,
     &      TAB  /'TABLE=      '/,
     &      BLANK/'  '/
*
C...   Each new sets start with "TABLE=      xxxx  "
       WRITE(UTF, 801) TAB, TABLE
*
 100   DO 199  I= 1, NK
            WRITE(UTF,800) PARAM, KEY(I), BLANK, KEYV(I)
 199        CONTINUE
*
       WRITE(UTF, 801) DESC, TITXT
       WRITE(UTF, 801) BLANK
*
       RETURN
*
 800   FORMAT(1X, A,A,A,A)
 801   FORMAT(1X, A,A)
       END
