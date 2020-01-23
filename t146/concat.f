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
*      Subroutine CONCAT
*      -----------------
*
C...   Title:  Concatenates two character strings
*
*      Written: 82-04-07 by M.Weingart , USA
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE CONCAT(STRNG1,N,STRNG2,M,LENG)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       INTEGER   LENG,L1,M,M1,N,N1
*
       CHARACTER *(*) STRNG1,STRNG2
*
CC*SEBL End of the declarations block.
*
C... Concatenate the strings
*
CC*SBBL Start of the concatenation block.
*
       L1=MAX0(LENG,1)
       N1=MAX0(N,1)
       M1=MAX0(M,1)
       STRNG1(N1:N1+L1-1)=STRNG2(M1:M1+L1-1)
*
CC*SEBL End of the concatenation block.
*
       RETURN
       END
