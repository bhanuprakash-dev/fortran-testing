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
*      Subroutine KUTS
*      ---------------
*
C...   Printing of the costmatrix.
*
*      Written: XX-XX-XX by A.N. Other      , XXXX
*      Revised: 90-01-09 by Per Sãderberg   , SETFO/IK
*
************************************************************************
*
       SUBROUTINE KUTS(JFC,NLIND,IDENT,DATE,IPAGE,RUB,BBFINE,
     &                 CONVAR,TCEPOX)
*
       CHARACTER     LTEXT(115)*24
       COMMON /LTXT/ LTEXT
C
       REAL            RES1(241),RES2(241),RES3(241),
     &                 RES4(241),RES5(241),RES6(241)
       COMMON /KONSRE/ RES1,RES2,RES3,
     &                 RES4,RES5,RES6
C
       CHARACTER IDENT*(*),RUB(15)*4,DATE*26,CONVAR(9)*4,TCEPOX(9)*4
C
       LOGICAL BBFINE
C
       IRAD=99
       DO 500 I=1,NLIND
       IF (CONVAR(I).EQ.'YES ') THEN
          LTEXT(1)=' Cond. Copper Varnish   3'
       ELSE
          LTEXT(1)=' Conductor Copper       3'
       ENDIF
       IF (TCEPOX(I).EQ.'YES ') THEN
          LTEXT(3)=' Cond. Transp. Epoxy    3'
       ELSE
          LTEXT(3)=' Conductor Transposed   3'
       ENDIF
       DO 400 J=1,18
       K=J+(I-1)*19
       CALL RPRINT(JFC,IDENT,DATE,IPAGE,IRAD,RUB,BBFINE,
     & LTEXT(J),RES1(K),RES2(K),RES3(K),RES4(K),RES5(K),RES6(K))
 400   CONTINUE
       K=K+1
       CALL RPRINT(JFC,IDENT,DATE,IPAGE,IRAD,RUB,BBFINE,
     & LTEXT(18+I),RES1(K),RES2(K),RES3(K),RES4(K),RES5(K),RES6(K))
 500   CONTINUE
C
       DO 600  K=27,115
       K126= K+126
       CALL RPRINT(JFC,IDENT,DATE,IPAGE,IRAD,RUB,BBFINE,
     & LTEXT(K),RES1(K126),RES2(K126),RES3(K126),
     & RES4(K126),RES5(K126),RES6(K126))
 600   CONTINUE
C
       RETURN
       END
