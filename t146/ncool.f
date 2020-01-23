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
*      Function   NCOOL
*      -----------------
*
C...   Title:  Calculation of cooling ducts in transformer cores
*
*      Written: 89-10-02 by B-G Bladh     , SETFO/K1
*      Revised: 90-02-12 by Per S�derberg , SETFO/IK
*      Revised: 92-04-08 by B-G Bladh ,     SETFO/TS
*
************************************************************************
*
        FUNCTION NCOOL(CORESE,TYPCOR,DK1,TOPOIL,B,FREKV,BBSTLA,
     &                 JFC, ISTGRD)
*
*       CORESE  Coreseries (TAA, TAC, TAD, TBA, TCA, SL-C)
*       TYPCOR  CORETYP (D, T, EY, ...) AS A REAL NUMBER .
*       DK1     Corediameter  (m)
*       TOPOIL  Top oil temperature
*       B       max flux density (T)
*       FREKV   frequency  (HZ)
*       BBSTLA  step-lap core   (.T.  OR .N.)
*
        LOGICAL   BBSTLA
        CHARACTER CORESE*4
        INTEGER   KSERIE, JFC, ISTGRD
*
C... DK1 I M
*
        DK = DK1 * 1000.
*
C... TAA, TAD, TAC
*
       IF (CORESE.EQ.'TAA ' .OR. CORESE.EQ.'TAC ' .OR.
     &    CORESE.EQ.'TAD ') THEN
          NCOOL=NCDTAD(DK,TOPOIL,B,FREKV,BBSTLA,ISTGRD)
*
C... TBA
*
       ELSEIF (CORESE.EQ.'TBA ') THEN
          NCOOL=NCDTBA(DK,TOPOIL,B,FREKV,BBSTLA,ISTGRD)
*
C... TCA
*
       ELSEIF (CORESE.EQ.'TCA ') THEN
          WRITE(*,*) '(KSERIE,TYPCOR,DK1,TOPOIL,B,FREKV)', KSERIE,TYPCOR,DK1,TOPOIL,B,FREKV
          KSERIE = 4
          NCOOL=NCDTCA(KSERIE,TYPCOR,DK1,TOPOIL,B,FREKV)
          WRITE (*,*) '$NCOOL = ', NCOOL
*
C... TAA-M
*
       ELSEIF (CORESE.EQ.'TAAM') THEN
          KSERIE = 5
          NCOOL=NCDTCA(KSERIE,TYPCOR,DK1,TOPOIL,B,FREKV)
*
C... LTB  (old side limb core)
*
       ELSE
          KSERIE =5
          NCOOL=NCDTCA(KSERIE,TYPCOR,DK1,TOPOIL,B,FREKV)
       ENDIF
*
       RETURN
       END
