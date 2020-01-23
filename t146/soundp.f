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
*      Function Soundp
*      -----------------
*
C...   Title:    Calculation of transformer sound power level
*                According to SETFO/TA 4567-101E  1990-10-08
*      Written: 92-02-14 by B-G Bladh      , SETFO/TS
*      Revised: ......................     , .........
*
************************************************************************
*
*
       FUNCTION SOUNDP(TYP,CD,LP,LH,ISTGRD,B,F)
*
*      TYP   = Core type code:
*              1: D;  3: T;  7: EY;  8: DY;  9: TY-1;  10: TY-3
*      CD    = Core diameter            (m)
*      LP    = Limb pitch               (m)
*      LH    = Limb height              (m)
*      ISTGRD = Core lamination quality code:
*              1: M5;  2: M2;  3: ZDKH;  4: PLJT
*      B     = Flux density in corelimb (T)
*      F     = Frequency                (Hz)
*      SOUNDP= Transformer sound power level (dB)
*
C... Declarations
*
       DIMENSION C(10)
*
       REAL
     &     L0, LP, LH, CB, CD, CF, CS, CQ, B, C, F, TYP
*
       INTEGER  ISTGRD , ITYP
       DATA C/ -2., 0., 0., 0., 0.,0., -2., -2., 0.,   0./
*               D       T              EY   DY   TY-1  TY-3
*
       ITYP = NINT(TYP)
*
*      CHECK of TYP
       IF(ITYP.LT.1 .OR. ITYP.GT.10) THEN
         ITYP = 3
         WRITE(*,*)
     &   '***WARNING*** Wrong core type code in routine SOUNDP '
         WRITE(*,*)
     &   '***WARNING*** Core type T assigned. *****************'
       ENDIF
*
*...Size variable
*
       CS = 20.*ALOG10(LH * LP * SQRT(CD))
*
*...Flux variable
*
       IF(B.GE.1.6) THEN
          CB = 0.061*B**9
       ELSE
          CB = 23.6*B - 33.6
       ENDIF
*
*...Frequence variable
*	
       CF = 45.*ALOG10(F/50.)
*
*...Quality variable
*
       IF(ISTGRD.EQ.2)     THEN
         CQ = 0.8
       ELSEIF(ISTGRD.EQ.3) THEN
         CQ = 0.
       ELSEIF(ISTGRD.EQ.4) THEN
         CQ = 3.
       ELSE
         CQ = 7.
       ENDIF
*
       L0 = 68.4+CS+CB+CF+CQ+C(ITYP)
*
       SOUNDP = L0
*
       RETURN
       END

