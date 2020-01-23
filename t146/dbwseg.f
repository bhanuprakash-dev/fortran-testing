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
*      Subroutine DBWSEG
*      ------------------
*
C...   Title:  Writing data on neutral file. Winding segment data
*
*      Written: 91-11-12 by B-G Bladh      , SETFO/TS
*      Revised: ....................       , ........
*********************************************************************
*
       SUBROUTINE DBWSEG(UTF, NWIND)
*
C... This file contain winding segment data
*
       DIMENSION
     &       KEY(10), VAL(10)
*
       CHARACTER CWIND*12, CABTYP*8, AXSTRP*8,
     &           TAB*4,DESC*60, KEY*12, VAL*12, CHI*12, CHR*12
*
       INTEGER   IWDG , UTF, NVAL, NWIND
*
       include'cominp.h'
*
 200    DO 299 IWDG = 1, NWIND
*
           CWIND = CHI(IWDG)
*
C... Decide type of cable and designation of axial strip in the cable
*
C... SHEET WINDINGS ARE NOT HANDLED HERE:
*
           IF (CBLTYP(IWDG).EQ. 'LR') THEN
              CABTYP = 'N'
              AXSTRP = 'GS'
           ELSE IF (CBLTYP(IWDG).EQ. 'OR') THEN
              CABTYP = 'N'
              AXSTRP = 'US'
           ELSE IF (CBLTYP(IWDG).EQ. 'TC') THEN
              CABTYP = 'T'
              AXSTRP = 'NO'
           ELSE
              CABTYP = 'N'
              AXSTRP = 'NO'
           ENDIF
*
           TAB   ='ph05'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='LIMB_LAB    '
           VAL(3)= '1'
           KEY(4)='PWG_LAB     '
           VAL(4)= CWIND
           KEY(5)='SEG_LAB     '
           VAL(5)= '1'
           KEY(6)='CBL_TY      '
           VAL(6)= CABTYP
           NVAL  = 6
           DESC  ='Cable type (Normal/Transposed:  N / T)'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph05'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='LIMB_LAB    '
           VAL(3)= '1'
           KEY(4)='PWG_LAB     '
           VAL(4)= CWIND
           KEY(5)='SEG_LAB     '
           VAL(5)= '1'
           KEY(6)='CBL_STPAX_D '
           VAL(6)= CABTYP
           NVAL  = 6
           DESC='Axial strip in cable:NO; GS(Glued); US(Unglued)'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph05'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='LIMB_LAB    '
           VAL(3)= '1'
           KEY(4)='PWG_LAB     '
           VAL(4)= CWIND
           KEY(5)='SEG_LAB     '
           VAL(5)= '1'
           KEY(6)='COND_MTR    '
           VAL(6)= CONMAT(IWDG)
           NVAL  = 6
           DESC  ='Conductor Material'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph05'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='LIMB_LAB    '
           VAL(3)= '1'
           KEY(4)='PWG_LAB     '
           VAL(4)= CWIND
           KEY(5)='SEG_LAB     '
           VAL(5)= '1'
           KEY(6)='CBL_CV_T    '
           VAL(6)= CHR(COVCBL(IWDG))
           NVAL  = 6
           DESC  ='Cable Covering Thickness, doublesided'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph05'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='LIMB_LAB    '
           VAL(3)= '1'
           KEY(4)='PWG_LAB     '
           VAL(4)= CWIND
           KEY(5)='SEG_LAB     '
           VAL(5)= '1'
           KEY(6)='COND_CV_T   '
           VAL(6)= CHR(COVSTR(IWDG))
           NVAL  = 6
           DESC  ='Conductor Covering Thickness, double-sided'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           IF (CBLTYP(IWDG).EQ. 'TC') THEN
              TAB   ='ph05'
              KEY(1)='ORD_NO_LAB  '
              VAL(1)='?DBTOPKEY?  '
              KEY(2)='ACTP_LAB    '
              VAL(2)= '1'
              KEY(3)='LIMB_LAB    '
              VAL(3)= '1'
              KEY(4)='PWG_LAB     '
              VAL(4)= CWIND
              KEY(5)='SEG_LAB     '
              VAL(5)= '1'
              KEY(6)='COND_VAEP_B '
              VAL(6)= TCEPOX(IWDG)
              NVAL  = 6
              DESC  ='CABLE with Epoxy ( Yes/No)'
              CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
           ENDIF

           TAB   ='ph05'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='LIMB_LAB    '
           VAL(3)= '1'
           KEY(4)='PWG_LAB     '
           VAL(4)= CWIND
           KEY(5)='SEG_LAB     '
           VAL(5)= '1'
           KEY(6)='COND_AX_W   '
           VAL(6)= CHR(HPART(IWDG))
           NVAL  = 6
           DESC  ='Conductor Axial Dimension'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph05'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='LIMB_LAB    '
           VAL(3)= '1'
           KEY(4)='PWG_LAB     '
           VAL(4)= CWIND
           KEY(5)='SEG_LAB     '
           VAL(5)= '1'
           KEY(6)='COND_RA_W   '
           VAL(6)= CHR(BPART(IWDG))
           NVAL  = 6
           DESC  ='Conductor Radial Dimension'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph05'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='LIMB_LAB    '
           VAL(3)= '1'
           KEY(4)='PWG_LAB     '
           VAL(4)= CWIND
           KEY(5)='SEG_LAB     '
           VAL(5)= '1'
           KEY(6)='COND_VA_D   '
           VAL(6)= CONVAR(IWDG)
           NVAL  = 6
           DESC  ='Conductor Varnish designation(YES / NO / ..)'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='co04'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='LIMB_LAB    '
           VAL(3)= '1'
           KEY(4)='PWG_LAB     '
           VAL(4)= CWIND
           KEY(5)='SEG_LAB     '
           VAL(5)= '1'
           KEY(6)='SPC_AX_T    '
           VAL(6)= CHR(HCLAC(IWDG))
           NVAL  = 6
           DESC ='Spacer Thickness between discs/turns; conceptual'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
 299    CONTINUE
        RETURN
        END
