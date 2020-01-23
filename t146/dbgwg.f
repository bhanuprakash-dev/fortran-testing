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
*      Subroutine DBGWG
*      ------------------
*
C...   Title:  Writing data on neutral file. General Winding data
*
*      Written: 91-11-12 by B-G Bladh      , SETFO/TS
*      Revised: ....................       , ........
************************************************************************
*
       SUBROUTINE DBGWG(UTF,SCRTMP, NTERM, GWGLAB, EXTNOD)
*
       include'cominp.h'
*
       INTEGER
     &       NTERM, J, I, UTF, NVAL
*
       REAL
     &       SCRTMP
*
       DIMENSION
     &       GWGLAB(18), KEY(10), VAL(10), EXTNOD(18)
*
       CHARACTER
     &       GWGLAB*4, TAB*4,DESC*60, KEY*12, VAL*12, CHR*12, CHI*12,
     &       VLTST*8, EXTNOD*4
*
       J     = 0
*
C... GENERAL WINDING DATA .....................................
*
 300   DO 399 K= 1, NTERM
*
          TAB   ='cu02'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='GWG_LAB     '
          VAL(3)= GWGLAB(K)
          KEY(4)='GWG_VG      '
          VAL(4)= KCON(K)
          NVAL  = 4
          DESC  ='Vector group'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='cu02'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='GWG_LAB     '
          VAL(3)= GWGLAB(K)
          KEY(4)='REG1_STNE_Q '
          VAL(4)= CHR(NMSTEP(K))
          NVAL  = 4
          DESC  ='Number of decreasing steps (minus), reg. 1'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='cu02'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='GWG_LAB     '
          VAL(3)= GWGLAB(K)
          KEY(4)='REG1_STPO_Q '
          VAL(4)= CHR(NPSTEP(K))
          NVAL  = 4
          DESC  ='Number of increasing steps (plus), reg. 1'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='cu02'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='GWG_LAB     '
          VAL(3)= GWGLAB(K)
          KEY(4)='REG1_STSIZE '
          VAL(4)= CHR(PUSTEP(K))
          NVAL  = 4
          DESC  ='Step size , percent, reg. 1'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='cu02'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='GWG_LAB     '
          VAL(3)= GWGLAB(K)
          KEY(4)='GWG_RAT_P   '
          VAL(4)= CHR(SRATE(K))
          NVAL  = 4
          DESC  ='Rated Power'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='cu02'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='GWG_LAB     '
          VAL(3)= GWGLAB(K)
          KEY(4)='GWG_VLT_R   '
          VAL(4)= CHR(UNLINE(K))
          NVAL  = 4
          DESC  ='Rated voltage'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='cu02'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='GWG_LAB     '
          VAL(3)= GWGLAB(K)
          KEY(4)='GWG_SC_P    '
          VAL(4)= CHR(SLINE(K))
          NVAL  = 4
          DESC  ='Short circuit power at rated voltage'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)

*
C... Search for regulated winding
*
          IF (KTYPRW(K) .NE.'  ' ) THEN
             J = J+1
             TAB   ='ph18'
             KEY(1)='ORD_NO_LAB  '
             VAL(1)='?DBTOPKEY?  '
             KEY(2)='TC_LAB      '
             WRITE(VAL(2), 80) J
  80         FORMAT (I1,7X)
             KEY(3)='TC_CONN_TY  '
             VAL(3)= KTYPRW(K)
             NVAL  = 3
             DESC  ='Tap changer connection type (PM/L/CF)'
             CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
          ENDIF
*
          TAB   ='cu07'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='COOL_ST_LAB '
          VAL(3)='1'
          KEY(4)='GWG_LAB     '
          VAL(4)= GWGLAB(K)
          KEY(5)='POW_COOLST  '
          VAL(5)= CHR( SRATE(K) )
          NVAL  = 5
          DESC  ='Power in different cooling steps'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
C... Two cooling steps defined :
*
          IF (ABS(SCRTMP-1.) .GT. 1.E-4) THEN
             TAB   ='cu07'
             KEY(1)='ORD_NO_LAB  '
             VAL(1)='?DBTOPKEY?  '
             KEY(2)='ACTP_LAB    '
             VAL(2)= '1'
             KEY(3)='COOL_ST_LAB '
             VAL(3)= '2'
             KEY(4)='GWG_LAB     '
             VAL(4)= GWGLAB(K)
             KEY(5)='POW_COOLST  '
             VAL(5)= CHR ( SCRTMP * SRATE(K))
             NVAL  = 5
             DESC  ='Power in different cooling steps'
             CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
          ENDIF
*
          TAB   ='cu11'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='TST_VLT_LAB '
          WRITE(VLTST, 81) K
 81       FORMAT (I1,'S')
          VAL(2)= VLTST
          KEY(3)='TST_VLTI_LEV'
          VAL(3)= CHR(USURG(K))
          KEY(4)='EXTNODE     '
          VAL(4)= EXTNOD(K)
          NVAL  = 4
          DESC  ='Test voltage, peak value'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
 399      CONTINUE
*
        RETURN
        END
