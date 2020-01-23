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
*      Subroutine DBGNRL
*      -----------------
*
C...   Title:  Writing data on neutral file.  General data
*
*      Written: 91-11-12 by B-G Bladh      , SETFO/TS
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
************************************************************************
*
       SUBROUTINE DBGNRL(UTF, SCRTMP, GWGLAB)
*
       CHARACTER COOL1*8, COOL2*8,GWGLAB*4,
     &           CX*8, CMAINL*1, CSIDEL*1, VLTST*8,
     &           TAB*4,DESC*60, KEY*12, VAL*12, CHR*12, CHI*12
*
*
           REAL      SCRTMP, X
*
           INTEGER   NVAL, UTF
*
           DIMENSION
     &          KEY(10), VAL(10), GWGLAB(*)
*
           include'cominp.h'
*
C... Design series, tank type
*
           TAB   ='ph25'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TNK_TY      '
           VAL(2)= CKTANK
           NVAL  = 2
           DESC  ='Tank type'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
C... Distances around active part
*
           TAB   ='co01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='CO_COV_DIS  '
           VAL(2)= CHR(DCOCOV)
           NVAL  = 2
           DESC  ='Conceptual distance core - cover'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='co01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TNK_WI_DIS  '
           VAL(2)= CHR(DWITA)
           NVAL  = 2
           DESC  ='Conceptual distance winding - tank'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='co01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TNK_EXTEN   '
           VAL(2)= CHR(EXTANK)
           NVAL  = 2
           DESC  ='Conceptual tank extra extension'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
C... Tank dimensions
*
           TAB   ='ph25'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TNK_HGT     '
           VAL(2)= CHR(HTANK)
           NVAL  = 2
           DESC  ='Tank height'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph25'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TNK_L       '
           VAL(2)= CHR(LTANK)
           NVAL  = 2
           DESC  ='Tank length'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph25'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TNK_W       '
           VAL(2)= CHR(BTANK)
           NVAL  = 2
           DESC  ='Tank width'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph18'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TC_LAB      '
           VAL(2)='1'
           KEY(3)='TC_TY       '
           VAL(3)= OLTC
           NVAL  = 3
           DESC  ='Model designation'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
C... Cooling equipment, COOLING CLASSES (ONAN, OFAF...)
*
           SCRTMP = 1.
           IF (KCOOL.EQ.'ONAF'.OR.KCOOL.EQ.'ONAN/F') THEN
              SCRTMP= FONAN
              IF (SCRTMP.GT.3.) SCRTMP=SCRTMP/100.
*
              IF ( SCRTMP .LT. 1.) THEN
                 COOL1 = 'ONAF'
                 COOL2 = 'ONAN'
              ELSE
                 COOL1 = 'ONAN'
                 COOL2 = 'ONAF'
              ENDIF
*
              TAB   ='cu06'
              KEY(1)='ORD_NO_LAB  '
              VAL(1)='?DBTOPKEY?  '
              KEY(2)='COOL_ST_LAB '
              VAL(2)='1'
              KEY(3)='COOL_CLASS  '
              VAL(3)= COOL1
              KEY(4)='TEMP_TOPR_MX'
              VAL(4)= CHR(TTOILM)
              KEY(5)='TEMP_WIMR_MX'
              VAL(5)= CHR(TWINDM)
              NVAL  = 5
              DESC  ='Cooling class / Top Oil / Mean Oil'
              CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
              TAB   ='cu06'
              KEY(1)='ORD_NO_LAB  '
              VAL(1)='?DBTOPKEY?  '
              KEY(2)='COOL_ST_LAB '
              VAL(2)='2'
              KEY(3)='COOL_CLASS  '
              VAL(3)= COOL2
              KEY(4)='TEMP_TOPR_MX'
              VAL(4)= CHR(TTOILM)
              KEY(5)='TEMP_WIMR_MX'
              VAL(5)= CHR(TWINDM)
              NVAL  = 5
              DESC  ='Cooling class / Top Oil / Mean Oil'
              CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           ELSE
              TAB   ='cu06'
              KEY(1)='ORD_NO_LAB  '
              VAL(1)='?DBTOPKEY?  '
              KEY(2)='COOL_ST_LAB '
              VAL(2)='1'
              KEY(3)='COOL_CLASS  '
              VAL(3)= KCOOL
              KEY(4)='TEMP_TOPR_MX'
              VAL(4)= CHR(TTOILM)
              KEY(5)='TEMP_WIMR_MX'
              VAL(5)= CHR(TWINDM)
              NVAL  = 5
              DESC  ='Cooling class / Top Oil / Mean Oil'
              CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
           ENDIF
*
C... Tank Screens and Shieldings
*
           TAB   ='ph25'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TNK_SHLD_TY '
           CX = 'N'
           IF (ALSHLD(1:1).EQ.'Y') CX = 'C'
           VAL(2)= CX
           NVAL  = 2
           DESC  ='Tank Field Screens   Type'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph25'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TNK_SOUS_TY '
           CX    = 'Y'
           IF(SNDSCR(1:1) .EQ.'N') CX = 'N'
           VAL(2)= CX
           NVAL  = 2
           DESC  ='Tank; Sound Shields Type or No or Yes '
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
C... Some electrical key data; Loss evaluations
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='OVERMAG_MX  '
           VAL(2)= CHR(UMAXPU)
           NVAL  = 2
           DESC  ='Max. overmagnetization (%)'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)='1'
           KEY(3)='REG_CFR_VFR '
           VAL(3)= TYPREG
           NVAL  = 3
           DESC  ='CFR/VFR-Regulation according to T146'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='LEVL_NOLOAD '
           VAL(2)= CHR(EVALP0)
           NVAL  = 2
           DESC  ='No load loss evaluation (local currency/kW)'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='LEVL_LOAD   '
           VAL(2)= CHR(EVALPK)
           NVAL  = 2
           DESC  ='Load loss evaluation    (local currency/kW)'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='FRQ         '
           VAL(2)= CHR(FREQ)
           NVAL  = 2
           DESC  ='Network frequency'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ORD_PROJECT '
           VAL(2)= IDENT
           NVAL  = 2
           DESC  ='Project'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='PH_Q        '
           VAL(2)= CHR(NPHAS)
           NVAL  = 2
           DESC  ='Number of phases in each tank (1 / 3)'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
C... Limits for optimization..............................
*
           TAB   ='co01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='FLD_MX      '
           VAL(2)= CHR(BLTOPM)
           NVAL  = 2
           DESC  ='Max flux density for optimization    '
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='co01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='LIMB_H_MX   '
           VAL(2)= CHR(HLIMBM)
           NVAL  = 2
           DESC  ='Max core limb height for optimization'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='co01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='LIMB_H_MI   '
           VAL(2)= CHR(HLIMBN)
           NVAL  = 2
           DESC  ='Min core limb height for optimization'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='co01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='CO_DIA_MX   '
           VAL(2)= CHR(DLMBMA)
           NVAL  = 2
           DESC  ='Max core diameter for optimization'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='co01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='CO_DIA_MI   '
           VAL(2)= CHR(DLMBMI)
           NVAL  = 2
           DESC  ='Min core diameter for optimization'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TRP_M_MX    '
           VAL(2)= CHR(GTRPM )
           NVAL  = 2
           DESC  ='Max transport mass '
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='SOUL_MX     '
           VAL(2)= CHR(SOUNDM)
           NVAL  = 2
           DESC  ='Max Sound level'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TNK_H_MX    '
           VAL(2)= CHR(HTANKM)
           NVAL  = 2
           DESC  ='Max Tank height'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TANK_W_MX   '
           VAL(2)= CHR(BTANKM)
           NVAL  = 2
           DESC  ='Max Tank width'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TNK_L_MX    '
           VAL(2)= CHR(RLTANM)
           NVAL  = 2
           DESC  ='Max Tank length'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='LOSS_LOAD_MX'
           VAL(2)= CHR(MAXPK)
           NVAL  = 2
           DESC  ='Max Load losses'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='LOSS_NOLOADM'
           VAL(2)= CHR(MAXP0)
           NVAL  = 2
           DESC  ='Max No load losses'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
C... Core data...........................................
*
       IF (UTURN .GT. 5) THEN
              TAB   ='co03'
              KEY(1)='ORD_NO_LAB  '
              VAL(1)='?DBTOPKEY?  '
              KEY(2)='ACTP_LAB    '
              VAL(2)= '1'
              KEY(3)='CO_LIMB_A   '
              X     = UTURN/222.14415*1E4
C...          222.14415 = 100.*PI/SQRT(2);   X i cm2 .........
              VAL(3)= CHR(X)
              NVAL  = 3
              DESC  ='Conceptual core cross section area (Active Ð)'
              CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
           ENDIF
*
           TAB   ='co03'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_LIMB_DIA '
           VAL(3)= CHR(DCORE)
           NVAL  = 3
           DESC  ='Core Diameter'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='co03'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_PH_PH_DIS'
           VAL(3)= CHR(DPHAS)
           NVAL  = 3
           DESC  ='Conceptual distance between phases'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='co03'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_SL_PH_DIS'
           VAL(3)= CHR(DPHSL)
           NVAL  = 3
           DESC  ='Conceptual distance between phase - sidelimb'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='co03'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_LIMB_HGT '
           VAL(3)= CHR(HLIMB)
           NVAL  = 3
           DESC  ='Core Limbheight'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_JNT_TY   '
           CX    = 'STEPLAP'
           IF(STEPLA(1:1) .EQ.'N') CX = 'NORMAL'
           VAL(3)= CX
           NVAL  = 3
           DESC  ='Core Lamination Joint Type'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_YOHGHTRED'
           VAL(3)= CHR( -1.*HYOKAD)
           NVAL  = 3
           DESC  ='Core Yoke height reduction'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_LAM_D    '
           VAL(3)= COREMA
           NVAL  = 3
           DESC  ='Core Lamination Designation'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_STRP_TY  '
           VAL(3)= KBAND
           NVAL  = 3
           DESC  ='Core Strapping Type'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_SER      '
           VAL(3)= CORESE
           NVAL  = 3
           DESC  ='Core design series'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_BLD_TY   '
           VAL(3)= BLDING
           NVAL  = 3
           DESC  ='Core Lamination Blading'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_AS_FC_TOT'
           VAL(3)= CHR(QMONT)
           NVAL  = 3
           DESC  ='Core, Total assembly force'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
C...    CORE TYPE conversion .....................................
           IF (CORTYP.EQ.'D   ') THEN
              CMAINL ='2'
              CSIDEL ='0'
           ELSE IF (CORTYP.EQ.'DY  ') THEN
              CMAINL ='2'
              CSIDEL ='2'
           ELSE IF (CORTYP.EQ.'EY  ') THEN
              CMAINL ='1'
              CSIDEL ='2'
           ELSE IF (CORTYP.EQ.'TY  ') THEN
              CMAINL ='3'
              CSIDEL ='2'
           ELSE IF (CORTYP.EQ.'TY-1') THEN
              CMAINL ='3'
              CSIDEL ='2'
           ELSE IF (CORTYP.EQ.'TY-3') THEN
              CMAINL ='3'
              CSIDEL ='2'
           ELSE
C..           DEFAULT "T":
              CMAINL ='3'
              CSIDEL ='0'
           ENDIF

           TAB   ='ph01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_SLIMB_Q  '
           VAL(3)= CSIDEL
           NVAL  = 3
           DESC  ='Core, number of sidelimbs (0 if not appl.)'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='ph01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='CO_MLIMB_Q  '
           VAL(3)= CMAINL
           NVAL  = 3
           DESC  ='Core, number of main limbs'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
C...    END CORE TYPE conversion  ................................

           TAB   ='ph01'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='LKFLPL_T    '
           VAL(3)= CHR(TWSUP)
           NVAL  = 3
           DESC  ='Leakage flux plate thickness'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
*
           TAB   ='co07'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='TWOW_LAB    '
           VAL(3)= '1'
           KEY(4)='GWG_FST_LAB '
           VAL(4)= GWGLAB(1)
           KEY(5)='GWG_SEC_LAB '
           VAL(5)= GWGLAB(2)
           KEY(6)='POW_BASE    '
           VAL(6)= CHR( SRATE(1) )
           KEY(7)='POW_LOAD    '
           VAL(7)= CHR( SRATE(1))
           NVAL  = 7
           DESC  ='Two-winding load case data'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='cu08'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='TWOW_LAB    '
           VAL(3)='1'
           KEY(4)='SCIMPED     '
           VAL(4)= CHR (USHORE)
           NVAL  = 4
           DESC  ='Short circuit impedance'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='pr03'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='ACTP_LAB    '
           VAL(2)= '1'
           KEY(3)='GWG_EX_LAB  '
           VAL(3)= GWGLAB(1)
           KEY(4)='VLT_EX_PRC  '
           VAL(4)='100'
           KEY(5)='CO_MLIMB_FLD'
           VAL(5)= CHR(BLIMB)
           NVAL  = 5
           DESC  ='Core, flux density in main limb'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           TAB   ='pr10'
           KEY(1)='ORD_NO_LAB  '
           VAL(1)='?DBTOPKEY?  '
           KEY(2)='TWOW_LAB    '
           VAL(2)='1'
           KEY(3)='TEMP_TOPR_CA'
           VAL(3)= CHR(TOPDTO)
           NVAL  = 3
           DESC  ='Top oil temperature rise'
           CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
           RETURN
           END
