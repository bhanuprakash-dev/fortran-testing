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
*      Subroutine DBPWG
*      ------------------
*
C...   Title:  Writing data on neutral file. Physical winding
*
*      Written: 91-11-12 by B-G Bladh      , SETFO/TS
*      Revised: ....................       , ........
************************************************************************
*
       SUBROUTINE DBPWG(UTF, TOPU, BOTU, NWIND)
*
       DIMENSION
     &           KEY(10), VAL(10), TOPU(*), BOTU(*)
*
       CHARACTER CWIND*12, TOPU*4, BOTU*4,
     &           TAB*4,DESC*60, KEY*12, VAL*12, CHR*12, CHI*12
*
C... TOPU = Nod name for top winding node
C... BOTU = Nod name for bottom winding node
*
       INTEGER   IWDG, NWIND, UTF
*
       REAL      X
*
       include'cominp.h'
*
 200   DO 299 IWDG = 1, NWIND
*
          CWIND = CHI(IWDG)
*
          TAB   ='ph04'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='SEG_SEQN    '
          VAL(5)='1'
          KEY(6)='SEG_TY      '
          VAL(6)= KWITYP(IWDG)
          NVAL  = 6
          DESC  ='Segment Type (Winding type )'
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
          KEY(5)='PWG_IN_DIS  '
          VAL(5)= CHR(BDUCT(IWDG))
          NVAL  = 5
          DESC  ='Conceptual duct width inside  log.winding'
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
          KEY(5)='PHW_OILGR_Q '
          VAL(5)= CHR(XOILGR(IWDG))
          NVAL  = 5
          DESC  ='Oil guiding rings in ph. winding (conceptual)'
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
          KEY(5)='PWG_W       '
          VAL(5)= CHR(BWIND(IWDG))
          NVAL  = 5
          DESC  ='Conceptual winding width, radially'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='ph03'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='PHW_COOLD_Q '
          VAL(5)= CHR(NCDUCT(IWDG))
          NVAL  = 5
          DESC  ='Vertical cooling duct quantity in ph. windings'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_SND     '
          VAL(6)= TOPU(IWDG)
          NVAL  = 6
          DESC  ='Start node (internal)'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_END     '
          VAL(6)= BOTU(IWDG)
          NVAL  = 6
          DESC  ='End node (internal)'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_MAINFNC '
          VAL(6)= KWIFUN(IWDG)
          NVAL  = 6
          DESC  ='Main function'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_STREC_MX'
          VAL(6)= CHR(PRESSM(IWDG))
          NVAL  = 6
          DESC  ='Max compressive stress in logical winding'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='CUR_DNS_MX  '
          VAL(6)= CHR(MXCURD(IWDG))
          NVAL  = 6
          DESC  ='Current density , max'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_STRET_MX'
          VAL(6)= CHR(TENSM(IWDG))
          NVAL  = 6
          DESC  ='Max tensile stress in logical winding'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_AREA_FR '
          VAL(6)= CHR(1/NGROUP(IWDG))
          NVAL  = 6
          DESC  ='Area fraction'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_COND_A  '
          VAL(6)= CHR(ACOND0(IWDG))
          NVAL  = 6
          DESC  ='Conceptual conductor area'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_SPC_FAC '
          VAL(6)= CHR(FILLF(IWDG))
          NVAL  = 6
          DESC  ='Conceptual spacefactor in log.winding'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_TRN_FR  '
          VAL(6)= CHR(FRACT(IWDG))
          NVAL  = 6
          DESC  ='Turn fraction'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_HGT     '
C...      Winding height = limb height - yoke distance ...
C...      Physical winding is assubed to consist only of one logical W. ..
          X = HLIMB - DUYOKE(IWDG) - DLYOKE(IWDG)
          VAL(6)= CHR(X)
          NVAL  = 6
          DESC  ='Conceptual log.winding height'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
          TAB   ='co05'
          KEY(1)='ORD_NO_LAB  '
          VAL(1)='?DBTOPKEY?  '
          KEY(2)='ACTP_LAB    '
          VAL(2)= '1'
          KEY(3)='LIMB_LAB    '
          VAL(3)= '1'
          KEY(4)='PWG_LAB     '
          VAL(4)= CWIND
          KEY(5)='LWG_LAB     '
          VAL(5)= '1'
          KEY(6)='LWG_BTM_DIS '
          VAL(6)= CHR(DLYOKE(IWDG))
          NVAL  = 6
          DESC  ='Conceptual bot. dist.to core or next log.winding'
          CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
*
 299   CONTINUE
*
       RETURN
       END
