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
*      Subroutine DBPREP
*      ------------------
*
C...   Title:  Writing data on neutral file. Preparation and output call
*
*      Written: 91-11-29 by B-G Bladh      , SETFO/TS
*      Revised: ....................       , ........
************************************************************************
*
       SUBROUTINE DBPREP
*
*
       INTEGER
     &       NWIND,NTERM, J,I,UTF,
     &       NVAL, ICHK
*
       REAL
     &       SCRTMP
*
       DIMENSION
     &       TOPU(9),BOTU(9),GWGLAB(18),EXTNOD(18),INTNOD(18),
     &       KEY(10), VAL(10)
*
       CHARACTER
     &       TOPU*4,BOTU*4,GWGLAB*4,EXTNOD*4,INTNOD*4,
     &       KEY*12, VAL*12, DESC*60, TAB*4
*
       include'cominp.h'
*
C... Preparation...................................................
*
*
C... Output file for "NEUTRAL FILE DATA"
*
       UTF = 16
*
       OPEN(UNIT=UTF,ACCESS='SEQUENTIAL',FORM='FORMATTED',IOSTAT=ICHK)
*
       IF (ICHK.EQ.0) THEN
          REWIND(UTF)
*
          NWIND = NINT(RWILI)
*
C... Conunt terminals
*
          NTERM = 0
*
          DO 199 I = 1, 4
 199         IF (TNODE(I) .NE. '  ') NTERM = NTERM + 1
*
C... Make new node designations
*
          CALL NODCON(TNODE,WNOD1,WNOD2,NWIND,NTERM,
     &         TOPU, BOTU, GWGLAB, EXTNOD, INTNOD, NEXT)
*
C... Preparation  END .............................................
*
C... Connect EXTERNAL and INTERNAL nodes.
*
          DO 299 I= 1, NEXT
*
             TAB   ='co15'
             KEY(1)='ORD_NO_LAB  '
             VAL(1)='?DBTOPKEY?  '
             KEY(2)='EXTNODE     '
             VAL(2)= EXTNOD(I)
             KEY(3)='NODE        '
             VAL(3)= INTNOD(I)
             KEY(4)='ACTP_LAB    '
             VAL(4)='1'
             KEY(5)='GWG_LAB     '
             VAL(5)= GWGLAB(I)
             NVAL  = 5
             DESC  ='External - Internal node connection'
             CALL DBPOST(UTF, TAB, DESC, NVAL, KEY, VAL)
 299      CONTINUE
*
          CALL DBGNRL(UTF,SCRTMP,GWGLAB)
*
          CALL DBGWG(UTF,SCRTMP, NTERM, GWGLAB, EXTNOD)
*
          CALL DBPWG(UTF, TOPU, BOTU, NWIND)
*
          CALL DBWSEG(UTF, NWIND)
          CLOSE(UTF)

       ELSE
          WRITE(*,*)
     &    '** Opening of output file for NEUTRAL FILE failed'
       ENDIF
*
       RETURN
       END
