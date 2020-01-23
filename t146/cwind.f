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
*      Subroutine CWIND
*      ----------------
*
C...   Calculating the costs for a winding.
*
*      Written: XX-XX-XX by A.N. Other      , XXXX
*      Revised: 90-01-09 by Per S|derberg   , SETFO/IK
*
************************************************************************
*
C*..* CWIND
CCWIND
C     *******************    NYTT    *************************
C     ALL PARAMETERS PREVIOUSLY STARTING WITH THE PREFIX 'GSN'
C     HAVE BEEN CHANGED TO 'GS' TO ACCOMODATE THE FORTRAN 77
C     NAMING REQUIREMENT OF NOT MORE THAN 6 CHARACTERS.
C     ********************************************************
C
       SUBROUTINE CWIND (BBPRNT,JFC,NUMBER,NCOND,LTYPE,RGUID,GS11,
     *                   GS121,GS122,GS131,GS132,GS133,GS134,
     *                   GN1351,GN1352,GS136,APART,TCOV1,TCOV2,
     *                   CCOV1,CCOV2,NLIMB,DWIND,WWIND,WCLS,WDUCT,
     *                   NTAPP,ACOND,XBUNCH,ABUNCH,ZCOOL,NOUCO,
     *                   ZPART,BBEXAC,BPART,ZCABEL,ZCOIAR,ZLAG,BBLNX,
     *                   RPRTRR,ZWIND,IPISOL,HPART,CBUNCH,CONVAR,TCEPOX)
*-------------------------------------------------------------------------
* NUMBER  LINDNINGSUMMER (1 N#RMAST K#RNAN)
* NCOND   LEDARTYP       1= KOPPAR
*                        2= ALUMINIUM
*                        3= TRANSPONERAD KABEL
* LTYPE   LINDNINGSTYP   1 = S     6 = SLS
*                        2 = L     7 = SLL
*                        3 = CD    8 = BND
*                        4 = SD    9 = LN, LNX
*                        5 = D2
* RGUID   ANTAL OLJESTYRSK#RMAR P* IN-, OCH UTSIDA AV LINDNING
* GS11    NETTOMASSA OISOLERAD LEDARE                      (KG)
* GS121        "     PARTISOLATION(LTYPE .LE. 7)           (KG)
*         (PAPPER, MYLARPRESSPAN ELLER LIMREMSA)
*              "     ISOL.STRIP MELLAN PARTER   LTYPE = 9  (KG)
*              "     ISOLATION MELLAN BAND      LTYPE = 8  (KG)
* GS122        "     SPINNPAPPER KABELISOLATION LTYPE.NE.8 (KG)
* GS131        "     ISOLATIONDETALJER I KANAL  LTYPE.LE.7 (KG)
*              "     EPOXYSLANG I KANAL         LTYPE = 8  (KG)
*              "     RIBBMATTA I KANAL          LTYPE = 9  (KG)
* GS132        "     ISOLATIONSDETALJER MOT OK             (KG)
* GS133        "     OLJESTYRSK#RMAR                       (KG)
* GS134        "     KLACKBAND I KYLKANAL       LTYPE.LE.7 (KG)
*              "     RIBBMATTA I KYLKANAL       LTYPE.GE.8 (KG)
* GN1351       "     RAK KLACK                  LTYPE.LE.7 (KG)
*              "     LAGERISOLATION             LTYPE = 9  (KG)
* GN1352       "     SNED KLACK                 LTYPE.LE.7 (KG)
*              "     MALLCYLINDER               LTYPE = 8  (KG)
* GS136        "     L*SCYLINDER                LTYPE.LE.7 (KG)
*              "     KANTST@D                   LTYPE = 9  (KG)
* APART   LEDARPARTENS AREA                                (MM2)
* TCOV1   TJOCKLEK PARTSPINNING DUBBELSIDIG     LTYPE.NE.8 (MM)
*         ISOLATIONSTJOCKLEK MELLAN FOLIEVARV   LTYPE = 8  (MM)
* TCOV2   TJOCKLEK KABELSPINNING                LTYPE.LE.7 (MM)
* CCOV1   PARTISOLERINGENS HALVA OMKRETS              "    (MM)
* CCOV2   KABELISOLERINGENS HALVA OMKRETS             "    (MM)
* NLIMB   ANTAL LINDADE BEN
* DWIND   MEDELDIAMETER LINDNING                           (MM)
* WWIND   TOTAL LINDNINGSTJOCKLEK (RR)                     (MM)
* WCLS    KLACKBANDSBREDD                                  (MM)
* WDUCT   TOTAL KANALVIDD INNANF@R LINDNING                (MM)
* NTAPP   ANTAL LINDNINGSUTTAG ( F@R ALLA BEN )
* ACOND   AREA F@R KABEL                                   (MM2)
* XBUNCH  L#NGD AV LINDNINGSKNIPPET (ALLA BEN)             ( M ) *OBS*
* CBUNCH  L#NGD AV KABEL F@R ALLA BEN                      ( M ) *OBS*
* ABUNCH  LINDNINGSKNIPPETS AREA                           (MM2)
* ZCOOL   ANTAL VERTIKALA KYLKANALER
* NOUCO   ANTAL YTTRE @VERKOPPLINGAR (STAB.SKIV, ALLA BEN)
* ZPART   ANTAL PARTER I LINDNINGSKNIPPET
* BBEXAC  .TRUE. UNDER EXAKT BER#KNING
* BPART   BANDTJOCKLEK F@R BANDLINDNING         LTYPE = 8  (MM)
* ZCABEL  ANTAL KABLAR I LINDN.KNIPPET AXIELL   LTYPE = 9
* ZCOIAR  ANTAL SPOLAR AXIELLT                  LTYPE.GE.8
* ZLAG    ANTAL LAGER                           LTYPE = 9
* BBLNX   .TRUE.  F@R LNX - LINDNING            LTYPE = 9
* RPRTRR  ANTAL PARTER RADIELLT I KABELN
* ZWIND   TOTALT LINDNINGSVARVTAL
* IPISOL  PARTISOLATION  1= PAPPER
*                        2= MYLARPRESSPAN
*                        3= LIMREMSA
* HPART   LEDARPARTENS AXIELLA M*TT (H@JD)     (MM)
*---------------------------------------------------------------
*      FIXED VALUES ARE SAVED DURING OPTIMIZATION IN THE MAIN LINK
*      IF ROUTINE CWIND IS LINKED OUT COMMON AREA /SCWIND/ MUST BE
*      SAVED IN THE MAIN LINK
       COMMON /SCWIND/ LC(10)
*
       LOGICAL BBEXAC,BBLNX,BBPRNT
*
       CHARACTER SP1*7,SP*10,CONVAR*4,TCEPOX*4
       DATA SP1/'       '/,SP/'          '/
***************
       IX=19*(NUMBER-1)

***************
       IF(.NOT.BBPRNT) GO TO 101
       WRITE(JFC,*) '===== INPUT DATA TO CWIND ====='
       WRITE(JFC,*) SP1,'NUMBER',SP,'NCOND ',SP,'LTYPE ',SP,'RGUID '
       WRITE(JFC,*) NUMBER,NCOND,LTYPE,RGUID
       WRITE(JFC,*) SP1,'GS11  ',SP,'GS121 ',SP,'GS122 ',SP,'GS131 '
       WRITE(JFC,*) GS11,GS121,GS122,GS131
       WRITE(JFC,*) SP1,'GS132 ',SP,'GS133 ',SP,'GS134 ',SP,'GN1351'
       WRITE(JFC,*) GS132,GS133,GS134,GN1351
       WRITE(JFC,*) SP1,'GN1352',SP,'GS136 ',SP,'APART ',SP,'TCOV1 '
       WRITE(JFC,*) GN1352,GS136,APART,TCOV1
       WRITE(JFC,*) SP1,'TCON2 ',SP,'CCOV1 ',SP,'CCOV2 ',SP,'NLIMB '
       WRITE(JFC,*) TCOV2,CCOV1,CCOV2,NLIMB
       WRITE(JFC,*) SP1,'DWIND ',SP,'WWIND ',SP,'WCLS  ',SP,'WDUCT '
       WRITE(JFC,*) DWIND,WWIND,WCLS,WDUCT
       WRITE(JFC,*) SP1,'NTAPP ',SP,'ACOND ',SP,'XBUNCH',SP,'ABUNCH'
       WRITE(JFC,*) NTAPP,ACOND,XBUNCH,ABUNCH
       WRITE(JFC,*) SP1,'ZCOOL ',SP,'NOUCO ',SP,'ZPART ',SP,'BBEXAC'
       WRITE(JFC,*) ZCOOL,NOUCO,ZPART,BBEXAC
       WRITE(JFC,*) SP1,'BPART ',SP,'ZCABEL',SP,'ZCOIAR',SP,'ZLAG  '
       WRITE(JFC,*) BPART,ZCABEL,ZCOIAR,ZLAG
       WRITE(JFC,*) SP1,'BBLNX ',SP,'RPRTRR',SP,'ZWIND ',SP,'IPISOL'
       WRITE(JFC,*) BBLNX,RPRTRR,ZWIND,IPISOL
       WRITE(JFC,*) SP1,'HPART ',SP,'CBUNCH'
       WRITE(JFC,*) HPART,CBUNCH
       WRITE(JFC,*) SP1,'LC(1)-LC(4)'
       WRITE(JFC,*) (LC(I),I=1,4)
       WRITE(JFC,*) SP1,'LC(5)-LC(8)'
       WRITE(JFC,*) (LC(I),I=5,8)
 101   CONTINUE
***************
       IF (BBEXAC) THEN
*
*... Setting the correct BLOCK number in cost file.
*
          IF (CONVAR.EQ.'YES ') THEN
             LC(1) = 24
             IF (APART.GT.20.) LC(1) = 34
          ELSE
             LC(1) = 1
             IF (APART.GT.20.) LC(1) = 2
          ENDIF
          LC(2) = 3
          IF (APART.GT.30.) LC(2) = 4
          IF (TCEPOX.EQ.'YES ') THEN
             LC(3) = 21
             IF (ACOND.GT.300.) LC(3) = 38
          ELSE
             LC(3) = 5
             IF (ACOND.GT.300.) LC(3) = 6
          ENDIF
          LC(4) = 10
          IF (WWIND.GT.26.) LC(4) = 11
          LC(5) = 0
          HLP = DWIND+WWIND
          IF ((LTYPE.EQ.3.OR.LTYPE.EQ.4).AND.HLP.GE.700..AND.
     &       HLP.LE.1400.) LC(5) = 1
       ENDIF
***************
***    LEDARMATERIAL
       IF(LTYPE.EQ.8) GO TO 17
***    KOPPAR
       GOTO (11,13,15),NCOND
 11    CALL COST (3,LC(1),IX+1,GS11,1.,APART,0.,1.,1.,1.,1.,1.)
       GOTO 18
*
***    ALUMINIUM
 13    CALL COST (3,LC(2),IX+2,GS11,1.,APART,0.,1.,1.,1.,1.,1.)
       GOTO 18
*
***    TRANSPONERAD KABEL
 15    CALL COST (3,LC(3),IX+3,GS11,1.,ACOND,0.,1.,1.,1.,1.,1.)
       GOTO 20
*
***    BANDLINDNING  LEDARE
 17    IF(NCOND.EQ.2) GO TO 172
 171   CALL COST(3,5,IX+1,GS11,1.,BPART,0.,1.,1.,1.,1.,1.)
       GO TO 18
 172    CALL COST(3,6,IX+2,GS11,1.,BPART,0.,1.,1.,1.,1.,1.)
*
*
   18 CONTINUE
*
*... SPINNPAPPER
      GO TO (24,24,24,24,24,24,24,25,26),LTYPE
 24   IU = 7
      IF(LTYPE.EQ.2.OR.LTYPE.GE.6) IU = 8
*
      GO TO (241,242,242),IPISOL
*
 241  IF(ABS(GS121).LT.1.E-6) GOTO 19
*     PAPPER SOM PARTISOLATION
      CALL COST (4,IU,IX+4,GS121,1.,1.,0.,1.,GS121,TCOV1,CCOV1,1.)
   19 CALL COST (3,IU,IX+4,GS122,1.,1.,0.,1.,GS122,TCOV2,CCOV2,1.)
      GO TO 249
*
*       MYLARPRESSPAN ELLER LIMREMSA SOM PARTISOLATION
CC*BORT
CC*242  CALL COST(4,126+IPISOL,IX+4,GS121,1.,1.,0.,1.,CBUNCH,HPART,1.,1.)
CC*   CALL COST(3,IU,IX+4,GS122,1.,1.,0.,1.,0.,0.,0.,0.)
CC*NYTT
 242  CALL COST(4,126+IPISOL,IX+4,GS121,1.,1.,0.,1.,0.,0.,0.,0.)
      CALL COST(3,IU,IX+4,GS122,1.,1.,0.,1.,GS122,TCOV2,CCOV2,1.)
CC*
*          PART OCH KABELISOLATION UTF@RES I ETT ARBETSMOMENT
*
 249    GO TO 20
*
*       BANDLINDNING
 25     CALL COST(3,7,IX+4,GS121,1.,TCOV1,0.,1.,GS121,1.,1.,1.)
        GO TO 20
*
*       FLER-LAGERLINDNING
 26     ACM2 = DWIND*3.1416E-3
        ACM4 = 0.38 + 0.37*(RPRTRR-1.)
*        ISOLATION AV KABEL (ELLER PART VID ICKE SAMMSPINNING)
        CALL COST(4,8,IX+4,GS122,1.,1.,0.,1.,ZWIND,ACM2,ZCABEL,ACM4)
*         ISOLATIONSREMSA MELLAN PARTER VID SAMMSPINNING
        CALL COST(3,9,IX+4,GS121,1.,1.,0.,1.,1.,1.,1.,1.)
*
*----------------------------------------------------------------------
*... ISOLATIONSDETALJER KANAL
   20 CONTINUE
      XLIMB=NLIMB
      GS=GS131/XLIMB
      GO TO (201,202,201,201,201,202,202,203,204),LTYPE
 201  CALL COST (4,17,IX+5,GS131,DWIND,1.,0.,1.,XLIMB,GS,1.,1.)
      GO TO 209
 202  CALL COST (4,18,IX+5,GS131,DWIND,1.,0.,1.,XLIMB,GS,1.,1.)
      GO TO 209
*     BANDLINDNING
*       EPOXY-SLANG I HUVUDKANAL
 203  CALL COST(4,11,IX+5,GS131,1.,1.,0.,1.,XLIMB,1.,1.,1.)
*       MALLCYLINDER
      CALL COST(4,23,IX+5,GN1352,1.,1.,0.,1.,1.,1.,1.,1.)
      GO TO 209
*     FLER-LAGERLINDING
 204  CALL COST(4,12,IX+5,GS131,1.,1.,0.,1.,1.,1.,1.,1.)
 209  CONTINUE
*
*
***    ISOLATIONSDETALJER MOT OK
       GS=GS132/XLIMB
       GOTO (21,22,21,21,21,22,22,221,221),LTYPE
   21  CALL COST (4,9,IX+6,GS132,DWIND,1.,0.,1.,XLIMB,GS,1.,1.)
       GOTO 29
 22    CALL COST (4,LC(4),IX+6,GS132,DWIND,1.,0.,1.,XLIMB,GS,1.,1.)
       GOTO 29
 221   GS = GS132+ GS136
       CALL COST(4,13,IX+6,GS,1.,1.,0.,1.,GS,1.,1.,1.)
*
***    STYRSK#RMAR
   29 CONTINUE
      IF(ABS(GS133).LT.1.E-6) GOTO 30
      CALL COST (4,12,IX+7,GS133,1.,1.,0.,1.,GS133,1.,1.,1.)
*
*... KLACKBAND
   30 CONTINUE
      IF(ABS(GS134).LT.1.E-6) GOTO 31
      GO TO (301,301,301,301,301,301,301,302,302),LTYPE
 301  CALL COST (4,13,IX+8,GS134,1.,1.,0.,1.,GS134,WCLS,1.,1.)
       GO TO 309
*      RIBBMATTA I KYLKANAL
 302   ACM4 = 1. + 10./(ZCOOL+ZCOIAR+XLIMB)
       CALL COST(4,14,IX+8,GS134,1.,1.,0.,1.,ZCOOL,ZCOIAR,XLIMB,ACM4)
 309   CONTINUE
*
*... KLACKAR
   31 GO TO (32,32,32,32,32,32,32,34,33),LTYPE
   32 CALL COST (4,14,IX+9,GN1351,1.,1.,0.,1.,GN1351,1.,1.,1.)
  
      CALL COST (4,15,IX+10,GN1352,1.,1.,0.,1.,GN1352,1.,1.,1.)
      GO TO 34
*     LAGER-ISOLATION LN-LINDINGAR
   33 CALL COST(4,10,IX+9,GN1351,1.,1.,0.,1.,1.,1.,1.,1.)
   34 CONTINUE
*
*
*... L*SCYLINDRAR
      GS=GS136/XLIMB
      IF(ABS(GS136).LT.1.E-6 .OR. LTYPE.GT.7) GOTO 40
      CALL COST (4,16,IX+11,GS136,DWIND,1.,0.,1.,XLIMB,GS,1.,1.)
*
   40 CALL COST (3,0,IX+12,0.,1.,1.,0.,1.,1.,1.,1.,1.)
*----------------------------------------------------------------
      XCOOLD=ZCOOL+1.
*  XOGUR= ANTAL SEGMENT P* LINDNINGENS ENA SIDA (30% KLACKT#CKNING, KL=38)
      XOGUR=RGUID/2. * DWIND * 0.0248
      GO TO(411,412,413,414,415,416,417,42,43),LTYPE
CC*BORT
CC*411  IA= 19 ;  IB= 138 ; IC= 25 ; ID = 22
CC*NYTT
 411  IA= 19
      IB= 138
      IC= 25
      ID= 22
CC*
      GO TO 41
CC*BORT
CC*412  IA= 20 ;  IB= 139 ; IC= 26 ; ID = 23
CC*NYTT
 412  IA= 20
      IB= 139
      IC= 26
      ID= 23
CC*
      GO TO 41
CC*BORT
CC*413  IA= 19 ;  IB= 140 ; IC= 25 ; ID = 22
CC*NYTT
 413  IA= 19
      IB= 140
      IC= 25
      ID= 22
CC*
      GO TO 41
CC*BORT
CC*414  IA= 19 ;  IB= 140 ; IC= 25 ; ID = 22
CC*NYTT
 414  IA= 19
      IB= 140
      IC= 25
      ID= 22
CC*
      GO TO 41
CC*BORT
CC*415  IA= 19 ;  IB= 141 ; IC= 27 ; ID = 22
CC*NYTT
 415  IA= 19
      IB= 141
      IC= 27
      ID= 22
CC*
      GO TO 41
CC*BORT
CC*416  IA= 19 ;  IB= 142 ; IC= 25 ; ID = 23
CC*NYTT
 416  IA= 19
      IB= 142
      IC= 25
      ID= 23
CC*
      GO TO 41
CC*BORT
CC*417  IA= 20 ;  IB= 143 ; IC= 26 ; ID = 23
CC*NYTT
 417  IA= 20
      IB= 143
      IC= 26
      ID= 23
CC*
      GO TO 41
 41   CONTINUE
*
*
*... MONTERING MALLCYLINDER
      CALL COST (4,IA   ,IX+13,0.,1.,1.,0.,1.,XLIMB,DWIND,WDUCT,1.)
*
*    LINDNINGSARBETE
      CALL COST (5,IB,IX+15,0.,1.,1.,0.,1.,XBUNCH,ABUNCH,XCOOLD,1.)
*    MONTERING INRE STYRSK#RMAR
      CALL COST (4,144,IX+15,0.,1.,1.,0.,1.,XOGUR,1.,1.,1.)
      HLP = DWIND + WWIND
*    HJ#LPLINDARE
       IF(LC(5).EQ.1)
     *  CALL COST(4,30,IX+15,0.,1.,1.,0.,1.,XBUNCH,ABUNCH,XCOOLD,1.)
*
*    MONTERING LINDNING
      CALL COST (5,IC   ,IX+16,0.,1.,1.,0.,1.,XLIMB,DWIND,WDUCT,1.)
*    MONTERING YTTRE OLJESTYRSK#RMAR
      CALL COST (4,28,IX+16,0.,1.,1.,0.,1.,XOGUR,1.,1.,1.)
*
      XTAPP=NTAPP
      XOUCO=NOUCO
*    LINDNINGSUTTAG
      CALL COST (4,ID,IX+14,0.,1.,1.,0.,1.,XTAPP,ACOND,1.,1.)
*
      IF(ABS(XOUCO) .LT.1.E-6) GOTO 50
*    @VRIGA UTTAG, STAB SKIV
      ZPAR2 = ZPART/2.
      CALL COST (4,29,IX+17,0.,1.,1.,0.,1.,XOUCO,ZPAR2,1.,1.)
*
*
   50 CALL COST (3,0,IX+18,0.,1.,1.,0.,1.,1.,1.,1.,1.)
        GO TO 49
*
*       BANDLINDNING
 42    CALL COST(3,15,18+IX,0.,0.,0.,0.,1.,XLIMB,1.,1.,1.)
       GO TO 49
*
*      FLERLAGER-LINDNING
*           LINDNING KONTINUERLIGA VARV
  43   CALL COST(4,16,15+IX,0.,0.,0.,0.,1.,ZWIND,XLIMB,1.,1.)
*           @VERG*NG
       ACM1 = ZLAG - 1
       IY = 17
       IF(BBLNX) IY = 18
       CALL COST(4,IY,15+IX,0.,0.,0.,0.,1.,ACM1,ZCABEL,XLIMB,ZCOIAR)
*           KORSNINGAR
       ACM1= RPRTRR -1.
       CALL COST(4,19,15+IX,0.,0.,0.,0.,1.,ACM1,ZCABEL,XLIMB,ZCOIAR)
*           UTTAG
       CALL COST(4,20,14+IX,0.,0.,0.,0.,1.,XTAPP,1.,1.,1.)
*           MONTERING CYLINDER
       ACM2 = 1. + WDUCT/15.
       CALL COST(4,21,13+IX,0.,0.,0.,0.,1.,XLIMB,ACM2,1.,1.)
*            SET UP COSTS
       ACM2 = 1.+ 1.154/ZCOIAR
       CALL COST(4,22,16+IX,0.,0.,0.,0.,1.,ZCOIAR,ACM2,1.,1.)
       CALL COST(3, 0 ,18+IX,0.,0.,0.,0.,0.,0.,0.,0.,0.)
*
*
*----------------------------------------------------------------
  49  CALL COST (2,0,19*NUMBER,0.,1.,1.,0.,1.,1.,1.,1.,1.)
*
      RETURN
      END
