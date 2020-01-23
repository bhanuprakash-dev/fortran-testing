*********************************************************************
*                                                                   *
*      The copyright to the computer program herein is  the         *
*      property of ABB  TRANSFORMERS, Sweden.  The  program         *
*      may be used or copied only with the written  permis-         *
*      sion of ABB  TRANSFORMERS or in accordance with  the         *
*      terms of agreement under which the program has  been         *
*      supplied.                                                    *
*                                                                   *
*      In no event shall ABB  TRANSFORMERS  be  liable  for         *
*      incidental or consequential damages arising from use         *
*      of this program.                                             *
*                                                                   *
*********************************************************************

************************************************************************
*
*      Function NCDTCA
*      ---------------
*
*  0.  Written: XX-XX-XX by A.N. Other   , XXXX
*      Revised: 91-11-27 by Ron Bell     , ABB Power T & D Muncie
*
*  1.  Description
C...       Title:  Calculates the number of cooling ducts
C...               in the TCA core
*
************************************************************************
*
       INTEGER FUNCTION NCDTCA(KSERIE,TYP,DCORI,TOPOIL,BLIMB,FREQ)
*
       REAL       BLIMB,BCOMP,DCOR,DCORI,FREQ,TOPOIL,TYP,Y
*
       REAL       TSLOPE(7,8),TINTCP(7,8),BSLOPE(9,8),
     &            BINTCP(9,8)
*
       INTEGER    I,KCOL,KTROW,KSERIE,NCDLTB,NCD

       INTEGER    KCORE(10)

       DATA TSLOPE/.0124 ,.0132 ,.0143 ,.0150 ,.0160 ,.0178 ,.0200,
     &             .0136 ,.0140 ,.0144 ,.0152 ,.0160 ,.0174 ,.0200,
     &             .0124 ,.0125 ,.0133 ,.0140 ,.0156 ,.0171 ,.0200,
     &             .0123 ,.0126 ,.0133 ,.0139 ,.0156 ,.0169 ,.0200,
     &             .00713,.00763,.00806,.00850,.00906,.00944,.0100,
     &             .00781,.00819,.00863,.00913,.00956,.00981,.0100,
     &             .00825,.00838,.00869,.00881,.00913,.00950,.0100,
     &             .00769,.00813,.00825,.00856,.00863,.00913,.0100/
       DATA TINTCP/-6.12 ,-6.46 ,-6.82 ,-6.85 ,-7.00 ,-7.49 ,-8.00 ,
     &             -7.28 ,-7.20 ,-7.12 ,-7.16 ,-7.10 ,-7.27 ,-8.00 ,
     &             -5.42 ,-5.08 ,-5.27 ,-5.20 ,-5.58 ,-5.66 ,-6.00 ,
     &             -5.32 ,-5.13 ,-5.27 ,-5.10 ,-5.58 ,-5.45 ,-6.00 ,
     &             -4.09 ,-4.24 ,-4.34 ,-4.35 ,-4.44 ,-4.16 ,-4.00 ,
     &             -4.62 ,-4.58 ,-4.64 ,-4.69 ,-4.59 ,-4.19 ,-4.00 ,
     &             -4.78 ,-4.56 ,-4.48 ,-4.22 ,-4.19 ,-4.05 ,-4.00 ,
     &             -4.23 ,-4.34 ,-4.18 ,-4.19 ,-3.94 ,-3.89 ,-4.00 /
       DATA BSLOPE/-.0820,-.0595,-.0500,-.0388,0.    ,0. ,0.,0.,0.,
     &             -.0889,-.0641,-.0467,-.0347,0.    ,0. ,0.,0.,0.,
     &             -.1320,-.1020,-.0781,-.0559,-.0427,0. ,0.,0.,0.,
     &             -.1740,-.1160,-.0781,-.0694,-.0500,0. ,0.,0.,0.,
     &             -.1040,-.0909,-.0794,-.0641,-.0490,0. ,0.,0.,0.,
     &             -.1160,-.1060,-.0943,-.0746,-.0685,0. ,0.,0.,0.,
     &             -.1390,-.1140,-.0943,-.0862,-.0769,-.0621,-.0538,
     &             0.    ,0.    ,
     &             -.1470,-.1280,-.1050,-.0943,-.0877,-.0769,-.0676,
     &             -.0550,-.0444/
       DATA BINTCP/2.07,2.21,2.33,2.34,10.0,10.0,10.0,10.0,10.0,
     &             1.94,2.14,2.23,2.26,10.0,10.0,10.0,10.0,10.0,
     &             2.07,2.31,2.41,2.39,2.35,10.0,10.0,10.0,10.0,
     &             1.99,2.24,2.30,2.40,2.35,10.0,10.0,10.0,10.0,
     &             2.13,2.27,2.38,2.40,2.38,10.0,10.0,10.0,10.0,
     &             2.06,2.24,2.40,2.41,2.48,10.0,10.0,10.0,10.0,
     &             2.15,2.30,2.35,2.41,2.45,2.43,2.43,10.0,10.0,
     &             2.06,2.17,2.24,2.30,2.38,2.41,2.42,2.40,2.36/
       DATA  KCORE/1,0,3,0,0,0,5,5,5,7/
*
C... TCA Core
*      
       WRITE(*,*) 'NCDTCA TYP = ', TYP
       IF (KSERIE.EQ.4) THEN
          DCOR=DCORI*1000.
          NCD=0
          KCOL=KCORE(NINT(TYP))
          IF (FREQ.GT.55.) KCOL=KCOL+1
          KTROW=IFIX(.5+TOPOIL/5.)-11
*
          IF (KTROW.LT.1) THEN
             KTROW=1
          ELSE IF (KTROW.GT.7) THEN
             KTROW=7
          END IF
*
          Y=TINTCP(KTROW,KCOL)+ TSLOPE(KTROW,KCOL)*DCOR
          DO 799 I=1,9
             BCOMP= BINTCP(I,KCOL)+ BSLOPE(I,KCOL)*Y
             IF (BCOMP.LT.BLIMB) NCD=I
  799     CONTINUE
*
       ELSE IF (KSERIE.EQ.5) THEN
*
C...  (LTB Core, TAA-M)
*
          NCD=NCDLTB(TYP,(DCORI*1000.))
*
       END IF
*
       NCDTCA=NCD
*
       RETURN
       END
