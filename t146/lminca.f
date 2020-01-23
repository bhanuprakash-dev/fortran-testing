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

************************************************************************
*
*      Subroutine LMINCA
*      -----------------
*
C...   Title:  Call to the LMIN optimisation routine
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 83-06-21 by H.Westberg , ZKAB
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 87-02-18 by H H�glund  , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 90-12-10 by B-G Bladh  , SETFO/TS
*               Print out stopped                   .
************************************************************************
*
       SUBROUTINE LMINCA(VALUE,ICONV,NF,NA,NB,NCYC)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*

       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0181(1)    ,  BARR   (1)    ),
     &  (XZ0681(1)    ,  BPART  (1)    ),
     &  (XZ0791(1)    ,  FILLF  (1)    ),
     &  (XZ0871(1)    ,  HPART  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    )
       EQUIVALENCE
     &  (XZ1311(1)    ,  BBHELP (1)    ),
     &  (XZ1399       ,  AVALUE        ),
     &  (XZ1414       ,  CLOSSV        ),
     &  (XZ1415       ,  COST          ),
     &  (XZ1419       ,  CPUIND        )
       EQUIVALENCE
     &  (XZ1422       ,  CTROVN        ),
     &  (XZ1431       ,  DRV           ),
     &  (XZ1437       ,  EPS           ),
     &  (XZ1442       ,  FEPS          ),
     &  (XZ1444       ,  AF            )
       EQUIVALENCE
     &  (XZ1483       ,  MNLY          ),
     &  (XZ1484       ,  ITR           ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1497       ,  NFMX          ),
     &  (XZ1508       ,  NWILI         )
       EQUIVALENCE
     &  (XZ1743(1)    ,  XA     (1)    ),
     &  (XZ1755(1)    ,  XREL   (1)    ),
     &  (XZ2307(1)    ,  GREL   (1)    ),
     &  (XZ2346(1)    ,  G      (1)    ),
     &  (XZ2500       ,  BBOOVN        )
       EQUIVALENCE
     &  (XZ2501       ,  VALUEM        ),
     &  (XZ2502       ,  VALUEO        ),
     &  (XZ2503       ,  VALUEF        )
*
       REAL      HV(7574),
     &           XA(30),XREL(30),GREL(96),G(96),PARAM(25),BARR(100),
     &           BPART(9),HPART(9),FILLF(9),RRWDG(9),AF,AVALUE,CLOSSV,
     &           COST,CPUIND,CTROVN,DRV,EPS,FACTOR,FEPS,VALUE,VALUEF,
     &           VALUEM,VALUEO
*
       INTEGER   I,ICONV,ITR,JFC,MNLY,NA,NB,NCYC,NF,NFMX,NF1,NWILI
CCCC   INTEGER   IWDG
*
       LOGICAL   BBOOVN,BBHELP(10)
*
       EXTERNAL  TRYCK,FUNK
*
CC*SEBL End of the declarations block.
*
C... Initialisation block.
*
CC*SBBL Start of the initialisation block.
*
C... Initialise PARAM
*
       print *,"Start of Lminca"
       DO 10 I=1,25
   10  PARAM(I)=0.
*
       PARAM(1)=2.
       PARAM(3)=BARR(36)
       PARAM(6)=0.05
       PARAM(7)=0.2
       PARAM(8)=0.1
       PARAM(13)=BARR(37)
       PARAM(22)=10.
       BBOOVN=.FALSE.
       NF=0
*
CC*SEBL End of the initialisation block.
*
C... Call to the auxiliary optimisation routine LMOD.
*
CCCCC  IF (BBHELP(3)) CALL LMOD(NA,NB,XA,XREL,GREL,EPS,
CCCCC&                          FEPS,DRV,AF,NFMX,ITR,PARAM)
*
CCCC   WRITE (*,*) 'Optimising the comparison price'
*
C... Call to the optimisation routine LMIN.
*
       write(*,*) "NA=",NA
       write(*,*) "XA=",XA
       write(*,*) "VALUE=",VALUE
       write(*,*) "XREL=",XREL
       write(*,*) "LMINGREL=",GREL
       write(*,*) "EPS=",EPS
       write(*,*) "FEPS=",FEPS
       write(*,*) "DRV=",DRV
       write(*,*) "AF=",AF
       write(*,*) "NFMX=",NFMX
       print *, "NCYC=",NCYC
       write(*,*) "NF=",NF
       write(*,*) "ITR=",ITR
       CALL LMIN(NA,NB,XA,VALUE,XREL,GREL,EPS,
     &           FEPS,DRV,AF,NFMX,NCYC,NF,-IABS(ITR),TRYCK,
     &           FUNK,HV,PARAM,ICONV)
*
       print *,'First lmin'
       write(*,*) "NCYC1=",NCYC

C... Proceed with the calculation
*
C,,, If the optimisation went well.
*
       IF (ICONV.GE.1) THEN
*
C... Calculate the function
*
          CALL FUNK(XA,VALUE,G,199)
cc          irc=isplnk('select',13,'cmd(clrscrn )')
CCCC      WRITE (*,110) NF,VALUEO,CTROVN,COST,CLOSSV
CCCC      WRITE (*,121) 'Dcore',1.E+3*XA(1),
CCCC &                  'Limb height',1.E+3*XA(2),
CCCC &                  'Flux density',XA(3)
CCCC      WRITE (*,122) (1.E+6*XA(I),I=3+1,3+NWILI)
CCCC      WRITE (*,123) (1.E+3*HPART(IWDG),IWDG=1,NWILI)
CCCC      WRITE (*,124) (1.E+3*BPART(IWDG),IWDG=1,NWILI)
CCCC      WRITE (*,125) (FILLF(IWDG),IWDG=1,NWILI)
CCCC      WRITE (*,126) (1.E+3*RRWDG(IWDG),IWDG=1,NWILI)
CCCC      CALL NPAGE(8)
CCCC      WRITE (JFC,110) NF,VALUEO,CTROVN,COST,CLOSSV
CCCC      WRITE (JFC,121) 'Dcore',1.E+3*XA(1),
CCCC &                    'Limb height',1.E+3*XA(2),
CCCC &                    'Flux density',XA(3)
CCCC      WRITE (JFC,122) (1.E+6*XA(I),I=3+1,3+NWILI)
CCCC      WRITE (JFC,123) (1.E+3*HPART(IWDG),IWDG=1,NWILI)
CCCC      WRITE (JFC,124) (1.E+3*BPART(IWDG),IWDG=1,NWILI)
CCCC      WRITE (JFC,125) (FILLF(IWDG),IWDG=1,NWILI)
CCCC      WRITE (JFC,126) (1.E+3*RRWDG(IWDG),IWDG=1,NWILI)
*
C... Re-optimise the function
*
C,,, IF Optimisation OK or MNL79 or later and CPUIND <> 0
*
          IF (ICONV.GE.2.OR.(MNLY.GE.79.AND.CPUIND.NE.0.)) THEN
*
C... Re-optimise
*
C,,, If CLOSSV > 0
*
             IF (CLOSSV.GT.0.) THEN
*
                FACTOR=1.+CPUIND
                NF1=NF
                IF (MNLY.LT.79) FACTOR=1.
*
C... Re-optimise
*
C,,, If required
*
                IF (FACTOR.GE.1) THEN
                   VALUEM=FACTOR*VALUEO
                   BBOOVN=.TRUE.
                   GREL(4)=VALUEM/100.
                   AVALUE=COST
                   AF=0.01+0.5*CPUIND
*
C... Call to the auxiliary optimisation routine LMOD.
*
CCCCC              IF (BBHELP(3)) CALL LMOD(NA,NB,XA,XREL,GREL,EPS,
CCCCC&                                      FEPS,DRV,AF,NFMX,ITR,PARAM)
*
       WRITE (*,*)
       WRITE (*,*) 'Optimising the manufacturing price'
  
*
C... Call to the optimisation routine LMIN.
*
                   CALL LMIN(NA,NB,XA,VALUE,XREL,GREL,EPS,
     &                       FEPS,DRV,AF,NFMX,NCYC,NF,-IABS(ITR),TRYCK,
     &                       FUNK,HV,PARAM,ICONV)
*
                   write(*,*)"NCYC2",NCYC
                   print *,'SECOND lmin'
C... Calculate the function
*
                   CALL FUNK(XA,VALUE,G,199)
                   VALUEF=COST+CLOSSV
cc                   irc=isplnk('select',13,'cmd(clrscrn )')
                   WRITE (*,210) NF,VALUEF,CTROVN,COST,CLOSSV
                   CALL NPAGE(8)
                   WRITE (JFC,210) NF,VALUEF,CTROVN,COST,CLOSSV
                   NF=NF+NF1
                   WRITE (*,121) 'Dcore',1.E+3*XA(1),
     &                           'Limb height',1.E+3*XA(2),
     &                           'Flux density',XA(3)
CCCC               WRITE (*,122) (1.E+6*XA(I),I=3+1,3+NWILI)
CCCC               WRITE (*,123) (1.E+3*HPART(IWDG),IWDG=1,NWILI)
CCCC               WRITE (*,124) (1.E+3*BPART(IWDG),IWDG=1,NWILI)
CCCC               WRITE (*,125) (FILLF(IWDG),IWDG=1,NWILI)
CCCC               WRITE (*,126) (1.E+3*RRWDG(IWDG),IWDG=1,NWILI)
                   WRITE (JFC,121) 'Dcore',1.E+3*XA(1),
     &                             ' Limb height',1.E+3*XA(2),
     &                             ' Flux density',XA(3)
CCCC               WRITE (JFC,122) (1.E+6*XA(I),I=3+1,3+NWILI)
CCCC               WRITE (JFC,123) (1.E+3*HPART(IWDG),IWDG=1,NWILI)
CCCC               WRITE (JFC,124) (1.E+3*BPART(IWDG),IWDG=1,NWILI)
CCCC               WRITE (JFC,125) (FILLF(IWDG),IWDG=1,NWILI)
CCCC               WRITE (JFC,126) (1.E+3*RRWDG(IWDG),IWDG=1,NWILI)
                END IF
             END IF
          END IF
       END IF
*
C... Print the critical parameter conditions
*
       CALL PLIMIT(1)
       print *,'end of lminca'
*
  199  RETURN
  110  FORMAT ('0','       Optimization 1 : Comparison price',19X,
     &         'No of calc',I4,/,'        Comp P',F10.0,' OVN',F10.0,
     &         ' Manuf C',F10.0,' Loss C',F10.0)
  121  FORMAT (6X,A6,1X,F6.0,1X,A13,1X,F6.0,1X,A14,1X,F6.4)
  122  FORMAT ('      Conductor area  ',9(F8.2))
  123  FORMAT ('      Strand height   ',9(F8.2))
  124  FORMAT ('      Strand width    ',9(F8.2))
  125  FORMAT ('      Wdg space factor',9(F8.4))
  126  FORMAT ('      Wdg radial width',9(F8.2))
  210  FORMAT ('0','        Optimization 2 : Manufacturing cost',17X,
     &         'No of calc',I4,/,'        Comp P',F10.0,' OVN',F10.0,
     &         ' Manuf C',F10.0,' Loss C',F10.0)
       END
