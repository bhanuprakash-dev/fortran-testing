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
*      Subroutine HEAD2
*      ----------------
*
C...   Title: Optimisation control routine
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 83-05-25 by H.Westberg , ZKAB
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-12-10 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
*Print out lay out changed.
************************************************************************
*
       SUBROUTINE HEAD2
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ0001(1)    ,  AARR   (1)    ),
     &  (XZ0401(1)    ,  TARR   (1)    ),
     &  (XZ0651(1)    ,  ACOND  (1)    ),
     &  (XZ0881(1)    ,  HWIND  (1)    ),
     &  (XZ1001(1)    ,  RRWDG  (1)    )
       EQUIVALENCE
     &  (XZ1375       ,  BBDLI         ),
     &  (XZ1377       ,  BBEND         ),
     &  (XZ1379       ,  BBERR1        ),
     &  (XZ1380       ,  BBEXAC        )
       EQUIVALENCE
     &  (XZ1382       ,  BBFLU         ),
     &  (XZ1384       ,  BBHLI         ),
     &  (XZ1386       ,  BBOPTI        ),
     &  (XZ1389       ,  BBREA         ),
     &  (XZ1399       ,  AVALUE        ),
     &  (XZ1426       ,  DCORE         ),
     &  (XZ1446       ,  FREQ          )
       EQUIVALENCE
     &  (XZ1472       ,  HLIMB         ),
     &  (XZ1483       ,  MNLY          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ1508       ,  NWILI         ),
     &  (XZ1519       ,  IOPT          ),
     &  (XZ1587       ,  CHCOST        )
       EQUIVALENCE
     &  (XZ1596       ,  FILECH        ),
     &  (XZ1624       ,  MNL           ),
     &  (XZ1632(1)    ,  RUBCH  (1)    ),
     &  (XZ1743(1)    ,  XA     (1)    ),
     &  (XZ2346(1)    ,  G      (1)    )
       EQUIVALENCE
     &  (XZ2562       ,  BBFREQ        ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ2567       ,  BBLAY         )

*
       REAL      XA(30),G(96),AARR(200),RRWDG(9),RRWDGO(9),
     &           ACOND(9),HWIND(9),RRTOL,
     &           AVALUE,DCORE,DIFFRR,FACTOR,FREQ,HLIMB,VALUE
*
       INTEGER   ICONV,IOPT,IWDG,JFC,NA,NB,NCYC,NF,NTRIES,NWILI,MNLY,
     &           ISTOP
*
       LOGICAL   BBOPTI,BBEXAC,BBEND,BBREA,BBERR1,BBFREQ,BBLAY,
     &           BBDLI,BBFLU,BBHLI,BBLAYX(9),BBLAYD
*
       DIMENSION TEXT(5),RUBCH(15),
     &           TARR(100)
       CHARACTER TEXT*24,RUBCH*4,FILECH*24,MNL*8,CHCOST*6,
     &           TARR*8
*
       DATA TEXT/'Fundamental error       ',
     &           'Max.No.of calc. reached ',
     &           'Interrupt:  FUNK routine',
     &           'Optimisation successful ',
     &           'Optimum detected        '/
*
       DATA RRWDGO /9*0./,FACTOR /1.E+3/
*
CC*SEBL End of the declarations block.
*
       print*,'inside head2'
       BBLAYD=.TRUE.
       IF (BBLAY) BBLAYD=.FALSE.
       BBEND=.FALSE.
       BBERR1=.FALSE.
       BBFREQ=.FALSE.
       IF (ABS(FREQ-50.).GT.1.E-3.AND.
     &     ABS(FREQ-60.).GT.1.E-3) BBFREQ=.TRUE.

       XA(1)=DCORE
       XA(2)=HLIMB
       AVALUE=0.
       NTRIES=0
*
C... Quantities required for the optimisation subroutine
*
        NA=(3+NWILI)
        NB=(15+3*(NWILI-1))
*
C... Reading the cost file required
*
        write(*,*)'filech=',FILECH
        write(*,*) '*****HEAD2: Before READCH*******'
        CALL READCH(MNLY,FILECH,MNL,RUBCH,CHCOST)
        write(*,*) '*****HEAD2: After  READCH Rubch: ',RUBCH
*....   Backtracking
        IF(ISTOP.EQ.1) RETURN
*
C... Starting a new page on the output file
*
CCCC   CALL NPAGE(0)
*
C... Calculate the function
*
C,,, Optimise
*
  400  IF (BBOPTI) THEN
          BBEXAC=.TRUE.
*
C... AVALUE scales VALUE to approximately 1 in FUNK via LMIN
*
          AVALUE=1.
*
C... Calculate the transformer
*
          print*,'before funk'
          CALL FUNK(XA,VALUE,G,*299)
          print*,'after funk'
          write(*,*)'after first funk in head2 XZ1515=',XZ1515
*
C... Adjustment of certain quantities
*
          AVALUE=VALUE/1.2
          BBEXAC=.FALSE.
*
          write(*,*) "GWINCO=",XZ0861
          IF (.NOT.BBLAY) THEN
*
             IF (NTRIES.LE.1) THEN
*               
                
                print*,'before lminca1'
                write(*,*) "HEAD2_NCYC=",NCYC
                CALL LMINCA(VALUE,ICONV,NF,NA,NB,NCYC)
                IF (ICONV.LE.0) BBERR1=.TRUE.
*
                print*,'after lminca1'
C... Print an error warning, if any
*
                IF (BBERR1.OR.BBFREQ) CALL ERRRUT(BBERR1,BBFREQ,JFC)
*
             END IF
*
          ELSE
*
             IF (.NOT.BBLAYD) THEN
*
                NTRIES=1+NTRIES
*
                print*,'before prep1 in head2'
                CALL PREP1

                print*,'after prep1 in head2'
*
* ...           BACKTRACKING
                IF(ISTOP.EQ.1) RETURN
*
                print*,'before lminca2'
                write(*,*) "HEAD2_NCYC=",NCYC
                CALL LMINCA(VALUE,ICONV,NF,NA,NB,NCYC)
                print*,'after lminca2'
                IF (ICONV.LE.0) BBERR1=.TRUE.
*
C... Print an error warning, if any
*
                IF (BBERR1.OR.BBFREQ) CALL ERRRUT(BBERR1,BBFREQ,JFC)
*
C... Save the old winding widths and space factors
*
                DO 488 IWDG=1,NWILI
  488           RRWDGO(IWDG)=RRWDG(IWDG)
*
C... Automatic winding layout
*
                IF (.NOT.BBERR1.AND.NTRIES.LE.12) THEN                        
CCCC               CALL NPAGE(3)
CCCC               WRITE (JFC,100) 'Automatic winding layout'
                print*,'before wdglay'                        
                   CALL WDGLAY
                print*,'after wdglay'                   
*...........       BACKTRACKING
                   IF(ISTOP.EQ.1) RETURN
                END IF
*
C... Check whether winding layout fits the transformer window
C... RRTOL = winding width tolerance between optimized and exact
*
                RRTOL=0.99
                DO 497 IWDG=1,NWILI
                DIFFRR=FACTOR*ABS(RRWDG(IWDG)-RRWDGO(IWDG))
*
C... BBLAYX(IWDG) = Layout of winding (IWDG) within tolerance
*
                BBLAYX(IWDG)=.TRUE.
                IF (DIFFRR.GT.RRTOL) BBLAYX(IWDG)=.FALSE.
  497           CONTINUE
*
C... BBLAYD       = Layout of all windings within tolerance
*
                BBLAYD=.TRUE.
                DO 498 IWDG=1,NWILI
                IF (.NOT.BBLAYX(IWDG).OR..NOT.BBLAYD) BBLAYD=.FALSE.
  498           CONTINUE
*
CCCC            WRITE (*,127) (FACTOR*RRWDG(IWDG),IWDG=1,NWILI)
CCCC            WRITE (*,128) (FACTOR*ABS(RRWDG(IWDG)-RRWDGO(IWDG)),
CCCC &                         IWDG=1,NWILI)
CCCC            WRITE (*,*)
CCCC            CALL NPAGE(2)
CCCC            WRITE (JFC,127) (FACTOR*RRWDG(IWDG),IWDG=1,NWILI)
CCCC            WRITE (JFC,128) (FACTOR*ABS(RRWDG(IWDG)-RRWDGO(IWDG)),
CCCC &                         IWDG=1,NWILI)
*
                GOTO 400
             END IF
          END IF
*
*
C... Print an error warning, if any
*
          IF (BBERR1.OR.BBFREQ) CALL ERRRUT(BBERR1,BBFREQ,JFC)
CCCC      CALL NPAGE(2)
CCCC      WRITE (JFC,91) TEXT(ICONV+3),NF,' calculations.',
CCCC &                  'Optimisation accuracy',IOPT
CCCC      WRITE (JFC,92) NCYC,' cycles.'
*
C... Print 'No optimisation requested.'
*
  299  END IF
       BBEXAC=.TRUE.
       AVALUE=1.
*
C... Function calculation
*
       print*,'before funk'
       CALL  FUNK(XA,VALUE,G,*399)
       print*,'after funk'
  399  BBEND=.TRUE.
       BBREA=.TRUE.
*
C... Function calculation
*
       print*,'before funk'
       CALL  FUNK(XA,VALUE,G,*199)
       print*,'after funk'
*
C... Cooling calculation for TAD.
****
****       IF (TARR(82).EQ.'YES    ') THEN
****          CALL COOLEQ
****       ENDIF
*
       IF (MNLY .GT. 91 ) THEN
        print*,'before pnmass'               
          CALL PNMASS
        print*,'after pnmass'
       ENDIF
*
        print*,'coming out of head2'        
  199  RETURN
*
   91  FORMAT('0','     ',A26,1X,I4,A14,2X,A21,I2)
   92  FORMAT(' ','     ',26X,1X,I4,A14)
  100  FORMAT (' ',/,' ',A,/)
  127  FORMAT(' Current  radial width',9(F8.2))
  128  FORMAT(' Diff. in radial width',9(F8.2))
*
       END
