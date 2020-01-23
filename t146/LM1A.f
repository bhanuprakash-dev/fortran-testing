      SUBROUTINE LM1A(NVAR,K1,KL1,X,F0,XREL,GREL,NFMX,
     &NF,NCYK,ITR,TRYCK,FUNK,CONV,
     &ALFA,SIGLIM,BETA,STPLIM,MFIRST,PCONT,DELTA,
     &EPSI,FEPSI,DEPSI,GAMMA,TD,NE,NG,NP,
     &IUT1,IUT2,IUT3,IUT4,IUT5,IUT6,IUT7,IUT8,
     &AMMAX,ERRM,NOR,PARAM,MNRES,NHV,BNHV,
     &E,G,P,LASTX,SIGNAL,
     &AM,F,G0,LIMIT,XF,X0,XDELTA,
     &XC,XX,G1,G2,SIGMA,ALFA1,LASTG,HV,INDEX,G3,GC,
     &AMAX,CENT,EQLP,INDG,CD,AA,EQG)

      CHARACTER VP*2,VP2*2,VP3*5,VP4*2,BL*4
      DIMENSION X(*),XREL(*),GREL(*)
      DIMENSION E(NE,*),G(NG,*),P(NP,*),LASTX(5,*),SIGNAL(NP,*),
     &AM(*),F(*),G0(*),LIMIT(*),XF(*),X0(*),XDELTA(*),
     &XC(*),XX(*),G1(*),G2(*),SIGMA(*),ALFA1(*),
     &LASTG(5,*),HV(*),INDEX(*),G3(*),GC(*),AMAX(*),CENT(*),
     &EQLP(*),INDG(*),CD(*),AA(*),EQG(*)
      DIMENSION PARAM(*)
      DIMENSION LASTF(5)
      DIMENSION IND(25)
      INTEGER PE,SIGNAL,STEPS,CONV,RES,PP,PCONT,SIGLIM,STPLIM,PE1
      INTEGER EQG,BNHV,a,pkl,c,d,e1,f1,gg,h,i,LCOL
      LOGICAL LIMIT,START,LIN,CENT,DUAL
      REAL LASTX,LASTF,MFIRST,LASTG
      EXTERNAL FUNK, TRYCK
      DATA VP/'X('/,VP2/'G('/,VP3/'GREL('/,VP4/'A('/,BL/' '/
      DATA IND/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
     &         22,23,2*0/
C
C.... INITALIZE THE PROGRAM ............................................
      print *,"start of LM1A"
      IF(CONV.EQ.-3) GOTO 6
      N=0
      DO 3 NN=1,NVAR
      DO 2 J=1,5
    2 LASTX(J,NN)=X(NN)
c      CALL LM10 (XC(NN),' C',1)
      IF(XREL(NN).EQ.0.)GOTO 3
      N=N+1
      INDEX(N)=NN
c      CALL LM10 (XC(NN),BL,1)
      X0(N)=X(NN)/XREL(NN)
    3 CONTINUE
C

      K=0
      KL=0
      PE=0
      II=0
      JJ=0
      NZ=0
      write(*,*)"GREL LMIN=",(GREL(i),i=1,15)
      IF(K1.EQ.0)GOTO 11
      DO 7 I=1,K1
      IF(GREL(I).EQ.0.)NZ=NZ+1
    7   IF(GREL(I).LT.0.)PE=PE+1
c   7 CALL LM10 (GC(I),BL,1)
      III=K1-PE-NZ
      DO 10 I=1,K1
c      CALL LM10 (EQLP(I),BL,1)
c      CALL LM10 (GC(I),'I',1)
      IF(GREL(I).EQ.0)GOTO 10
      K=K+1
      IF(I.LE.IABS(KL1))KL=KL+1
c      CALL LM10 (GC(I),BL,1)
c      IF(GREL(I).LT.0.) CALL LM10 (GC(I),'E',1)
      IF(GREL(I).LT.0.)JJ=JJ+1
      IF(GREL(I).LT.0.)INDG(III+JJ)=I
      IF(GREL(I).GT.0.)II=II+1
      IF(GREL(I).GT.0.) INDG(II)=I
   10 CONTINUE
   11 CONTINUE


      DO 5 NN=1,N
    5 AMAX(NN)=AMMAX

      N2=N*2
      ME=2*N+K
      LCOLE=ME+2*N-PE+1
      NF=0
      NRES=0
      NCYK=0
      CONV=0
      START=.TRUE.
C
C.... LINEARIZATION
      DELTA1=DELTA
C
C.... LP-SOLUTION
      print *,'before lm8',PE,NE,ME,N2
      CALL LM8 (NE,ME,N2,PE,a,pkl,c,d,e1,f1,gg,h,i,LCOL)
C
      print *,'after lm8',PE,NE,ME,N2

C.......................................................................
      IF(ITR.EQ.0)GOTO 15
C.... STARTL@SNING
C.... CALCULATION
      CALL LM9A (X0,F0,G0,N,NF,X,FUNK,INDEX,XREL,GREL,
     &           G3,K1,NVAR,IUT3,IRET1)
      IF (IRET1 .EQ. 1) GOTO 145
      IF(ITR.GT.0)GOTO 12
    6 WRITE(6,500)NVAR,K1,EPSI,FEPSI,DELTA,MFIRST,NFMX,ITR
      IF(PARAM(1).EQ.0.) GOTO 8
      WRITE(6,620)
      WRITE(6,520) IND(1),IFIX(PARAM(1))
      IF(PARAM(2).NE.0.) WRITE(6,510) IND(2),TD
      IF(PARAM(3).NE.0.) WRITE(6,510) IND(3),ALFA
      IF(PARAM(4).NE.0.) WRITE(6,520) IND(4),SIGLIM
      IF(PARAM(5).NE.0.) WRITE(6,520) IND(5),PCONT
      IF(PARAM(6).NE.0.) WRITE(6,510) IND(6),DEPSI
      IF(PARAM(7).NE.0.) WRITE(6,510) IND(7),AMMAX
      IF(PARAM(8).NE.0.) WRITE(6,510) IND(8),ERRM
      IF(PARAM(9).NE.0.) WRITE(6,520) IND(9),NOR
      IF(PARAM(10).NE.0.)WRITE(6,520) IND(10),STPLIM
      IF(PARAM(11).NE.0.)WRITE(6,510) IND(11),GAMMA
      IF(PARAM(12).NE.0.)WRITE(6,510) IND(12),BETA
      IF(PARAM(13).NE.0.)WRITE(6,520) IND(13),IUT3
      IF(PARAM(1).EQ.1)  GOTO 8
      IF(PARAM(14).NE.0.)WRITE(6,520) IND(14),IUT4
      IF(PARAM(15).NE.0.)WRITE(6,520) IND(15),IUT1
      IF(PARAM(16).NE.0.)WRITE(6,520) IND(16),IUT2
      IF(PARAM(17).NE.0.)WRITE(6,520) IND(17),IUT5
      IF(PARAM(18).NE.0.)WRITE(6,520) IND(18),IUT6
      IF(PARAM(19).NE.0.)WRITE(6,520) IND(19),IUT7
      IF(PARAM(20).NE.0.)WRITE(6,520) IND(20),IUT8
      IF(PARAM(21).NE.0.)WRITE(6,520) IND(21),KL1
      IF(PARAM(22).NE.0.)WRITE(6,520) IND(22),MNRES
      IF(PARAM(1).EQ.2) GOTO 8
      IF(PARAM(23).NE.0.)WRITE(6,520) IND(23),NHV
    8 CONTINUE
      IF(CONV.NE.-3) GOTO 9
      WRITE(6,626) BNHV
      WRITE(6,613) CONV
      RETURN
    9 WRITE(6,502)(I,X(I),XC(I),I,XREL(I),I=1,NVAR)
      WRITE(6,615)(VP3,J,GREL(J),J=1,K1)
C
C
      WRITE(6,608)
      WRITE(6,609) F0
      WRITE(6,603)(VP,J,X(J),XC(J),J=1,NVAR)
      WRITE(6,606)(VP2,J,G3(J),EQLP(J),GC(J),J=1,K1)
      WRITE(6,601)
      GOTO 15
   12 CONTINUE
      DO 13 I=1,NVAR
c      CALL LM10 (CD(I),' .',1)
   13 AA(I)=0.
      CALL TRYCK(X,F0,G3,NCYK,NF,CD,AA,CONV)
C.......................................................................
   15 CONTINUE
 
      IF(IUT8.EQ.1)WRITE(42,280) N2+1,N2+2,ME+1
      BETA=BETA/STPLIM
      IRES=1
      RES=1
C
      IF(KL1.GE.0)GOTO 20
      LIN=.TRUE.
      KL1=-KL1
      GOTO 30
C
   20 LIN=.FALSE.
C
C
C.... CONTROL
   30 CALL LM2 (CONV,EPSI,FEPSI,F0,LASTF,LASTNF,LASTNX,LASTX,
     &          N,NCYK,START,XDELTA,X0,INDEX,LASTG,K,G0,X,G3,K1)
C
C.... START OF MINIMIZATION ............................................
C
C**** NEWITERATION ****
C.... LINEARIZATION
 
   40 print *,"LM1Acalling LM3A" 
      CALL LM3A (CENT,DELTA1,DEPSI,F,F0,G,NG,G0,K,KL,LIN,N,
     &           PCONT,PP,START,X0,TD,XX,G1,G2,AMAX,ERRM,NF,
     &           X,FUNK,INDEX,XREL,GREL,G3,K1,NVAR,IUT3,IRET1)
      IF (IRET1 .EQ. 1) GOTO 145
C
c      goto 190
C
   45 CONTINUE
C
C.... NEWLIMITS

      CALL LM4 (ALFA,AM,BETA,B,GAMMA,LIMIT,N,P,NP,SCALAR,
     &          SIGNAL,STEPS,START,XF,X0,XDELTA,MFIRST,STPLIM,
     &          ILP,NOR,SIGMA,SIGLIM,ALFA1)
C
C
 
C
C**** TRY AGAIN ****
   50 IF(IRES.NE.1)GOTO 56
      DO 55 NN=1,N
   55 AM(NN)=MFIRST
   56 CONTINUE
      IF(RES.LT.2.OR.RES.GT.4) GOTO 65
      DO 60 NN=1,N
   60 AM(NN)=ALFA*AM(NN)
C
C.... PREPARATION
   65 CALL LM7 (AM,E,NE,F,G,NG,G0,K,LCOLE,ME,N,P,NP,PE,
     &          AMAX,AMMAX,EPSI,RES)

C
   67 CONTINUE
C
C.... LP-SOLUTION
      DO 1000 MN=1,NE
 1000 write(*,*) "E=",(E(MN,J),J=1,50)
      write(*,*) "PE=",PE
      write(*,*) "E(22,41)=",E(22,41)
      CALL EPLM8 (E,NE,ME,N2,PE,RES,TD,XDELTA
     &,Z,HV,a,pkl,c,d,e1,f1,gg,h,i,LCOL)
      
C

C
C     
      write(*,*) "gotoRES=",RES
      IF(RES.EQ.1)GOTO 70
C.... CALCULATION  (ONLY TO GET RIGHT OUTPUT)
      IF(RES.EQ.5)CONV=-2
      IF(RES.EQ.5)GOTO 90
      NRES=NRES+1
C.... IF(NRES.GE.MNRES)CONV=3
      IF(NRES.GE.MNRES)CONV=-2
C
      IF(IRES.EQ.1)IRES=0
      GOTO 90
C
   70 write(*,*)"gotoXDELTA=",(XDELTA(i),i=1,9)
      write(*,*)"gotoX0=",(X0(i),i=1,9)
      DO 80 NN=1,N
      NN2=2*NN
      NN21=NN2-1
      X0(NN)=X0(NN)+XDELTA(NN21)-XDELTA(NN2)
   80 CONTINUE
 
      IF(IRES.EQ.1)IRES=2
      IF(IRES.EQ.0)IRES=1

      IF(IUT8.EQ.0)GOTO 88
C**** TEST AV LP-L@SN. ****
C     GENOM ATT S#TTA IN ERH$LLNA X-V#RDEN I EKV-SYSYEMET
C     OCH JFR. MED H@GERLEDET KAN LP-L@SN. KONTROLLERAS.
C     OM VL > HL  FEL.
C     OM VL < HL  OK
C     OM VL = HL  OK. BEG. LIGGER IMOT.
C
C     DUALA VARIABLER  HV(2*N+1,4*N+K)
C     HV(2*N+1,4*N) BEG. P$ DE FRIA VARIABLERNA
C     HV(4*N+1,4*N+K) IMPLICITA BEG. (=G-BEG.)
C     OM D.V. SKILLDA FR$N NOLL LIGGER BEG. MOT. JFR. VL=HL OVAN.
C     MINST N-ST. BEG. SKALL LIGGA MOT.
C
C     EKV.SYSTEMET(E-MATRISEN) ORDNAS GENOM ANROP AV EPLM7
      CALL LM7 (AM,E,NE,F,G,NG,G0,K,LCOLE,ME,N,P,NP,PE,
     &          AMAX,AMMAX,EPSI,RES)
      VL=0
      NC=0
      
      DO 84 II=2,ME+1
      HL=E(II,LCOLE)
      DO 81 NN=1,N2
   81 VL=VL+XDELTA(NN)*E(II,NN)
      IF(ABS(VL-HL).LT.0.001)GOTO 82
      IF(VL.LT.HL)GOTO 83
C     VL > HL  FEL
      WRITE(42,290)NF,II,VL,HL
      GOTO 83
   82 NC=NC+1
      EQG(NC)=II
   83 VL=0
   84 CONTINUE
C
      WRITE(42,300) NF
      WRITE(42,310) (EQG(II),II=1,NC)
      NC=0
      DO 85 II=N2+1,N2+ME
      IF(ABS(HV(II)).LT.1.E-7) GOTO 85
      NC=NC+1
      EQG(NC)=II-N2+1
   85 CONTINUE
C
      WRITE(42,320)(EQG(II),II=1,NC)
C**** SLUT P$ TEST ****
   88 CONTINUE
C
      ILP=0
      IF(NRES.EQ.0)ILP=1
      NRES=0
C
C.... CALCULATION
      print*,"LM9A call outside LM3A(1)"
      CALL LM9A (X0,F0,G0,N,NF,X,FUNK,INDEX,XREL,GREL,
     &           G3,K1,NVAR,IUT3,IRET1)
      IF (IRET1 .EQ. 1) GOTO 145
C
C.... CONTROL
      CALL LM2 (CONV,EPSI,FEPSI,F0,LASTF,LASTNF,LASTNX,LASTX,
     &          N,NCYK,START,XDELTA,X0,INDEX,LASTG,K,G0,X,G3,K1)
C
   90 CONTINUE
C.......................................................................
      NCYK=NCYK+1
      IF(NF.GT.NFMX)CONV=-1
      IF(ITR.EQ.0)GOTO 110
      I=MOD(NCYK,IABS(ITR))
      IF(I.NE.0.AND.ILP.EQ.0.AND.CONV.NE.0) GOTO 97
      IF(I.NE.0.AND.NCYK.NE.1.AND.CONV.EQ.0)GOTO 110

   97 IF(ITR)98,110,160
   98 WRITE(6,602)NCYK,NF
      IF(RES.EQ.1) WRITE(6,609)F0
      IF(IUT5.EQ.1) WRITE(6,260) Z,RES
      IF(RES.NE.1) GOTO 95
C
      WRITE(6,603)(VP,J,X(J),XC(J),J=1,NVAR)
C.... BEG. SOM LIGGER IMOT MARKERAS MED EN *
C     SE TEST AV LP-L@SNING OVAN
      DO 105 I=1,K
c      CALL LM10 (EQLP(INDG(I)),'*',1)
c      IF(ABS(HV(4*N+I)).LT.1.E-7) CALL LM10 (EQLP(INDG(I)),BL,1)
  105 CONTINUE
      WRITE(6,606)(VP2,J,G3(J),EQLP(J),GC(J),J=1,K1)
   95 IVAL=1
   99 CONTINUE
      II=0
C.... @VERLAGRING AV CENT TILL CD (F@R UTSKRIFT)
      DO 100 I=1,NVAR
c      CALL LM10 (CD(I),' .',1)
      IF(XREL(I).EQ.0.)GOTO 100
      II=II+1
c      CALL LM10 (CD(I),' F',1)
c      IF(CENT(II)) CALL LM10 (CD(I),' C',1)
  100 CONTINUE
      IF(IUT4.EQ.1) WRITE(6,250)(CD(I),I=1,NVAR)
C
C.... @VERLAGRING AV AM TILL AA (F@R UTSKRIFT)
      II=0
      DO 102 I=1,NVAR
      AA(I)=0
      IF(XREL(I).EQ.0.)GOTO 102
      II=II+1
      AA(I)=AM(II)
  102 CONTINUE
      IF(IUT2.EQ.1) WRITE(6,240)(VP4,J,AA(J),J=1,NVAR)
      IF(IVAL.EQ.2) GOTO 162
      IF(IVAL.EQ.3) GOTO 182

C.... UTSKRIFT AV DERIVATABER#KNINGEN
      IF(IUT1.EQ.0)GOTO 58
      WRITE(6,210)(F(I),I=1,N)
      DO 57 I=1,K
   57 WRITE(6,220) I,(G(I,J),J=1,N)
   58 CONTINUE
C
      IF(IUT6.EQ.1)WRITE(6,270) (AMAX(II),II=1,N)
      WRITE(6,620)

      IF(CONV.EQ.0)GOTO 115
      WRITE(6,601)
      IF(CONV.EQ.(-1))WRITE(6,610)
      IF(CONV.EQ.(-2))WRITE(6,611)
      IF(CONV.GT.0)WRITE(6,612)
  104 WRITE(6,602)NCYK,NF
  103 WRITE(6,609)F0
      WRITE(6,613) CONV
      WRITE(6,603)(VP,J,X(J),XC(J),J=1,NVAR)
      WRITE(6,606)(VP2,J,G3(J),EQLP(J),GC(J),J=1,K1)
      IF(IVAL.EQ.3.OR.CONV.EQ.-3) GOTO 190
      IF(IUT7.NE.1)GOTO 110
      WRITE(6,604) (NCYK+1-I-NRES,I=1,5),(LASTF(J),J=1,5)
      DO 106 NN=1,NVAR
      WRITE(6,605)NN,(LASTX(JJ,NN),JJ=1,5)
  106 CONTINUE

      WRITE(6,620)
      DO 107 NN=1,K1
  107 WRITE(6,622)NN,(LASTG(JJ,NN),JJ=1,5)
C
      GOTO 110
C
  160 IVAL=2
      GOTO 99
  162 CALL TRYCK(X,F0,G3,NCYK,NF,CD,AA,CONV)
C

C.......................................................................

  110 IF(CONV.NE.0)GOTO 130
  115 write(*,*) "RES=",RES
      IF(RES.EQ.1)GOTO 40
      GOTO 50
C
C
C
  130 print *,"LM1A ends"
      write(*,*) "ITR=",ITR
c      write(*,*) "\nCONV=",CONV
      RETURN
C
  145 IF(ITR)170,190,180
  170 WRITE(6,624)
  180 IVAL=3
      IF(ITR)104,190,99
  182 CALL TRYCK(X,F0,G3,NCYK,NF,CD,AA,CONV)
  190 print *,"LM1A ends"
      write(*,*) "ITR=",ITR
c      write(*,*) "\nCONV=",CONV
      RETURN
C
  210 FORMAT('0DERIVATES'/' FGRAD',6X,1P,7E14.5,(/1H ,12X,1P,7E14.5))
  220 FORMAT(' GGRAD.ROW',I2,1P,7E14.5,(/1H ,11X,1P,7E14.5))
  240 FORMAT((1X,3(A2,I2,')=',1PE13.5,6X)))
  250 FORMAT(1X,'FORW.OR CENTR.DIFF.',5X,20A2)
  260 FORMAT(1H ,19X,'(LP: FDELTA=',1PE13.5,' RES=',I2,' )')
  270 FORMAT(' AMAX',7X,1P,7E14.5,(/1H ,8X,1P,7E14.5))
  280 FORMAT(1H0,'TEST OF LP-SOLUTION'/1X,19(1H*)//,
     &' X.ROW =  2 -',I3/' G.ROW =',I3,' -',I3///)
  290 FORMAT(' FAULT. NF=',I4,' E-ROW=',I3,' VL=',1PE15.8,' HL=',E15.8)
  300 FORMAT(1H0,'NF',14X,'=',I4)
  310 FORMAT(' EQ E-ROW(VL=HL) =',30I3)
  320 FORMAT(' EQ DUALA VAR.   =',30I3)
  500 FORMAT(1H0///1X,'INPUT DATA TO MINIMIZATION ROUTINE LMIN'/1X,
     &39(1H*)//1X,
     &'N      =',I10/1X,
     &'M      =',I10/1X,
     &'EPS    =',1PE10.3/1X,
     &'FEPS   =',E10.3/1X,
     &'DRV    =',1PE10.3/1X,
     &'AF     =',E10.3/1X,
     &'NFMX   =',I10/1X,
     &'ITR    =',I10)
  502 FORMAT(1X/(1X,'X(',I2,') =',1PE10.3,A2,
     & 4X,'XREL(',I2,') =',E10.3))
  510 FORMAT(1X,'PARAM(',I2,')=',1PE10.3)
  520 FORMAT(1X,'PARAM(',I2,')=',I10)
  601 FORMAT(1H0)
  602 FORMAT(1H0,'CYCLE NUMBER',I3,',',I5,' CALCULATIONS')

  603 FORMAT(1X,'VARIABLES'/(1X,3(A2,I2,')=',1PE13.5,A2,4X)))

  604 FORMAT(/1X,'RESULTS FROM THE LAST 5 CYCLES',/1X,30(1H*)//1X,
     & 'CYCLE NR.',6X,I4,4(11X,I4)//1X,'F   =',5(3X,1PE12.5)//)

  605 FORMAT(2X,1HX,I2,1H=,5(3X,1PE12.5))

  606 FORMAT(1X,'CONSTRAINTS'/(1X,3(A2,I2,')=',1PE13.5,1X,2A1,3X)))

  608 FORMAT('0INITIAL SOLUTION')

  609 FORMAT(1X,'F =',1PE13.5)

  610 FORMAT(1H0,'*** MINIMIZATION DELETED ***'//1X,
     & 'GIVEN NFMX REACHED')

  611 FORMAT(1H0,'*** MINIMIZATION DELETED ***'//1X,
     & 'LP-ALGORITHM UNSUCCESSFUL')

  612 FORMAT(1H0,'OPTIMUM INDICATED'/1X,17(1H*))
  613 FORMAT(1H0,'CONVERGENCE (ICONV) =',I2/1X)
  615 FORMAT(1X/(1X,3(A5,I2,')=',1PE12.5,3X)))
  620 FORMAT(1X)
  622 FORMAT(2X,1HG,I2,1H=,5(3X,1PE12.5))
  624 FORMAT(1H0,'MINIMIZATION DELETED FROM'/
     &          1X,'SUBROUTINE FUNK'/1X,25(1H*)//)
  626 FORMAT(1H0,'*** MINIMIZATION DELETED ***'//1X,
     & 'HV-VECTOR TOO SMALL , MUST BE AT LEAST:',I10)
      END