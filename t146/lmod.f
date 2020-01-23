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
*      Subroutine LMOD
*      ---------------
*
C...   Title:  Display and modify parameters to LMIN
*
*      Written: 87-02-18 by H H¢glund  , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE LMOD(NA,NB,XA,XREL,GREL,EPS,
     &                 FEPS,DRV,AF,NFMX,ITR,PARAM)
*
       REAL      XA(30),XREL(30),GREL(96),PARAM(25),DRV,EPS,FEPS,RFMX,
     &           RTR,AF
*
       INTEGER   NG(35),I,IRC,ITR,NA,NB,NFMX
*
       DIMENSION CN(35)
       CHARACTER CN*3,P*4
*
       DATA NG/12,12,12, 0,12,12,12,12, 0,12,12,12,
     &          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     &          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
*
C--- Quantities required for LMIN routine
*
       DO 299 I=1,35
  299  WRITE(CN(I),98) I+10
*
       RTR=ITR
       RFMX=NFMX
       CALL RMOVD (RTR   ,'ITR ',1,5,0)
       CALL RMOVD (AF    ,'AF  ',1,14,12)
       CALL RMOVD (DRV   ,'DRV ',1,14,12)
       CALL RMOVD (FEPS  ,'FEPS ',1,14,12)
       CALL RMOVD (EPS   ,'EPS ',1,14,12)
       CALL RMOVD (RFMX  ,'NFMX ',1,5,0)
*
       DO 399 I=1,NA
       P='I'//CN(I)
  399  CALL RMOVD (XA(I)  ,P,1,14,12)
*
       DO 499 I=1,NA
       P='H'//CN(I)
  499  CALL RMOVD (XREL(I),P,1,14,12)
*
       DO 599 I=1,22
       P='P'//CN(I)
  599  CALL RMOVD (PARAM(I),P,1,14,12)
*
       DO 699 I=1,NB
       P='G'//CN(I)
  699  CALL RMOVD (GREL (I),P,1,14,NG(I))
*
       irc=isplnk('select',13,'cmd(clrscrn )')
*
C--- Display the input/output panel
*
       irc =isplnk('display','p146lmod')
       CONTINUE
*
C--- From dialog variables
*
       CALL DMOVR (RTR   ,'ITR ',1,5,0)
       CALL DMOVR (EPS   ,'EPS ',1,14,12)
       CALL DMOVR (FEPS  ,'FEPS ',1,14,12)
       CALL DMOVR (DRV   ,'DRV ',1,14,12)
       CALL DMOVR (AF    ,'AF  ',1,14,12)
       CALL DMOVR (RFMX  ,'NFMX ',1,5,0)
       ITR=NINT(RTR)
       NFMX=NINT(RFMX)
*
       RETURN
   98  FORMAT(I2,' ')
       END
