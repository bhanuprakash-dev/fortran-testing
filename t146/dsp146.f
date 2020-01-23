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
*      Subroutine DSP146
*      -----------------
*
C...   Title:  Move indata and results for program T31146
C...           to dialog variables for the display of
C...           the results panel P146
*
*      Written: 83-01-25 by SE Jansson , ZKB
*      Revised: 83-01-25 by SE Jansson , ZKB
*      Revised: 85-09-26 by Ron Bell   , TRAFO/IK
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 89-10-02 by B-G Bladh  , SETFO/K1
*
************************************************************************
*
       SUBROUTINE DSP146(OPTION,BBRES)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'com1.h'
*
       include'cominp.h'
*
       EQUIVALENCE
     &  (XZ1379       ,  BBERR1        ),
     &  (XZ1382       ,  BBFLU         ),
     &  (XZ1393       ,  BBVFR         ),
     &  (XZ1407       ,  BMAXPU        ),
     &  (XZ1405       ,  BLIMBX        )
       EQUIVALENCE
     &  (XZ1406       ,  BLTOPX        ),
     &  (XZ1473       ,  HLMBMA        ),
     &  (XZ1478       ,  HLMBMI        ),
     &  (XZ1424       ,  DCOMAX        ),
     &  (XZ1425       ,  DCOMIN        )
*
       REAL      HLMBMA,BLIMBX,BLTOPX,BMAXPU,DCOMIN,DCOMAX,BLTOP
*
       INTEGER   IRC,NW
*
       LOGICAL   BBERR1,BBRES,BBFLU,BBVFR
*
       CHARACTER KEY3*8,OPTION*1,OPTOK*1
*
       DATA KEY3/'       1'/
*
CC*SEBL End of the declarations block.
*
C... Move internal variable values to dialog variables
*
       CALL GETRD('WINDING ','NWIND   ',KEY3,RWILI,'WILI ', 1,1,0)
       NW=NINT(RWILI)
*
C... Move internal CHARACTER variable values to dialog variables
*
       CALL CMOVD(OPTION,'Q ',1,1)
*
C... Set OPTOK (Generates the appropriate message on the results panel)
*
CC*SBBL Start of the OPTOK block.
*
       OPTOK='3'
*
C... Set OPTOK (Resonance calculated for D and T cores only)
*
C,,, IF (CORTYP.EQ.'D   '.OR.CORTYP.EQ.'T   ')
*
       IF (CORTYP.EQ.'D   '.OR.CORTYP.EQ.'T   ') THEN
          IF (.NOT.BBERR1.AND.     BBRES) OPTOK='2'
          IF (.NOT.BBERR1.AND..NOT.BBRES) OPTOK='1'
       END IF
*
CC*SEBL End of the OPTOK block.
*
C... Set OPTOK = 0 if there has been an error in the optimisation
*
CC*SBBL Start of the OPTOK override block.
*
       IF (     BBERR1) OPTOK='0'
*
CC*SEBL End of the OPTOK override block.
*
CC*SOFF Switch off structure analyser (Not all calls need be shown)
*
       CALL CMOVD(OPTOK ,'ERR ', 1,1)
*
       CALL CMOVD(REOPT ,'D9* ', 4,3)
       CALL CMOVD(DETPRE,'B35 ',1,3)
*
       CALL RMOVD(HLIMB ,'X21 ',1,5,0)
       CALL RMOVD(DCORE ,'X22 ',1,5,0)
       BLIMB=BLIMBX
       IF (BBVFR) BLIMB=BLIMB*BMAXPU
       CALL RMOVD(BLIMB,'X23 ',1,6,4)
       CALL RMOVD(USHORE,'X24 ',1,6,2)
*
       CALL RMOVD(1.E+3*HLMBMA,'X31 ',1,5,0)
       CALL RMOVD(1.E+3*HLMBMI,'X35 ',1,5,0)
       CALL RMOVD(1.E+3*DCOMIN,'X34 ',1,4,0)
       CALL RMOVD(1.E+3*DCOMAX,'X32 ',1,4,0)
*
       BLTOP=BLTOPX
       IF (BBVFR) BLTOP=BLTOP*BMAXPU
       CALL RMOVD(BLTOP,'X33 ',1,6,4)
*
       CALL RMOVD(EVALP0,'B14 ',1,6,0)
       CALL RMOVD(EVALPK,'B15 ',1,6,0)
       CALL RMOVD(MAXP0 ,'X53 ',1,6,1)
       CALL RMOVD(MAXPK ,'X54 ',1,6,1)
*
CC*SON  Switch on  structure analyser (Not all calls need be shown)
*
C... Move internal CHARACTER variable values to dialog variables
*
       CALL CMOVD(WNDOPT,'D*1 ',NW,3)
*
C... Move internal REAL variable values to dialog variables
*
       CALL RMOVD(FILLF ,'Y*2 ',NW,6,4)
*
CC*SOFF Switch off structure analyser (Not all calls need be shown)
*
       CALL RMOVD(BWIND ,'Y*3 ',NW,6,1)
       CALL RMOVD(BDUCT ,'Y*4 ',NW,5,0)
       CALL RMOVD(ACOND0,'Y*5 ',NW,8,2)
       CALL RMOVD(CURD0 ,'Y*6 ',NW,4,2)
*
CC*SON  Switch on  structure analyser (Not all calls need be shown)
*
C... Display the results panel
*
       irc =isplnk('display','p146 ')
*
C... From dialog variables
*
CC*SOFF Switch off structure analyser (Not all calls need be shown)
*
       CALL DMOVC(OPTION,'Q ',1,1)
*
       CALL DMOVC(REOPT ,'D9* ', 4,3)
       CALL DMOVC(DETPRE,'B35 ',1,3)
*
       CALL DMOVR(HLIMB ,'X21 ',1)
       CALL DMOVR(DCORE ,'X22 ',1)
       CALL DMOVR(BLIMB,'X23 ',1)
       CALL DMOVR(USHORE,'X24 ',1)
*
*
*
       CALL DMOVR(EVALP0,'B14 ',1)
       CALL DMOVR(EVALPK,'B15 ',1)
       CALL DMOVR(MAXP0 ,'X53 ',1)
       CALL DMOVR(MAXPK ,'X54 ',1)
*
CC*SON  Switch on  structure analyser (Not all calls need be shown)
*
C... Move dialog variables to internal CHARACTER variable values
*
       CALL DMOVC(WNDOPT,'D*1 ',NW,3)
*
C... Move dialog variables to internal REAL variable values
*
       CALL DMOVR(FILLF ,'Y*2 ',NW)
*
CC*SOFF Switch off structure analyser (Not all calls need be shown)
*
       CALL DMOVR(BWIND ,'Y*3 ',NW)
       CALL DMOVR(BDUCT ,'Y*4 ',NW)
       CALL DMOVR(ACOND0,'Y*5 ',NW)
       CALL DMOVR(CURD0 ,'Y*6 ',NW)
*
CC*SON  Switch on  structure analyser (Not all calls need be shown)
*
       RETURN
       END
