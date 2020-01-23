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
*      Subroutine HEAD1
*      ----------------
*
C...   Title:  Reading & analysis of indata for the
C...           power transformer optimisation program
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*      Revised: 91-09-05 by B-G Bladh  , SETFO/TS
*      Revised: 91-09-24 by B-G Bladh  , SETFO/TS
*        Call of print out routine for PANEL P146-5 is entered.
*      Revised: 92-02-11 by B-G Bladh  , SETFO/TS
************************************************************************
*
       SUBROUTINE HEAD1
*
C... Declarations
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1379       ,  BBERR1        ),
     &  (XZ1485       ,  IVERS         ),
     &  (XZ0581(21)   ,  CORESE        ),
     &  (XZ1486       ,  JFC           )
       EQUIVALENCE
     &  (XZ1589       ,  DATE          ),
     &  (XZ1604(1)    ,  IDENT  (1)    ),
     &  (XZ1624       ,  MNL           ),
     &  (XZ1626(1)    ,  OBJ    (1)    ),
     &  (XZ2551       ,  DVERS         ),
     &  (XZ2701       ,  EXTCOR        ),
     &  (XZ2732       ,  BBOOS         ),
     &  (XZ1378       ,  BBERR         ),
     &  (XZ2782       ,  ISTOP         ),
     &  (XZ1311(1)    ,  BBHELP(1)     )
       INTEGER   I,IVERS,JFC,NEWP,ISTOP
*
       DIMENSION IDENT(9),OBJ(6)
       CHARACTER IDENT*8,OBJ*4,DATE*26,MNL*8,DVERS*10,CORESE*4
*
       LOGICAL   BBERR1, EXTCOR, BBOOS, BBERR, BBHELP(10)
*
       BBERR1=.FALSE.
*
C... Set the output file (JFC)
*
       print*,'inside head1'
       write(*,*)JFC
       REWIND JFC
*
C... Print the heading
*
ccc    WRITE (JFC,84) '     Program: T31146, O P T R A F',DATE
ccc    WRITE (JFC,85) 'Version ',IVERS,' : ',DVERS
ccc    WRITE (JFC,86) 'Input data are stored on object ',(OBJ(I),I=1,6)
ccc    WRITE (JFC,87) 'Cost file','Identification'
ccc    WRITE (JFC,88) MNL,(IDENT(I),I=1,9)
       CALL NPAGE(0)
*
C... Conversion of indata fields to independent variables
C... with SI-system units as the dimensions
*
C... Terminal data
*
       CALL SITERM
*
C... Winding data
*
       CALL SIWIND
*
C... Active part data
C...             ( EXTCOR get values within SIACTP)
       CALL SIACTP
*
C... Tank data
*
       CALL SITANK
	
*
C... General preliminary calculations
*
       CALL PREPAR
	
*
C...   Reading external no load loss parameters, for special qualitys
*
      IF(EXTCOR) CALL NLOSSR
*
C... Checking of function codes and winding fractions
*
       CALL CHFUNK
* ...  BACKTRACKING
       IF(ISTOP.EQ.1) RETURN
*
C... Print the indata onto the output file depending on the environment
*
       NEWP=0
*
C... Print the indata onto the output file in TIME-SHARING mode
*
cc     write(*,*)'****** HEAD1  Before PRPNL ***********'
cc       CALL PRPNL(NEWP,JFC,'T146WPNL ','P146-1  ')
cc       CALL PRPNL(NEWP,JFC,'T146WPNL ','P146-2  ')
       CALL NPAGE(0)
cc       CALL PRPNL(NEWP,JFC,'T146WPNL ','P146-3  ')
cc       CALL PRPNL(NEWP,JFC,'T146WPNL ','P146-4  ')
       IF(BBOOS) THEN
          CALL NPAGE(0)
cc         CALL PRPNL(NEWP,JFC,'T146WPNL ','P146-5  ')
       ENDIF
       IF (CORESE.EQ.'TA1 ') THEN
          IF(.NOT.BBOOS) CALL NPAGE(0)
cc          CALL PRPNL(NEWP,JFC,'T146WPNL ','P146-6  ')
       ENDIF
cc     write(*,*)'****** HEAD1  After  PRPNL ***********'
*
C... Check of USERID-limitations
*
       CALL  CHECK
	
* ...  BACKTRACKING
       IF(ISTOP.EQ.1) RETURN
*
       IF((.NOT.BBHELP(10)).AND.BBERR) THEN
           STOP
       ENDIF
*
C... Calculation of the relative No. of turns
*
       CALL TURNCA
	
*
C... Calculation of currents
*
       CALL CURDIM
*
       CALL VOLTAG
*
C... Winding layout data
*
       CALL PRESET
	
*
       print*,'coming out of head1'
       RETURN
*
   84  FORMAT('  ',A33,15X,A26)
   85  FORMAT(23X,A8,I2,A3,A10)
   86  FORMAT(6X,A32,6A4)
   87  FORMAT(6X,A9,3X,A15)
   88  FORMAT(6X,A8,4X,9A8)
*
         END
