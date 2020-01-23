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
*      Subroutine ICHECK
*      -----------------
*
C...   Title:  Checks certain limitations for each UDB
*
*      Written: XX-XX-XX by A.N.Other  , XXXX
*      Revised: 85-10-18 by Ron Bell   , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell   , TRAFO/IK
*
************************************************************************
*
       SUBROUTINE ICHECK(Y,JFC,BBSTOP)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       DIMENSION USER(20),  TEXT(20)
       CHARACTER USER*12, IUSER*12, TEXT*48
*
       REAL     X(20,20), Y(20)
*
       INTEGER  ICLASS(20),I,IC,ITEST,IU,J,JFC,MAXC,NLIM,NUSER
*
       LOGICAL  BBSTOP,BBHACK
*
CC*SEBL End of the declarations block.
*
C... DATA
*
CC*SBBL Start of the DATA block.
*
       DATA (TEXT(I),I=1,10)
     &  /'Self-cooled 3-phase power exceeds licence limit ' ,
     &   'Rated 3-phase power exceeds licence limit       ' ,
     &   'Winding current exceeds licence limit           ' ,
     &   'Rated voltage exceeds licence limit             ' ,
     &   'BIL-value exceeds licence limit                 ' ,
     &   'Licence does not allow use of oil-guiding rings ' ,
     &   'Licence does not allow use of small-MVA windings' ,
     &   'Rated 3-phase power is too small                ' ,
     &   'Rated 1-phase power exceeds limit               ' ,
     &   'Rated 1-phase power,self-cooled, exceeds limit  ' /
       DATA (TEXT(I),I=11,20)
     &  /'Licence does not allow use of winding supports  ' ,
     &   'Licence does not allow use of side limbs        ' ,
     &   'Licence does not allow use of HI-B core-steel   ' ,
     &   'Licence does not allow use of ASECOND bandage   ' ,
     &   '                                                ' ,
     &   '                                                ' ,
     &   '                                                ' ,
     &   '                                                ' ,
     &   '                                                ' ,
     &   '                                                ' /
*
       DATA NUSER,NLIM,MAXC /20,14,10/
*
       DATA (USER(I),I=1,20)
     & /'ZK','WZA','UAN','UAI','UAW','UAB','UAL',13*' '/
*
       DATA (ICLASS(I),I=1,20)
     & / 20 ,  20 ,   1 ,   4 ,   5 ,   6 ,  7  ,13*0  /
*
       DATA (X(1,J),J=1,14)
     & /1000.,1000.,1.E+8,450.,1550.,4.,2.,-2.5,500.,500.,4.,4.,4.,4./
       DATA (X(2,J),J=1,14)
     & /1000.,1000.,1.E+8,450.,1550.,4.,2.,-2.5,500.,500.,4.,4.,4.,4./
       DATA (X(3,J),J=1,14)
     & / 800., 800.,1.E+8,450.,1550.,4.,4.,-2.5,300.,300.,4.,4.,4.,4./
       DATA (X(4,J),J=1,14)
     & /9999.,9999.,1.E+8,800.,2050.,4.,2.,-2.5,9999.,9999.,4.,4.,4.,4./
       DATA (X(5,J),J=1,14)
     & / 188., 188.,1.E+8,275.,1050.,4.,2.,-2.5,100.,100.,4.,2.,4.,4./
       DATA (X(6,J),J=1,14)
     & /1000.,1000.,1.E+8,800.,2050.,4.,2.,-2.5,600.,600.,4.,4.,4.,4./
       DATA (X(7,J),J=1,14)
     & /1100.,1100.,1.E+8,550.,1800.,4.,4.,-2.5,1100.,1100.,4.,4.,4.,4./
       DATA ((X(I,J),J=1,20),I=8,20)
     & /260*0./
*
CC*SEBL End of the DATA block.
*
C... Determine the user code.
*
       CALL USCOD(IUSER)
*
       BBSTOP=.FALSE.
       IU=0
       BBHACK=.TRUE.
*
C... Find the correct user.
*
       DO 299 I=1,NUSER
*
C... Carry out the checks
*
C,,, For correct user
*
       IF(IUSER.EQ.USER(I)) THEN
          IU=I
          BBHACK=.FALSE.
          IC=ICLASS(IU)
*
C... Check in sequence
*
C,,, If room left in list
*
          IF(IC.LE.MAXC) THEN
*
C... For each test
*
             DO 203 ITEST=1,NLIM
*
C... Check
*
C,,, If limit exceeded
*
             IF(Y(ITEST).GT.X(IC,ITEST)) THEN
*
C... Print text
*
C,,, If Y(ITEST) <> 0
*
                IF(ABS(Y(ITEST)).GT.1.E-16) THEN
                   BBSTOP=.TRUE.
                   WRITE (*,802) TEXT(ITEST),Y(ITEST),X(IC,ITEST)
                   WRITE (JFC,802) TEXT(ITEST),Y(ITEST),X(IC,ITEST)
                END IF
             END IF
  203        CONTINUE
          END IF
       END IF
  299  CONTINUE
*
C... Print warning
*
C,,, If not registered.
*
       IF (BBHACK) THEN
          BBSTOP=.TRUE.
          WRITE(*,800) '*** You are not a registered user *** '
          WRITE(JFC,800) '*** You are not a registered user *** '
       END IF
*
       IF (BBSTOP) WRITE (*,801)
*
       RETURN
*
  800  FORMAT('0',A)
  801  FORMAT(' ',35(/))
  802  FORMAT(' ',A48,/' ','Value = ',F12.1,'  Limit = ',F12.1,/)
*
       END
