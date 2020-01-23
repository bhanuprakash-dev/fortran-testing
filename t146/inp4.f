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
*      Subroutine INP4
*      ----------------
*
C...   Title:    Read indata for panel 4 ( P146-4 )
*
*      Written: 83-01-25 by SE Jansson      , ZKB
*      Revised: 85-12-05 by Ron Bell        , TRAFO/IK
*      Revised: 88-02-10 by Ron Bell        , TRAFO/IK
*      Revised: 89-10-02 by B-G Bladh       , SETFO/K1
*      Revised: 89-10-24 by Per Sãderberg   , SETFO/IK
*      Revised: 91-10-10 by B-G Bladh       , SETFO/TS
*        New: TEMPLO: Temperature for loss calculation (75 / 85)
*            STKFAC: Stacking factor for 130 kV CORES
*      Revised: 91-12-09 by B-G Bladh    SETFO/TS
*             MNL75 and some input data of small intereset are removed.
************************************************************************
*
       SUBROUTINE INP4(BBDISP,SCR)
*
C... Declarations
*
CC*SBBL Start of the declarations block.
*
       include'cominp.h'
*
       INTEGER I,IRC,INULL,NCOOLX
*
       REAL NULL
       EQUIVALENCE (NULL, INULL)
*
       LOGICAL BBDISP
*
       CHARACTER KEY3*8,SCR*4,CHR2*8
*
       DATA NULL /1.e-20/
*
CC*SEBL End of the declarations block.
*
C... Initialisations
*
CC*SBBL Start of initialisation block
*
       CKTANK=' '
       OPTTNK=' '
       SNDSCR=' '
       ALSHLD=' '
       CORESE=' '
       STEPLA=' '
       CORTYP=' '
       COREMA=' '
       KBAND=' '
       BLDING=' '
       CVARN=' '
       NCOOLX = INULL
*
C... Initialise T146AC(*)
*
       DO 10 I=1,2
   10  T146AC(I)=' '
*
       KEY3='       1'
       CHR2='       2'
*
CC*SEBL End of initialisation block
*
C... Read indata from the data base for panel 4 P146-4
*
	open(2,file='DATA_9220_10_inp4',status='old',action='read')

	read(2,*) CKTANK
	read(2,*) OPTTNK
	read(2,*) BTANK
	read(2,*) LTANK
	read(2,*) HTANK
	read(2,*) DPHAS
	read(2,*) DPHSL
	read(2,*) DWITA
	read(2,*) DCOCOV
	read(2,*) EXTANK
	read(2,*) SNDSCR
	read(2,*) ALSHLD
	read(2,*) (T146AC(i),i=1,2)
	read(2,*) CORESE
	read(2,*) CORTYP
	read(2,*) TWSUP
	read(2,*) QMONT
	read(2,*) COREMA
	read(2,*) KBAND
	read(2,*) STEPLA
	read(2,*) UMAXPU
	read(2,*) UTURN
	read(2,*) HYOKAD
	read(2,*) BLDING
	read(2,*) CVARN
	read(2,*) TOPDTO
	read(2,*) NCOOLX
	read(2,*) TWINDM
	read(2,*) TTOILM
	read(2,*) BLTOPM
	read(2,*) HLIMBM
	read(2,*) HLIMBN
	read(2,*) MAXP0
	read(2,*) MAXPK
	read(2,*) RLTANM
	read(2,*) BTANKM
	read(2,*) HTANKM
	read(2,*) GTRPM
	read(2,*) SOUNDM
	read(2,*) DLMBMA
	read(2,*) DLMBMI
	read(2,*) STACKF
	read(2,*) TEMPLO
	read(2,*) SPFADU
	
cC PRINT
c	write(*,*)'CKTANK=',CKTANK
c	write(*,*)'OPTTNK=',OPTTNK
c	write(*,*)'BTANK=',BTANK
c	write(*,*)'LTANK=',LTANK
c	write(*,*)'HTANK=',HTANK
c	write(*,*)'DPHAS=',DPHAS
c	write(*,*)'DPHSL=',DPHSL
c	write(*,*)'DWITA=',DWITA
c	write(*,*)'DCOCOV=',DCOCOV
c	write(*,*)'EXTANK=',EXTANK
c	write(*,*)'SNDSCR=',SNDSCR
c	write(*,*)'ALSHLD=',ALSHLD
c	write(*,*)'T146AC=',(T146AC(i),i=1,2)
c	write(*,*)'CORESE=',CORESE
c	write(*,*)'CORTYP=',CORTYP
c	write(*,*)'TWSUP=',TWSUP
c	write(*,*)'QMONT=',QMONT
c	write(*,*)'COREMA=',COREMA
c	write(*,*)'KBAND=',KBAND
c	write(*,*)'STEPLA=',STEPLA
c	write(*,*)'UMAXPU=',UMAXPU
c	write(*,*)'UTURN=',UTURN
c	write(*,*)'HYOKAD=',HYOKAD
c	write(*,*) 'BLDING=',BLDING
c	write(*,*) 'CVARN=',CVARN
c	write(*,*)'TOPDTO=',TOPDTO
c	write(*,*)'NCOOLX=',NCOOLX
c	write(*,*)'TWINDM=',TWINDM
c	write(*,*)'TTOILM=',TTOILM
c	write(*,*)'BLTOPM=',BLTOPM
c	write(*,*)'HLIMBM=',HLIMBM
c	write(*,*)'HLIMBN=',HLIMBN
c	write(*,*)'MAXP0=',MAXP0
c	write(*,*)'MAXPK=',MAXPK
c	write(*,*)'RLTANM=',RLTANM
c	write(*,*)'BTANKM=',BTANKM
c	write(*,*)'HTANKM',HTANKM
c	write(*,*)'GTRPM=',GTRPM
c	write(*,*)'SOUNDM=',SOUNDM
c	write(*,*)'DLMBMA=',DLMBMA
c	write(*,*)'DLMBMI=',DLMBMI
c	write(*,*)'STACKF=', STACKF
c	write(*,*)'TEMPLO=', TEMPLO
c	write(*,*)'SPFADU=', SPFADU

        close(2)
CC*SBBL Start of Read block
*

C... Transfer NPHAS to a dialog variable
*
 
*
*
C...   Read number of cooling ducts in core, if fixed.
*
       IF(NCOOLX.EQ.INULL) THEN
          NCOOLI= -1
       ELSE
          NCOOLI= NCOOLX
       ENDIF

CC*SON  Switch ON  structure analyser (Not all calls need be shown)
*
CC*SEBL End of Write block
*
       RETURN
       END
