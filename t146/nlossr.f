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
*      Subroutine NLOSSR
*      -----------------
*
C...   Title: Read external variables for calculation of
C...          no load losses, active and apparent.
*
*      Written: 91-09-05 by B-G Bladh  , SETFO/TS
*      Revised: ..-..-.. by .........  ,........
*
************************************************************************
*
       SUBROUTINE NLOSSR
*
C... Declarations
*
CC*SBBL Start of the declarations block
*
       include'com1.h'
*
       EQUIVALENCE
     &  (XZ1378       ,  BBERR         ),
     &  (XZ1311(1)    ,  BBHELP(1)    ),
     &  (XZ1391       ,  BBTS          ),
     &  (XZ1486       ,  JFC           ),
     &  (XZ2701       ,  EXTCOR        ),
     &  (XZ2702(1)    ,  PSPL1(1)      ),
     &  (XZ2703(1)    ,  PSPL2(1)      ),
     &  (XZ2704(1)    ,  PSPL3(1)      ),
     &  (XZ2705(1)    ,  PSPHD1(1)     ),
     &  (XZ2706(1)    ,  PSPHD2(1)     ),
     &  (XZ2707(1)    ,  PSPHD3(1)     ),
     &  (XZ2708(1)    ,  PSPHT1(1)     ),
     &  (XZ2709(1)    ,  PSPHT2(1)     ),
     &  (XZ2710(1)    ,  PSPHT3(1)     ),
     &  (XZ2711(1)    ,  PSPTY1(1)     ),
     &  (XZ2712(1)    ,  PSPTY2(1)     ),
     &  (XZ2713(1)    ,  PSPTY3(1)     )
       EQUIVALENCE
     &  (XZ2714(1)    ,  SSPL1(1)      ),
     &  (XZ2715(1)    ,  SSPL2(1)      ),
     &  (XZ2716(1)    ,  SSPL3(1)      ),
     &  (XZ2717(1)    ,  SSPHD1(1)     ),
     &  (XZ2718(1)    ,  SSPHD2(1)     ),
     &  (XZ2719(1)    ,  SSPHD3(1)     ),
     &  (XZ2720(1)    ,  SSPHT1(1)     ),
     &  (XZ2721(1)    ,  SSPHT2(1)     ),
     &  (XZ2722(1)    ,  SSPHT3(1)     ),
     &  (XZ2723(1)    ,  SSPTY1(1)     ),
     &  (XZ2724(1)    ,  SSPTY2(1)     ),
     &  (XZ2725(1)    ,  SSPTY3(1)     ),
     &  (XZ2726       ,  LAMID         )

C---------------------------------------------------------------------
         REAL      PSPL1(3),   PSPL2(5),   PSPL3(3),
     &             PSPHD1(3),  PSPHD2(5),  PSPHD3(3),
     &             PSPHT1(3),  PSPHT2(5),  PSPHT3(3),
     &             PSPTY1(3),  PSPTY2(5),  PSPTY3(3)
         REAL      SSPL1(3),   SSPL2(5),   SSPL3(3),
     &             SSPHD1(3),  SSPHD2(5),  SSPHD3(3),
     &             SSPHT1(3),  SSPHT2(5),  SSPHT3(3),
     &             SSPTY1(3),  SSPTY2(5),  SSPTY3(3)
C---------------------------------------------------------------------
         CHARACTER LAMID*44,FILE*72, cont*50
C---------------------------------------------------------------------
         INTEGER   JFC
C---------------------------------------------------------------------
         LOGICAL   BBERR, BBHELP(10), BBTS
C---------------------------------------------------------------------
       integer getprf, lcont

cc#ifdef SUNOS
cc       external getprf !$pragma C ( getprf )
cc#endif
cc*
cc       iret = getprf("BAT_LIB ", 7, cont, lcont)
       FILE = 'extcor_H1'
       open(15, file=FILE, status='old',action='read')
C
       REWIND(15)
C
C.. Read identifier for external no load data
       READ(15,80,END=98,ERR=98) LAMID
 80    FORMAT(A44)
C
       READ(15,*,END=98,ERR=98)
     &             PSPL1    ,  PSPL2    ,  PSPL3    ,
     &             PSPHD1   ,  PSPHD2   ,  PSPHD3   ,
     &             PSPHT1   ,  PSPHT2   ,  PSPHT3   ,
     &             PSPTY1   ,  PSPTY2   ,  PSPTY3   ,
     &             SSPL1    ,  SSPL2    ,  SSPL3    ,
     &             SSPHD1   ,  SSPHD2   ,  SSPHD3   ,
     &             SSPHT1   ,  SSPHT2   ,  SSPHT3   ,
     &             SSPTY1   ,  SSPTY2   ,  SSPTY3
C
       CLOSE(15)
       GO TO 99
C
  98   CALL FPRINT(2,BBERR,BBHELP,BBTS,JFC,
     &      'Reading external no loss parameters failed' )

  99   RETURN
       END
