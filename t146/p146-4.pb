s        0       0       0       0  F11 TMY
s        0       0       0       0  F11 TMY1
s        0       0       0       0  F11 TMY2
s        0       0       0       0  F11 TMY3
s        0       0       0       0  F11 TMY4
s        0       0       0       0  F11 TMY5
s        0       0       0       0  F11 TMY6
s        0       0       0       0  F11 TMY7
s        0       0       0       0  F11 TMY8
s        0       0       0       0  F11 TMY9
s        0       0       0       0  F11 TAA
s        0       0       0       0  F11 TAA1
s        0       0       0       0  F11 TAA2
s        0       0       0       0  F11 TAD
s        0       0       0       0  F11 TBA
s        0       0       0       0  F11 TBA1
s        0       0       0       0  F11 TBA2
s        0       0       0       0  F11 TBA3
s        0       0       0       0  F11 N
s        0       0       0       0  F11 NO
s        0       0       0       0  F12 YES
s        0       0       0       0  F12 NO
s        0       0       0       0  F31 YES
s        0       0       0       0  F31 NO
s        0       0       0       0  F34 YES
s        0       0       0       0  F34 NO
s        0       0       0       0  F35 YES
s        0       0       0       0  F35 NO
s        0       0       0       0  F41 TAA
s        0       0       0       0  F41 TAC
s        0       0       0       0  F41 TAD
s        0       0       0       0  F41 TBA
s        0       0       0       0  F41 TCA
s        0       0       0       0  F41 SL-C
s        0       0       0       0  F41 130
s        0       0       0       0  F42 D
s        0       0       0       0  F42 T
s        0       0       0       0  F42 T
s        0       0       0       0  F42 D
s        0       0       0       0  F42 T
s        0       0       0       0  F42 D
s        0       0       0       0  F42 T
s        0       0       0       0  F42 D
s        0       0       0       0  F42 T
s        0       0       0       0  F42 EY
s        0       0       0       0  F42 DY
s        0       0       0       0  F42 TY-3
s        0       0       0       0  F42 TY-1
s        0       0       0       0  F42 D
s        0       0       0       0  F42 T
s        0       0       0       0  F42 EY
s        0       0       0       0  F42 DY
s        0       0       0       0  F42 TY-3
s        0       0       0       0  F42 TY-1
s        0       0       0       0  F42 D
s        0       0       0       0  F42 T
s        0       0       0       0  F42 EY
s        0       0       0       0  F42 DY
s        0       0       0       0  F42 TY-3
s        0       0       0       0  F42 TY-1
s        0       0       0       0  F42 D
s        0       0       0       0  F42 T
s        0       0       0       0  F45 M5
s        0       0       0       0  F45 ZDKH
s        0       0       0       0  F45 PLJT
s        0       0       0       0  F45 HI-B
s        0       0       0       0  F45 EXT
s        0       0       0       0  F46 GLUED
s        0       0       0       0  F46 STEEL
s        0       0       0       0  F46 ASEC
s        0       0       0       0  F47 YES
s        0       0       0       0  F47 NO
s        0       0       0       0  F54 2/2
s        0       0       0       0  F54 4/4
s        0       0       0       0  F42 EY
s        0       0       0       0  F42 DY
s        0       0       0       0  F42 TY-1
s        0       0       0       0  F42 D
s        0       0       0       0  F42 TY-3
s        0       0       0       0  F42 T
s        0       0       0       0  TEMPLO 75
s        0       0       0       0  TEMPLO 85
s        0       0       0       0  F41 130
l   1  1  1  3  *
l   1  3 10  6  T146 Input
l   1 14  6  3  P146-4
l   1 21 10  3  *  OBJECT:
o   1 32 21  6  DSYOBJ
l   1 54 11  3   Next Panel
i   1 66  6  0  SCROLL
l   2  1  1  3  =
l   2  3  7  3  Version
o   2 11  4  3  VER
l   2 16  1  3  :
o   2 18 11  3  DVERS
l   2 30 20  3  ====================
o   2 51 28  3  DAIT
l   4  1  8  6  T A N K 
l   4 10  4  3  Type
i   4 15  4  0  F11 
l   4 20 52  3                          Insul.distance  Double-shell
l   5  1  8  3  Dim.free
l   5 10 32  6  (No=>Width     Height    Length)
l   5 43 26  3   Ph-ph    Ph-sl    Winding
l   6  1  4  3      
i   6  6  3  0  F12
l   6 10  5  3       
i   6 16  7  0  F13    
l   6 24  1  3   
i   6 26  7  0  F14    
l   6 34  1  3   
i   6 36  7  0  F15    
l   6 44  1  3   
i   6 46  6  0  F21   
l   6 53  1  3   
i   6 55  6  2  F22   
l   6 62  1  3   
i   6 64  3  0  F35
l   7  1 54  3  Distance   Distance      Tank        Sound       Field
l   8  1 56  3  Wdg-tank   Core-cover   extension    screen?     screen?
i   9  2  7  0  F23    
l   9 10  3  3     
i   9 14  7  0  F24    
l   9 22  3  3     
i   9 26  7  0  F25    
l   9 34  7  3      Y/N
i   9 42  3  0  F31
l   9 46  7  3      Y/N
i   9 54  3  0  F34
l  10  1 71  3  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
l  11  1  8  6  C O R E 
l  11 10 61  3      Wind.sup Tot. assy Laminat. Limb   Step-  Core    Top.oil
l  12  1 71  3  Series  Type  height  force(MN) quality banding Lap   blading  temprise
i  13  2  4  0  F41 
l  13  7  1  3   
i  13  9  4  0  F42 
l  13 14  1  3   
i  13 16  6  2  F43   
l  13 23  1  3   
i  13 25  6  2  F44   
l  13 32  1  3   
i  13 34  4  0  F45 
l  13 39  2  3    
i  13 42  5  0  F46  
l  13 48  1  3   
i  13 50  4  0  F47 
l  13 55  1  3   
i  13 57  4  0  F54 
l  13 62  3  3     
i  13 66  5  0  F56  
l  14  1 69  3  Overvolt   Uturn0    Hyokeadd   Cool.ducts   Loss      Stack.   Space
l  15  1  3  3  (%)
i  15  5  5  0  F51  
l  15 11  1  3   
i  15 13  7  2  F52    
l  15 21  2  3    
i  15 24  6  0  F53   
l  15 31 40  3    (if fixed)   Temp.(C)  Factor   Factor
l  16  1 32  3                                  
i  16 34  8  2  NCOOLI  
l  16 43  3  3     
i  16 47  8  2  TEMPLO  
i  16 57  7  2  STACKF 
i  16 66  7  2  SPFADJ 
l  17  1 12  6  L I M I T S 
l  18  1 24  3  Max.Mean.Wdg.Temp.rise :
i  18 26  4  2  F61 
l  18 31  6  3    degC
l  18 38 21  3   Max.Transport mass :
i  18 60  6  2  F74   
l  18 67  4  3  tons
l  19  1 24  3  Max.Top.Oil  Temp.rise :
i  19 26  4  2  F62 
l  19 31  6  3    degC
l  19 38 21  3   Max.Sound Power l. :
i  19 60  5  2  F75  
l  19 66  5  3   dB  
l  20  1 24  3  Max.Flux density . . . :
i  20 26  6  2  F63   
l  20 33  4  3  T   
l  20 38 21  3   Max.Tank length. . :
i  20 60  6  2  F71   
l  20 67  4  3  mm  
l  21  1 24  3  Max.Limb height. . . . :
i  21 26  5  2  F64  
l  21 32  5  3   mm  
l  21 38 21  3   Max.Tank width . . :
i  21 60  5  2  F72  
l  21 66  5  3   mm  
l  22  1 24  3  Min.Limb height. . . . :
i  22 26  5  2  F78  
l  22 32  5  3   mm  
l  22 38 21  3   Max.Tank height. . :
i  22 60  6  2  F73   
l  22 67  4  3  mm  
l  23  1 24  3  Max.Core diameter. . . :
i  23 26  5  2  F76  
l  23 32  5  3   mm  
l  23 38 21  3   Max.No-load losses :
i  23 60  6  2  F65   
l  23 67  4  3  kW  
l  24  1 24  3  Min.Core diameter. . . :
i  24 26  5  2  F77  
l  24 32  5  3   mm  
l  24 38 21  3   Max.   Load losses :
i  24 60  6  2  F66   
l  24 67  4  3  kW  
