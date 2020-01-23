s   0  0  0  0  A14 I
s   0  0  0  0  A14 I/A
s   0  0  0  0  A14 Y
s   0  0  0  0  A14 YN
s   0  0  0  0  A14 Y/A
s   0  0  0  0  A14 D
s   0  0  0  0  A15 NO
s   0  0  0  0  A15 L
s   0  0  0  0  A15 PM
s   0  0  0  0  A15 CF
s   0  0  0  0  A24 I
s   0  0  0  0  A24 I/A
s   0  0  0  0  A24 Y
s   0  0  0  0  A24 YN
s   0  0  0  0  A24 Y/A
s   0  0  0  0  A24 D
s   0  0  0  0  A25 NO
s   0  0  0  0  A25 L
s   0  0  0  0  A25 PM
s   0  0  0  0  A25 CF
s   0  0  0  0  A34 I
s   0  0  0  0  A34 I/A
s   0  0  0  0  A34 Y
s   0  0  0  0  A34 YN
s   0  0  0  0  A34 Y/A
s   0  0  0  0  A34 D
s   0  0  0  0  A35 NO
s   0  0  0  0  A35 L
s   0  0  0  0  A35 PM
s   0  0  0  0  A35 CF
s   0  0  0  0  A44 I
s   0  0  0  0  A44 I/A
s   0  0  0  0  A44 Y
s   0  0  0  0  A44 YN
s   0  0  0  0  A44 Y/A
s   0  0  0  0  A44 D
s   0  0  0  0  B11 50
s   0  0  0  0  B11 60
s   0  0  0  0  B12 1
s   0  0  0  0  B12 3
s   0  0  0  0  WILI 1
s   0  0  0  0  WILI 2
s   0  0  0  0  WILI 3
s   0  0  0  0  WILI 4
s   0  0  0  0  WILI 5
s   0  0  0  0  WILI 6
s   0  0  0  0  B13 CFR
s   0  0  0  0  B13 VFR
s   0  0  0  0  B31 ONAN
s   0  0  0  0  B31 ONAF
s   0  0  0  0  B31 OFAF
s   0  0  0  0  B34 YES
s   0  0  0  0  B34 NO
s   0  0  0  0  B35 YES
s   0  0  0  0  B35 NO
l   1  1  1  3  *
l   1  3 10  6  T146 Input
l   1 14  6  3  P146-1
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
l   3  1 29  3  Identification..............:
i   3 31 40  2  ID                                      
l   4  1 25  3  Terminal node. . . . . .:
i   4 27  2  0  A1
l   4 30  5  3       
i   4 36  2  0  A2
l   4 39  5  3       
i   4 45  2  0  A3
l   4 48  5  3       
i   4 54  2  0  A4
l   5  1 25  3  Rated power. . . . (MVA):
i   5 27  7  0  A12    
i   5 36  7  0  A22    
i   5 45  7  0  A32    
i   5 54  6  0  A42   
l   6  1 25  3  Rated voltage. . . .(kV):
i   6 27  7  0  A13    
i   6 36  7  0  A23    
i   6 45  7  0  A33    
i   6 54  6  0  A43   
l   7  1 25  3  Connection . . . . . . .:
i   7 27  3  0  A14
l   7 31  4  3      
i   7 36  3  0  A24
l   7 40  4  3      
i   7 45  3  0  A34
l   7 49  4  3      
i   7 54  3  0  A44
l   8  1 25  3  Regulation type. . . . .:
i   8 27  3  0  A15
l   8 31  4  3      
i   8 36  3  0  A25
l   8 40  4  3      
i   8 45  3  0  A35
l   8 49  4  3      
o   8 54  3  3  A45
l   9  1 25  3  No. of plus  steps . . .:
i   9 27  3  0  A16
l   9 31  4  3      
i   9 36  3  0  A26
l   9 40  4  3      
i   9 45  3  0  A36
l  10  1 25  3  No. of minus steps . . .:
i  10 27  3  0  A17
l  10 31  4  3      
i  10 36  3  0  A27
l  10 40  4  3      
i  10 45  3  0  A37
l  11  1 25  3  Step-size. . . . . . (%):
i  11 27  5  0  A18  
l  11 33  2  3    
i  11 36  5  0  A28  
l  11 42  2  3    
i  11 45  5  0  A38  
l  12  1 25  3  Short-circuit power(GVA):
i  12 27  6  0  A10   
l  12 34  1  3   
i  12 36  6  0  A20   
l  12 43  1  3   
i  12 45  6  0  A30   
l  12 52  1  3   
i  12 54  6  0  A40   
l  13  1 25  3  BIL. . . . . . . . .(kV):
i  13 27  5  0  A19  
l  13 33  2  3    
i  13 36  5  0  A29  
l  13 42  2  3    
i  13 45  5  0  A39  
l  13 51  2  3    
i  13 54  5  0  A49  
l  15  1  7  3  Freq..:
i  15  9  3  0  B11
l  15 13 10  3   Wdg/limb:
i  15 24  4  0  WILI
l  15 29 19  3  Cooling type......:
i  15 49  4  0  B31 
l  15 54 10  3  Cost code:
i  15 65  4  0  B18 
l  16  1  7  3  Phases:
i  16  9  3  0  B12
l  16 13 10  3   CFR/VFR :
i  16 24  4  0  B13 
l  16 29 19  3  Selfcool.ratio....:
i  16 49  4  0  B32 
l  16 54 10  3  Fix Price:
i  16 65  4  0  B23 
l  16 70  3  3  (%)
l  18  1 19  3  Re-opt. margin(%).:
i  18 21  6  0  B22   
l  18 28 11  3   EvalP0...:
i  18 40  7  0  B14    
l  19  1 19  3  Loss calc. at step:
i  19 21  6  0  A110  
l  19 28 11  3   EvalPK...:
i  19 40  7  0  B15    
l  20  1  4  3      
l  21  1 16  3  Printout(Yes/No)
l  22  1 13  3  -  Oper.xtics
i  22 15  3  0  B34
l  22 19 54  3           ...... Current Limiting Reactors(%).......  .
l  23  1 13  3  -  Det.costs 
i  23 15  3  0  B35
l  23 19 16  3           Terml1:
i  23 36  5  2  B41  
l  23 42  7  3  Terml2:
i  23 50  5  2  B42  
l  23 56  8  3   Terml3:
i  23 65  7  2  B43    
