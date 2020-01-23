l   1  1  1  3  *
l   1  3 10  6  T146 Input
l   1 14  6  3  P146-5
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
l   3  1 71  6  -----------------------------------------------------------------------
l   4  1 18  6  B O O S T E R     
l   6  1 30  3  Mass.....................(kg) 
l   6 32  1  3   
i   6 34 10  0  BOOSMA    
l   7  1 30  3  Volume...................(m3) 
l   7 32  1  3   
i   7 34 10  0  BOOSVO    
l   8  1 30  3  Equivalent resistance...(ohm) 
l   8 32  1  3   
i   8 34 10  0  BOOSRE    
l   9  1 30  3  Turns ratio............( >1 ) 
l   9 32  1  3   
i   9 34 10  0  BOOSTR    
l  10  1 71  6  -----------------------------------------------------------------------
l  12  1 35  3  Turns ratio (for the booster) = N =
l  13  1 77  3    (Current in reg.wind. without booster) / (Current in reg.wind with booster)
l  15  1 23  3  Equivalent resistance =
l  16  1 30  3    Booster as full transformer:
l  17  1 30  3                   R1 + R2/(N*N)
l  18  1 44  3         R1 = resistance in booster, down side
l  19  1 42  3         R2 = resistance in booster, up side
l  21  1 25  3    Booster autoconnected :
l  22  1 30  3                   R*(N-1)/(N*N)
l  23  1 61  3         R  = resistance in the whole winding (series + common)
