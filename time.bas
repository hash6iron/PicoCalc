Const dblue = RGB(0,0,128)
Colour RGB(green), RGB(black)
Font 1, 3

Box 0,0,MM.HRES-1,MM.VRES/2,3,RGB(red),dblue

Do
  Text MM.HRES/2,MM.VRES/4,Time$,"CM"  ,1,4,RGB(cyan),dblue
  Text MM.HRES/2,MM.VRES*3/4,Date$,"CM"
  If Inkey$=Chr$(13) Then CLS :End
Loop
