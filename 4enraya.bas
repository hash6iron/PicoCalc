'----------------------------------
'
'   4 EN RAYAS - HUMANO vs MAQUINA
'
'   2025. A. Tamairon
'
'----------------------------------

Dim board$(6,7)
Dim win$(6,7)
Dim c, r, turn$, move, done
Dim cx0,cx,cy,mx
Dim ptosH, ptosM
Dim cty
Randomize Timer

CLS

' Tamaqo de celda
Dim CELLW = 44
Dim CELLH = 44
Dim X0 = 30       ' margen izquierdo
Dim Y0 = 64      ' margen superior
Dim RADIUS = 20

' Inicializa tablero y marcas de  lmnea ganadora
For r = 1 To 6
  For c = 1 To 7
    board$(r,c) = " "
    win$(r,c) = " "
  Next
Next

turn$ = "X"   ' jugador empieza
done = 0

'
' Funcion: Dibujar tablero completo
'
Sub DrawBoardWin
  'CLS
  ' dibujar celdas
  For r = 1 To 6
    For c = 1 To 7
      x = X0 + (c-1)*CELLW
      y = Y0 + (r-1)*CELLH
      ' color de fondo de la celda
      ' dibujar ficha si existe
      If win$(r,c)="*" Then
       Circle x,y, RADIUS-2,1,1, RGB(YELLOW),RGB(yellow)
      End If
    Next c
  Next r
End Sub


Sub DrawBoard(type As integer)
  'CLS
  ' dibujar celdas
  For r = 1 To 6
    For c = 1 To 7
      x = X0 + (c-1)*CELLW
      y = Y0 + (r-1)*CELLH
      ' color de fondo de la celda
      If type=0 Then
      Circle x,y,RADIUS,1,, RGB(WHITE),RGB(BLACK)
      End If
      ' dibujar ficha si existe
      Select Case board$(r,c)
        Case "X"
          Circle x,y, RADIUS-2,1,1, RGB(RED),RGB(red)
        Case "O"
          Circle x,y, RADIUS-2,1,1, RGB(BLUE),RGB(blue)
        Case "*"
          Circle x,y, RADIUS-2,1,1, RGB(YELLOW),RGB(yellow)
      End Select
    Next c
  Next r
End Sub


'
' Funcion: Soltar ficha
'
Function DropPiece(col, piece$)
  For rr = 6 To 1 Step -1
    If board$(rr,col) = " " Then
       board$(rr,col) = piece$
       DropPiece = rr
       Exit Function
    EndIf
  Next
  DropPiece = 0
End Function

'
' Comprobar victoria y marcar lmnea
'
Function empate()
 Local p
 For k=1 To 6
  For j=1 To 7
   If board$(k,j)<>" " Then p=p+1
  Next j
 Next k
 If p=42 Then empate=1:Exit Function
 empate=0
End Function

Function CheckWinMark(piece$)
  'limpiar marcas
  For r = 1 To 6
    For c = 1 To 7
      win$(r,c) = " "
    Next c
  Next r

  ' Horizontal
  For r = 1 To 6
    For c = 1 To 4
      If board$(r,c)=piece$ And  board$(r,c+1)=piece$ And board$(r, c+2)=piece$ And board$(r,c+3)=piece$  Then
       For k = 0 To 3
        win$(r,c+k)="*"
       Next k
       CheckWinMark = 1
       Exit Function
      EndIf
    Next c
  Next r

  ' Vertical
  For c = 1 To 7
    For r = 1 To 3
      If board$(r,c)=piece$ And  board$(r+1,c)=piece$ And board$(r+2, c)=piece$ And board$(r+3,c)=piece$  Then
        For k = 0 To 3
         win$(r+k,c)="*"
        Next k
        CheckWinMark = 1
        Exit Function
      EndIf
    Next r
  Next c

  ' Diagonal /
  For r = 4 To 6
    For c = 1 To 4
      If board$(r,c)=piece$ And  board$(r-1,c+1)=piece$ And board$(r-2, c+2)=piece$ And board$(r-3, c+3)=piece$ Then
        For k = 0 To 3
         'board$(r-k,c+k)="*"
         win$(r-k,c+k)="*"
        Next k
        CheckWinMark = 1
        Exit Function
      EndIf
    Next c
  Next r

  ' Diagonal \
  For r = 1 To 3
    For c = 1 To 4
      If board$(r,c)=piece$ And  board$(r+1,c+1)=piece$ And board$(r+2, c+2)=piece$ And board$(r+3, c+3)=piece$ Then
        For k = 0 To 3
         win$(r+k,c+k)="*"
        Next k
        CheckWinMark = 1
        Exit Function
      EndIf
    Next c
  Next r

  CheckWinMark = 0
End Function

'
' IA simple
'
Function AIMove()
  Local r0
  For testcol = 1 To 7
    r0 = DropPiece(testcol,"O")
    If r0<>0 Then
      If CheckWinMark("O") Then  board$(r0,testcol)=" ": AIMove =  testcol: Exit Function
      board$(r0,testcol)=" "
    EndIf
  Next
  For testcol = 1 To 7
    r0 = DropPiece(testcol,"X")
    If r0<>0 Then
      If CheckWinMark("X") Then  board$(r0,testcol)=" ": AIMove =  testcol: Exit Function
      board$(r0,testcol)=" "
    EndIf
  Next
  Do
    testcol = Int(Rnd*7)+1
    r0 = DropPiece(testcol,"O")
    If r0<>0 Then board$(r0, testcol)=" ": AIMove = testcol: Exit Function
Loop
End Function

'
' Bucle principal
'
cty=20

Do
CLS

DrawBoard(0)
Text 18,cty," "+Str$(ptosH)+" ","LM",,1,RGB(white),RGB(red)
Text 284,cty," "+Str$(ptosM)+" ","LM",,1,RGB(white),RGB(blue)
Do While done = 0

  DrawBoard(1)
  If turn$ = "X" Then
    move=4
    cx0=154
    cx=cx0

    cy=cty+16
    mx=44


    Text cx0,cy,"\/","LM",,1,       RGB(red),RGB(black)
    Do
      ke$=Inkey$
      If ke$=Chr$(130) Then
       Text cx,cy,"  ","LM",,1,       RGB(white),RGB(black)
       move=move-1
       cx=cx-mx
       If cx<22 Then move=7:cx=22+6*mx
       Text cx,cy,"\/","LM",,1,       RGB(red),RGB(black)

      End If

      If ke$=Chr$(131) Then
       Text cx,cy,"  ","LM",,1,       RGB(white),RGB(black)
       move=move+1
       cx=cx+mx
       If cx>(7*mx) Then move=1:cx=22
       Text cx,cy,"\/","LM",,1,       RGB(red),RGB(black)

      End If

    Loop Until ke$=Chr$(13) Or    ke$=Chr$(129)
Text cx,cy,"  ","LM",,1,       RGB(white),RGB(black)
    r0 = DropPiece(move,"X")

    If r0 = 0 Then
     Print @(95,cty-5),"Columna llena!"
     Pause 800
     Continue DO
    End If

    'DrawBoard
    turn$="O"
    If empate()=1 Then
     Text 115,cty,"!Empate!","LM",     ,1,RGB(white),RGB(green)
     done=1
     Exit Do
    End If

    If CheckWinMark("X") Then
     done=1
     DrawBoardWin
     Text 115,cty,"!Has ganado!","LM",     ,1,RGB(white),RGB(red)
     ptosH=ptosH+1
     Exit Do
    End If

  Else

    Pause 400
    move = AIMove()
    r0 = DropPiece(move,"O")
    'DrawBoard
    turn$="X"

    If empate()=1 Then
     Text 115,cty,"!Empate!","LM",     ,1,RGB(white),RGB(green)
     done=1
     Exit Do
    End If

    If CheckWinMark("O") Then
     done=1
     DrawBoardWin
Text 90,cty,"La maquina gana...","LM",,1,RGB(white),RGB(blue)
     ptosM=ptosM+1
     Exit Do
    End If
  EndIf
  '
  'DrawBoard
Loop

Pause (5000)
done=0
turn$="X"
For k=0 To 6
For j=0 To 7
  board$(k,j)=" "
  win$(k,j)=" "
Next
Next
Loop
