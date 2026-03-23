'************************************
'
' Tres en raya
'
' A. Tamairon. 2025
'
'************************************
Option explicit

'Variables globales
Dim tblro(9) As string
Dim wi(7,2) As integer
Dim finjuego As integer
Dim numju As integer
Dim k As integer
Dim x As integer
Dim y As integer
Dim i As integer
Dim n As integer
Dim ju$ As string
Dim ke$ As string
Dim turnoIA As integer
Dim win$ As string
Dim IA_Move As integer
Dim quien As integer
Dim ptosX,ptosO As integer

Data 0,1,2 'fila 1
Data 3,4,5 'fila 2
Data 6,7,8 'fila 3
Data 0,3,6 'col 1
Data 1,4,7 'col 2
Data 2,5,8 'col 3
Data 0,4,8 'dia \
Data 2,4,6 'dia /



'Funciones IA
Function minimax(jug$ As string,alpha As integer, beta As integer)
 Local gan$ As string
 Local i,j,score,mejorScore
 Local copia(8) As string
 Local ori$

  gan$ = ganador()
  If gan$="O" Then
   minimax=1
   Exit Function
  End If

  If gan$="X" Then
   minimax=-1
   Exit Function
  End If

  If empate()=1 Then
   minimax=0
   Exit Function
  End If

 'turno IA
 If jug$="O" Then
  mejorScore=-1000
  For i=0 To 8
   If tblro(i)<>"X" And tblro(i)<>"O" Then
    ori$=tblro(i)
    tblro(i)="O"
    score = minimax("X",alpha,beta)
    tblro(i)=ori$

    If score>alpha Then alpha=score
    If beta<=alpha Then Exit For
   End If
  Next i
  minimax=alpha
  Exit Function
 End If

 'turno humano
 mejorScore=1000
 For i=0 To 8
    If tblro(i)<>"X" And tblro(i)<>"O" Then
   ori$=tblro(i)
   tblro(i)="X"
   score = minimax("O",alpha,beta)
   tblro(i)=ori$
   If score<beta Then beta=score
   If beta<=alpha Then Exit For
 End If
Next i
minimax = beta

End Function

'-----------------------------
' Elegir mejor movimiento IA
'-----------------------------
Function pesos(i As integer) As integer
 If i=4 Then pesos=3:Exit Function
 If i=0 Or i=2 Or i=6 Or i=8 Then pesos=2:Exit Function
 pesos=1
End Function

Function bonus(i As integer)As integer
 'Estilo creativo
 'elijo agresivo o defensivo
 bonus=Int(Rnd*3)

End Function

Function mejorJugada()
 Local i,j,c,score,mejorScore
 Local mejorMovimiento
 Local copia(8) As string
 Local ori2$
 Local best(8) As integer
 mejorScore=-999
 mejorMovimiento=-1
  For i=0 To 8
   'Print @(0,16),"pass: "+Str$(i)
   If tblro(i)<>"X" And      tblro(i)<>"O" Then
    ori2$=tblro(i)
    tblro(i)="O"
    'Estrategias
    score = minimax("X",-999,999)
    score=score+pesos(i)
    score=score+bonus(i)
    tblro(i)=ori2$

    If score>mejorScore Then
     mejorScore = score
     c=0
     best(c)=i
     c=c+1
     mejorMovimiento = i
    ElseIf score=mejorScore Then
     best(c)=i
     c=c+1
    End If
   End If
  Next i
  'Cogemos uno al azar
  If c>1 Then
    mejorMovimiento=best(Int(Rnd*c))
  Else
    mejorMovimiento=best(0)
  End If

  'si no eligio coger la primer libre
  If mejorMovimiento = -1 Then
   For n=0 To 8
   If tblro(i)<>"X" And      tblro(i)<>"O" Then

     mejorMovimiento=n
     Exit For
    End If
   Next n
  End If
  mejorJugada = mejorMovimiento
End Function

'Comunes
Function empate() As integer
  Local hayEmpate
  hayEmpate=1
  For n=0 To 8
    If tblro(n)=" " Then
      hayEmpate=0
      Exit For
    End If
  Next n
  empate=hayEmpate
End Function

Function ganador() As string
  Local a,b,c,i
  For i=0 To 7
   a=wi(i,0)
   b=wi(i,1)
   c=wi(i,2)

   If tblro(a)<>" " And tblro(a)=    tblro(b) And tblro(b)=tblro(c) Then
      ganador = tblro(a)
      Exit Function
    End If
  Next i

  ganador = ""

End Function

Sub imp(x As integer, y As integer,co As integer, sc As integer, txt$ As string, inve As integer)
  If inve=1 Then
  Text x,y,txt$,"LT",1,sc,RGB(black),  co
  Else
    Text x,y,txt$,"LT",1,sc,co,RGB(black)
  End If

End Sub

Sub cur(x As integer, y As integer,k As integer,inve As integer)
  Local sc,i0,j0
  sc=5:i0=60:j0=50
  imp((x*16*sc)+i0,(y*16*sc)+j0,  RGB(green),sc,tblro(k),inve)
End Sub

Sub rediTab()
  Local i0,j0,sc,i,j,v0,h0
  k=0
  i0=60
  j0=50
  sc=5
  For i=0 To 2
    For j=0 To 2
      k=(3*j)+i
      imp((i*16*sc)+i0,(j*16*sc)+j0,RGB(green),sc,tblro(k),0)
    Next j
  Next i

  v0=120
  h0=120
  For i=0 To 1
    Line v0+(i*80),20,v0+(i*80),300,1,RGB(100,100,100)
    Line 20,h0+(i*80),300,h0+(i*80),1,RGB(100,100,100)
  Next i
End Sub

Sub comprobar()
  'ahora compruebo
    win$=ganador()
    If win$<>"" Then
      imp(40,140,RGB(white),3,"Ganador: "+win$,1)
      If win$="X" Then ptosX=ptosX+1
      If win$="O" Then ptosO=ptosO+1
      finjuego=1
      Pause 2500
      CLS
      rediTab()
      Print @(90,280),"Pulsa una tecla."
      Do
      Loop Until Inkey$<>""
      Exit Sub
    End If

  If empate()=1 Then
    imp(50,140,RGB(white),3,"Empatados",1)
    finjuego=1
    Pause 2500
    CLS
    rediTab()
    Print @(90,280),"Pulsa una tecla."
    Do
    Loop Until Inkey$<>""
  End If
End Sub

'--------------------
'        Main
'--------------------

CLS
imp(87,100,RGB(CYAN),2,"3 en raya",1)
imp(99,150,RGB(190,190,190),1,"por A. Tamairon",0)
Pause (1500)

numju=2

Print @(0,270),"Quieres jugar contra  la maquina? s/n"
  Do
    ke$=Inkey$
    If ke$="s" Then
     numju=1
     ju$="X"
    End If
  Loop Until ke$="s" Or ke$="n"

Do

  Restore

  For i=0 To 7
    Read wi(i,0),wi(i,1),wi(i,2)
  Next i

  CLS
  'Limpiamos tablero

  For n=0 To 8
    tblro(n)=" "
  Next n
  'Cursor en el centro
  k=4
  x=1
  y=1
  If numju=1 Then
   'Quien empieza?
   Randomize Timer
   If (Int(Rnd*100) Mod 2)=0 Then
    'humano
    turnoIA=0
    imp(15,130,RGB(white),2,"-=- Empieza tu -=-",0)
   Else
    'IA
    turnoIA=1
    imp(15,130,RGB(white),2,"-=- Empieza IA -=-",0)
   End If
  End If
  Pause (2000)
  'Empiezan el que primero
  'pulse con su ficha
  ju$=""


  CLS
  'Redibujo tablero
  rediTab()

  imp(0,0,RGB(white),1,"PLAYER: "+ju$,0)

  '-------------------------------
  'bucle principal
  '-------------------------------
  finjuego=0
  imp(180,0,RGB(yellow),1, "X: "+Str$(ptosX)+  "  O: "+Str$(ptosO))
  Do
      'indico el prox. turno
      If ju$<>"" And numju=2 Then
        imp(0,0,RGB(white),1,"TURNO PLAYER: "+ju$,0)
      End If

      If ju$="" And numju=2 Then
        imp(0,0,RGB(white),1,"Quien empieza?",0)
      Else
        imp(0,0,RGB(white),1,"TURNO PLAYER",0)
      End If


    If turnoIA=1 Then
     imp(0,0,RGB(white),1,"TURNO IA        ",0)
     'Juega la IA
     IA_Move = mejorJugada()
     'Print @(150,0),"IA_Move = "+Str$(IA_Move)
     'Registro movimiento
     tblro(IA_Move)="O"
     ju$="X"
     'Forzamos espera para humanizar
     Pause (Int(Rnd*(3000-1000+1))+1000)

     'Acaba el turno de la IA
     turnoIA=0

     rediTab()
     'Comprobar ganador
     comprobar()
    End If

    ke$=Inkey$

    'para evitar parpadeo solo redibujo
    'al moverme
    If Asc(ke$)>=128 And     Asc(ke$)<=131 Then cur(x,y,k,0)

    'Verifico que la casilla este libre
    'y que se pulsa el simbolo del jgador
    'en turno
    If tblro(k)=" " Then

      'Auto selec. de quien empieza
      If ju$="" Then
        If ke$="x" Or ke$="o" Then
          ju$=UCase$(ke$)
        End If
      End If

      If ke$="x" And ju$="X" Then
       tblro(k)="X"
       ju$="O"
       If numju=1 Then
        turnoIA=1
       Else
        turnoIA=0
       End If
      End If

      If ke$="o" And ju$="O" Then tblro(k)="O":ju$="X"

      'Comprobar ganador
      comprobar()

    End If

    'teclas de mov. del cursor
    If ke$=Chr$(128) Then y=y-1
    If ke$=Chr$(129) Then y=y+1
    If ke$=Chr$(130) Then x=x-1
    If ke$=Chr$(131) Then x=x+1

    If y>2 Then y=0
    If y<0 Then y=2
    If x>2 Then x=0
    If x<0 Then x=2


    k=(3*y)+x
    cur(x,y,k,1)

  Loop Until (ke$=Chr$(27) Or finjuego=1)

Loop Until (ke$=Chr$(27))

CLS
Print @(0,0),"Gracias por jugar!"
