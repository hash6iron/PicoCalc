Rem *********************************
Rem *                               *
Rem *         Ahorcado 1.0          *
Rem *                               *
Rem * A.Tamairon. 2025              *
Rem *                               *
Rem *********************************

Dim totalPal As integer

Sub pinta(p As integer)

  x1=200:y1=250

  If p=1 Then Line x1,y1,x1,y1-100,1,  RGB(100,100,20)
  If p=2 Then Line x1,y1-100,x1-70,y1-100,1,  RGB(100,100,20)
  If p=3 Then Line x1-70,y1-100,x1-70,  y1-90,1,RGB(100,100,20)
  If p=4 Then Circle x1-70,y1-80,10,1  ,1,RGB(100,100,20),0
  If p=5 Then Line x1-70,y1-70,x1-70,  y1-30,1,RGB(100,100,20)
  If p=6 Then Line x1-70,y1-70,x1-90,  y1-60,1,RGB(100,100,20)
  If p=7 Then Line x1-70,y1-70,x1-50,  y1-60,1,RGB(100,100,20)
  If p=8 Then Line x1-70,y1-30,x1-60,y1-10,1,RGB(100,100,20)
  If p=9 Then Line x1-70,y1-30,x1-80,  y1-10,1,RGB(100,100,20)
  If p=10 Then
    Line x1-120,y1,x1+20,y1,  1,RGB(100,100,20)
    Text x1-20,y1-80,"crack!!!","CM",1,1,    RGB(255,255,255),RGB(0,0,0)
  End If

End Sub

Function busca(le As string, p As string) As integer

  busok=0

  'Vemos si ya se uso la letra
  'para que no afecte nuevamente
  For j=1 To Len(usa$)
    If le=Mid$(usa$,j,1) Then busok=-1
  Next j

  'Si aun no se descubrio,
  'las descubro
  If busok=0 Then
    For i=1 To Len(p)
      If le=Mid$(p,i,1) Then
        'Print @((i*10)+px,py);le
  Text (i*10*sc)+px,py,le,"CM",1,sc,  RGB(white),RGB(black)
        busok=busok+1
      End If
    Next i

    'La almaceno como descubierta
    usa$=usa$+le
  End If

  'retornamos
  busca=busok

End Function


Sub marcador()

  Print @(10,76),UCase$(usa$)
  Print @(180,76),"Puntos: ";puntos

End Sub

Function escoge() As string

  Local posi As integer
  Local pal$ As string

  Randomize Timer
  posi=Int(Rnd*totalPal)
  pal$=""

  For n=0 To posi
    Read pal$
  Next n

  escoge=pal$
End Function

Rem **************
Rem      MAIN
Rem **************


palabra$=""
p$=""
puntos=0

usa$=""
usaok$=""
err=0:aci=0:px=50:py=28
win=0

GoSub leepalabras


CLS
Print @(83,100);"Juego del ahorcado"
Print @(88,135);"A.Tamairon. 2025"
Text 154,260,"Pulsa una tecla","CM",1,1,RGB(255,255,255),RGB(0,0,0)

Do
Loop Until Inkey$<>""

empieza:

CLS
Do
  Print "Introduce palabr. (min. 4 letras)"
  Print "Dejar en blanco para aleatoria"
  Input "";palabra$
  CLS

  If palabra$="" Then
   Restore
   palabra$=escoge()
  End If

Loop Until Len(palabra$)>3 And Len(palabra$)<15

Rem cuadro marcador
Rem ---------------
RBox 0,0,320,100,10,RGB(100,100,100),0
Line 0,60,320,60,1,RGB(100,100,100)

p$=UCase$(palabra$)

Rem rayas texto oculto
Rem ------------------
sc=2
px = 155 - ((1+Len(p$))*5*sc)
For i=1 To Len(p$)
  'Print @((i*10)+px,py);"_"
  Text (i*10*sc)+px,py,"_","CM",1,sc,  RGB(white),RGB(black)
Next i

Rem main loop
Rem ---------------

Do
  key$=Inkey$
  If key$<>"" And key$<>Chr$(27) Then

    'Comprobamos
    res=busca(UCase$(key$),UCase$(p$))

    If res=0 Then
     'La letra no esta en p$
      err=err+1
      'usa$=usa$+key$
      pinta(err)
    Else If res>0 Then
      'La letra esta en p$
      aci=aci+res
      'usa$=usa$+key$
      If aci=Len(p$) Then win=1
    End If

  End If
  marcador()

Loop Until key$=Chr$(27) Or err=10 Or win=1

Rem ganador
Rem -------------------

If win=1 Then
  puntos=puntos+Len(p$)
  win=0
  err=0
  marcador()
  Text 160,210,"GANASTE!!","CM",3,1,RGB(255,255,255),RGB(0,0,0)
Else
  Text 160,210,"PERDISTE!!","CM",3,1,RGB(255,25,25),RGB(0,0,0)
  puntos=0
  sc=2
  px = 155 - ((1+Len(p$))*5*sc)
  For i=1 To Len(p$)
    le$=Mid$(p$,i,1)
    'Print @((i*10)+px,py);Mid$(p$,i,1)
Text (i*10*sc)+px,py,le$,"CM",1,sc,  RGB(white),RGB(black)
  Next i
End If

Text 155,260,"Pulsa una tecla","CM",1,1,RGB(255,255,255),RGB(0,0,0)

p$=""
aci=0
usa$=""
'usaok$=""

Do
Loop Until Inkey$<>""

GoTo empieza

leepalabras:
Data abaco,abanico,abrevadero
Data abrillantador,antiguo,arista
Data avestruz,armario,aceituna,astilla
Data aceite,asado,aseo,asimetrico,asa
Data acaro,amanecer,amarillo,azul

Data bonito,bueno,barato,breva,brillo
Data bricolaje,bravo,beneficio
Data bisuteria,bricolaje,brindis

Data casa,coche,carro,camion,castillo
Data costura,cosmetica,casero,cancion
Data ceniza,cepillo,cesped,celiaco
Data cencerro,cuchara,cocina,cubierto
Data circulo,cuadrado,cubo,cuspide
Data caravana,crema,cria,crio,cristal
Data crepusculo,crater,criollo,casero
Data calculadora,carcareo,creciente
Data canario,carpeta,camino,canijo
Data cacerola,cazador,cena,cita,cifra
Data chocolate,chorizo,choco,chico
Data chimenea,charco,chisme,chuleta
Data civil

Data dado,diana,dinero,distraido,dios
Data decimo,derecha,distraido,divertido,distancia,diferente,diadema
Data despacio,delante,detras,duda,ducha
Data dormido,dolencia,duda,defensa
Data decreciente,decena,daga,dolmen
Data diverso,dorsal,diurno

Data esfera,estreno,estrategia
Data estupido,extintor,encendido
Data enfermo,enfermera,error,eructo
Data esfige,esfuerzo,escalera,estribo
Data estribillo,estricto,enganche

Data feo,formal,futuro,frio,fresco
Data furia,furioso,fuego,fermentado
Data falso,fantastico,forma,fotografia
Data fermento,fisura,fisgon,fiel,fibra
Data fiesta,finca,fino,foco,fuerza
Data fresa,frito,fritura,frigorifico
Data farmacia,falta,franja,frambuesa
Data foso,fosa,fuente,flaco,flauta
Data fatiga,felino,fertilizante

Data gato,gancho,gafas,garita,gas
Data gazpacho,granizada,gala,ganancia
Data ganzua,garganta,gaseoso,gema,gesto
Data geranio,girasol,giroscopio,ginebra
Data gigante,guisante,guiso,grito
Data geologia,ginecologo,gimnasta,goma
Data golosina,gordo,globo,gloton,genio
Data gemelos,gestacion,gusano,gusto
Data guarderia,guarro,guapo,gastado
Data guardia,guantera,guarnicion

totalPal=206

Return
