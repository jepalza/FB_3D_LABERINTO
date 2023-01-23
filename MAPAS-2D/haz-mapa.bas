' 2011 empezado, 2017 remodelado para que funcione en FB

screen 20
color 7,0
cls

Dim Shared As Integer anchomapa,altomapa
anchomapa=50
altomapa=35

DECLARE SUB LEE()
DECLARE SUB GRABA()
DECLARE SUB ALMACENA(A As integer,B As integer,C As Integer) ' ALMACENA LOS DATOS DEL MAPA EN PANT() PARA LUEGO GRABARLOS

DIM SHARED As Integer AX,AY,AX2,AY2,AM,AM2,AB
dim shared As Integer c1(100),c2(100) ' colores del caracter a poner
dim shared As Integer el(100) ' almacen de los elementos (paredes, suelos, objetos)
dim shared As Integer xx(100,100) ' lo mismo, pero para las coordenas en pantalla
DIM SHARED As Integer CUADRADO,SUELO
DIM SHARED As Integer PANT(anchomapa*altomapa) ' anchomapax45=5382 CASILLAS DE MAPA
DIM SHARED As Integer CH ' ELEMENTO A PINTAR

Dim Shared As Integer g,f,c,d,b,a

CUADRADO=0
SUELO=0

' BORRAMOS PANTALLA Y PANT()
for g=2 to altomapa+1
for f=11 to anchomapa+10
  locate g,f:color 7,7:print " ";
  ALMACENA (G-1,F-1,1)
next:next

el(1)=32:c1(1)=7:c2(1)=7 ' PRIMER CARACTER SIEMPRE ESPACIO, (NULO=BORRADO=0=NADA=VACIO)
F=2
C=1:D=15
for a=65 to 90 ' DESDE LA "A" A LA "Z"
 el(F)=a
 c1(F)=C
 c2(F)=D
 F=F+1
 C=C+1:D=D-1
 IF C=D THEN C=C+1
 IF D=0 THEN D=15
 IF C=16 THEN C=1
NEXT
C=15:D=1
for a=97 to 122 'DESDE LA "a" A LA "z"
 el(F)=a
 c1(F)=C
 c2(F)=D
 F=F+1
 C=C-1:D=D+1
 IF C=D THEN D=D+1
 IF C=0 THEN C=15
 IF D=1 THEN D=1
NEXT
C=2:D=0
for a=48 to 57 ' DEL "0" AL "9"
 el(F)=a
 c1(F)=C
 c2(F)=D
 F=F+1
 C=C+1
NEXT
el(F)=ASC("#"):c1(F)=7:c2(F)=9 ' ESTOS DOS PARA RELLENAR UN HUECO Y QUEDE ORDENADO
F=F+1
el(F)=ASC("="):c1(F)=7:c2(F)=12
F=F+1
for a=1 to 15 ' UNOS POCOS SOLIDOS, SIN LETRA, PARA SUELOS
 el(F)=32
 c1(F)=A
 c2(F)=A
 F=F+1
NEXT


a=0
for f=2 to 33 step 2:for g=1 to 9 step 2
  a=a+1
  locate f,g:color c1(a),c2(a)
  print chr$(el(a));
  xx(f,g)=a ' guardamos la posicion para luego poder recogerla
next:next
fuera1: a=a-1
ch=1

locate 1,11:color 0,7:print "UNDO"
locate 1,16:color 0,7:print "CUADRADO"
locate 1,25:color 0,7:print "SUELO"
locate 1,31:color 0,7:print "GUARDAR"

  LEE

Dim sa As String
inicio:
locate 1,1:color 7,0:print "actual:";:color c1(ch),c2(ch)::print chr$(el(ch)):color 7,0
   sa=InKey
   sa=UCASE(sa)
   IF sa="S" THEN SUELO=1
   IF sa="C" THEN CUADRADO=1
   IF sa="B" THEN CH=1

      GETMOUSE AX,AY,AM,AB

      ax=int(ax/8)
      ay=int(ay/16)

      locate 1,40:print ay,ax,am,ab

      REM PULSAMOS BOTON
      if ab=1 then
          IF CUADRADO=2 THEN 
             IF AX2=AX AND AY2=AY THEN GOTO INICIO
             CUADRADO=0
             FOR F=AX2 TO AX
               locate ay+1,F+1:color c1(ch),c2(ch):print chr$(el(ch));
               locate ay+1+(AY2-AY),F+1:color c1(ch),c2(ch):print chr$(el(ch));
               ALMACENA (AY,F,CH):ALMACENA (AY+(AY2-AY),F,CH)
             NEXT
             FOR F=AY2 TO AY
               locate F+1,AX+1:color c1(ch),c2(ch):print chr$(el(ch));
               locate F+1,AX+1+(AX2-AX),F:color c1(ch),c2(ch):print chr$(el(ch));
               ALMACENA (F,AX,CH):ALMACENA (F,AX+(AX2-AX),CH)
             NEXT
             COLOR 7,0:LOCATE 1,90:PRINT "        "
          END IF

          IF SUELO=2 THEN 
             IF AX2=AX AND AY2=AY THEN GOTO INICIO
             SUELO=0
             FOR F=AX2 TO AX
               FOR G=AY2 TO AY
                 locate G+1,F+1:color c1(ch),c2(ch):print chr$(el(ch));
                 ALMACENA (G,F,CH)
               NEXT
             NEXT
             COLOR 7,0:LOCATE 1,90:PRINT "        "
          END IF



           rem si ax<10 elegimos elemento
           if ax<10 then
              a=xx(ay+1,ax+1)
              if a<>0 then ch=a
              goto inicio
           end if
 
           rem menus de arriba
           if ay=0 and (aX>9 and aX<14) then GOTO INICIO
           if ay=0 and (aX>14 and aX<23) then CUADRADO=1:LOCATE 1,90:PRINT "CUADRADO":GOTO INICIO
           if ay=0 and (aX>23 and aX<29) then SUELO=1:LOCATE 1,90:PRINT "SUELO":GOTO INICIO
           if ay=0 and (aX>29 and aX<37) then GRABA:GOTO INICIO
            
           if ay<1 then ay=1
           if ax<10 then ax=10
           if ay>altomapa then ay=altomapa ' evito dibujar mas alla del alto del mapa indicado
           if ax>anchomapa+9 then ax=anchomapa+9 ' evito dibujar mas alla del ancho del mapa indicado
           IF CUADRADO=1 THEN CUADRADO=2:AX2=AX:AY2=AY
           IF SUELO=1 THEN SUELO=2:AX2=AX:AY2=AY
           locate ay+1,ax+1:color c1(ch),c2(ch):print chr$(el(ch));
           ALMACENA (AY,AX,CH)
      end if

      if ab=2 then
           locate ay+1,ax+1:color 7,7:print " "
           ALMACENA (AY,AX,1)
      end if


     if sa=Chr(27) then end

goto inicio

SUB LEE()
	Dim As Integer ch2
	Dim As String sa
	OPEN "MAPA.BIN" FOR BINARY ACCESS READ AS 1
	CH=0
	sa=" "
	WHILE NOT EOF(1)
	 GET #1, CH+1,sa:PANT(CH)=ASC(sa)
	CH=CH+1
	WEND
	
	CH=0
	for g=2 to altomapa+1
	for f=11 to anchomapa+10
	    locate g,f
	    CH2=PANT(CH)
	    color c1(ch2),c2(ch2)
	    Print chr$(el(ch2));
	    CH=CH+1
	next:next
	CH=2 ' PONEMOS EL PRIMER ELEMENTO (EL 1 ES BORRAR=ESPACIO)
	COLOR 0,7
	CLOSE 1
END SUB

SUB GRABA()
	SHELL "COPY MAPA.BIN MAPA.BAK /Y >NUL"
	Dim car As UByte
	OPEN "MAPA.BIN" FOR BINARY ACCESS WRITE AS 1
	CH=0
	FOR F=0 TO (anchomapa*altomapa)-1
		car=PANT(f)
		If car=1 Then car=0 
	 	Put #1 , F+1 ,Chr(car)
	NEXT
	DO : GETMOUSE A,B,C,D :LOOP WHILE D<>0
	CLOSE 1
END SUB

SUB ALMACENA(a As integer,b As Integer ,c As integer)
  PANT( (A-1) * anchomapa + (B-10) )=c
END SUB
