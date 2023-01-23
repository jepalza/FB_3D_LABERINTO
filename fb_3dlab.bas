' lectura de texturas:
' son cuadros de 64x64 pixel, y cada pixel ocupa 4 bytes (alpha,r,g,b), por lo tanto 64x64x4=16384bytes 
' se almacenan seguidas en el binario, de 16k en 16k. en el binario se almacena como UInteger.
' un color se guarda en el binario como "BGRA" (azul, rojo, verde, alfa)
' de ese modo, al leerlo con el "GET" se lee en su orden, o sea, que queda como "ARGB"
' ejemplo, en el binario, un pixel, se ve como "BB GG RR AA", pero al leerlo se ve como "AA RR GG BB"
Dim Shared texturas(200,4096) As UInteger ' 200 texturas de 4096bytes por 4 bytes del Uinteger=10*16384
Dim As Integer e,f,g
e=0
f=1
Open "texturas.bin" For binary As 1
While Not Eof(1)
	Get #1,f,texturas(e,0),4096 ' leo una textura de golpe (4096*4 bytes de golpe)
	f+=4096*4 ' siguiente textura
	e+=1
Wend
Close 1

Randomize timer

#Include "crt/math.bi"
#Include "fbgfx.bi"

' para el multikey
Using fb


#Define screenWidth 1024
#Define screenHeight 768
#Define texWidth 64
#Define texHeight 64
#Define mapWidth 50
#Define mapHeight 35

Dim Shared worldmap (mapHeight ,mapWidth) As Integer
Dim Shared buffer (screenWidth, screenHeight) As UInteger

Dim As Double posX = 1.5, posY = 1.5  'x and y start position: esquina sup.izq. siempre
Dim As Double dirX = -1.0, dirY = 0.0 'initial direction vector
Dim As Double planeX = 0.0, planeY = 0.66 'the 2d raycaster version of camera plane
Dim As Double ctime = 0 'time of current frame
Dim As Double oldTime = 0 'time of previous frame
Dim As Integer done = 0
Dim As Integer x, y, scrw
Dim framebuffer As UInteger Ptr

Dim As Integer xmouse,ymouse, mboton, xmouseold,ymouseold, ratonmas,ratonmenos


   
'Loads the map data.
Sub LoadMapData(mapa As String)
	Dim As Integer e,f,g
	e=0
	f=0
	g=1
	dim sc As String=" "
	Open mapa For binary As 1
	While Not Eof(1)
		Get #1,g,sc
		g+=1
		worldmap (f,e)=Asc(sc)
		'if f<25 then Locate f+1,e+1:Print Chr(Asc(sc)+48)
		e+=1:If e=mapWidth Then f+=1:e=0
	Wend
	Close 1
End Sub

'Returns texture color for given texture
Function GetTextureColor(texnum As Integer, x As Integer, y As Integer, isfloor As Integer = 0) As UInteger
	Dim As UInteger pixel
	Dim As Integer twh
	
	If isfloor = 0 Then
		twh = texHeight
	Else
		twh = texWidth
	EndIf
	'RDC make sure values are in range here	
	If x < 0 Then x = 0
	If x > 63 Then x = 63
	If y < 0 Then y = 0
	If y > 63 Then y = 63
	
	' ****************************************************
	' Aqui recogemos el color del pixel segun la textura
	  pixel = texturas(texNum-1,twh * Y + X)
	' ****************************************************
	
	Return pixel
End Function

'Draws buffer to screen.
Sub DrawBuffer
    Dim As Integer x, y, scrw
    Dim framebuffer As UInteger Ptr

    framebuffer = ScreenPtr
    If framebuffer Then
        ScreenInfo scrw
        ScreenLock
        For x = LBound(buffer, 1) To UBound(buffer, 1)
            For y = LBound(buffer, 2) To UBound(buffer, 2)
            	Poke UInteger, framebuffer + (y * scrw + x), buffer(x, y)
            Next
        Next
        ScreenUnlock
    End If
End Sub


'Set up graphic screen.
ScreenRes screenWidth, screenHeight, 32
framebuffer = ScreenPtr
ScreenInfo scrw

' leo el mapa (de 50 de ancho por 35 de alto)
LoadMapData "mapa.bin"

Dim velgiro As Single=1
Dim velavance As Single=1
SetMouse(screenWidth/2,screenHeight/2,0) ' centro el raton, y lo escondo

Do
   ScreenLock
   Cls
	For x As Integer = 0 To screenWidth - 1

      'calculate ray position and direction
      Dim As Double cameraX = 2 * x / CDbl(screenWidth) - 1 'x-coordinate in camera space
      Dim As Double rayPosX = posX
      Dim As Double rayPosY = posY
      Dim As Double rayDirX = dirX + planeX * cameraX
      Dim As Double rayDirY = dirY + planeY * cameraX

      'which box of the map we're in
      Dim As Integer mapX = Int(rayPosX)
      Dim As Integer mapY = Int(rayPosY)

      '//length of ray from current position to next x or y-side
      Dim As Double sideDistX
      Dim As Double sideDistY

      'length of ray from one x or y-side to next x or y-side
      Dim As Double deltaDistX = Sqrt(1 + (rayDirY * rayDirY) / (rayDirX * rayDirX))
      Dim As Double deltaDistY = Sqrt(1 + (rayDirX * rayDirX) / (rayDirY * rayDirY))
      Dim As double perpWallDist

      '//what direction to step in x or y-direction (either +1 or -1)
      Dim As Integer stepX
      Dim As Integer stepY

      Dim As Integer hit = 0 '//was there a wall hit?
      Dim As Integer side '//was a NS or a EW wall hit?

      'calculate step and initial sideDist
      If rayDirX < 0 Then
      	stepX = -1
      	sideDistX = (rayPosX - mapX) * deltaDistX
      Else
        stepX = 1
        sideDistX = (mapX + 1.0 - rayPosX) * deltaDistX
      End If
      
      If rayDirY < 0 Then
        stepY = -1
        sideDistY = (rayPosY - mapY) * deltaDistY
      Else
        stepY = 1
        sideDistY = (mapY + 1.0 - rayPosY) * deltaDistY
      End If
      'perform DDA
      Do While (hit = 0)
        'jump to next map square, OR in x-direction, OR in y-direction
        If sideDistX < sideDistY Then
          sideDistX += deltaDistX
          mapX += stepX
          side = 0
        Else
          sideDistY += deltaDistY
          mapY += stepY
          side = 1
        End If
        'Check if ray has hit a wall
        If worldMap(mapX, mapY) > 0 Then hit = 1
      Loop

      'Calculate distance of perpendicular ray (oblique distance will give fisheye effect!)
      If side = 0 Then 
      	perpWallDist = fabs((mapX - rayPosX + (1 - stepX) / 2) / rayDirX)
      Else           
      	perpWallDist = fabs((mapY - rayPosY + (1 - stepY) / 2) / rayDirY)
      End If

      'Calculate height of line to draw on screen
      Dim As Integer lineHeight = Abs(Int(screenHeight / perpWallDist))

      'calculate lowest and highest pixel to fill in current stripe
      Dim As Integer drawStart = -lineHeight / 2 + screenHeight / 2
      If drawStart < 0 Then drawStart = 0
      Dim As Integer drawEnd = lineHeight / 2 + screenHeight / 2
      If drawEnd >= screenHeight Then drawEnd = screenHeight - 1

      'coge el numero de grafico a emplear en el texturizado, segun el mapa
      Dim As Integer texNum = worldMap(mapX, mapY)+2 ' le sumo dos, para dejar el 0 y el 1 para suelo y techo

      'calculate value of wallX
      Dim As double wallX 
      'where exactly the wall was hit
      If side = 1 Then 
      	wallX = rayPosX + ((mapY - rayPosY + (1 - stepY) / 2) / rayDirY) * rayDirX
      Else           
      	wallX = rayPosY + ((mapX - rayPosX + (1 - stepX) / 2) / rayDirX) * rayDirY
      End If
      wallX -= Floor((wallX))

      'x coordinate on the texture
      Dim As Integer texX = Int(wallX * CDbl(texWidth))
      If (side = 0) And (rayDirX > 0) Then texX = texWidth - texX - 1
      If (side = 1) And (rayDirY < 0) Then texX = texWidth - texX - 1

      For y As Integer = drawStart To drawEnd - 1
        Dim As Integer d = y * 256 - screenHeight * 128 + lineHeight * 128  '256 and 128 factors to avoid floats
        Dim As Integer texY = ((d * texHeight) / lineHeight) / 256
        'make color darker for y-sides: R, G and B byte each divided through two with a "shift" and an "and"
        If side = 1 Then
        		Poke UInteger, framebuffer + (y * scrw + x), (GetTextureColor(texNum, texX,  texY) Shr 1) And 8355711
        Else
        		Poke UInteger, framebuffer + (y * scrw + x),  GetTextureColor(texNum, texX,  texY)
        EndIf
      Next

      'FLOOR CASTING
      Dim As double floorXWall, floorYWall '//x, y position of the floor texel at the bottom of the wall

      '4 different wall directions possible
      If (side = 0) and (rayDirX > 0) Then
        floorXWall = mapX
        floorYWall = mapY + wallX
      ElseIf (side = 0) and (rayDirX < 0) Then
        floorXWall = mapX + 1.0
        floorYWall = mapY + wallX
      ElseIf (side = 1) And (rayDirY > 0) Then
        floorXWall = mapX + wallX
        floorYWall = mapY
      else
        floorXWall = mapX + wallX
        floorYWall = mapY + 1.0
      End If
      
      Dim As double distWall, distPlayer, currentDist

      distWall = perpWallDist
      distPlayer = 0.0
      if (drawEnd < 0) Then drawEnd = screenHeight '//becomes < 0 when the integer overflows
      '//draw the floor from drawEnd to the bottom of the screen
      For y As Integer = drawEnd + 1 To screenHeight  - 1
        currentDist = screenHeight / (2.0 * y - screenHeight) '//you could make a small lookup table for this instead

        Dim As double weight = (currentDist - distPlayer) / (distWall - distPlayer)
         
        Dim As double currentFloorX = weight * floorXWall + (1.0 - weight) * posX
        Dim As double currentFloorY = weight * floorYWall + (1.0 - weight) * posY
        
        Dim As Integer floorTexX, floorTexY
        floorTexX = int(currentFloorX * texWidth) Mod texWidth
        floorTexY = int(currentFloorY * texHeight) Mod texHeight 
        
        ' Suelo, textura 0
         Poke UInteger, framebuffer + (y * scrw + x), (GetTextureColor(1, floorTexX,  floorTexY, 1)  Shr 1) And 8355711
        ' techo, textura 1
        Poke UInteger, framebuffer + ((screenHeight - y) * scrw + x), GetTextureColor(2, floorTexX,  floorTexY, 1)
        
      Next
	Next

	'DrawBuffer
	'//clear the buffer instead of cls()
	'Erase buffer
	ScreenUnLock
	
   'timing for input and FPS counter
    oldTime = ctime
    ctime = Timer
    Dim As Double frameTime = ctime - oldTime '/ 1000.0 'frametime is the time this frame has taken, in seconds

    'speed modifiers
    Dim As Double moveSpeed = frameTime * velavance'5.0 'the constant value is in squares/second
    Dim As Double rotSpeed = frameTime * velgiro'3.0 'the constant value is in radians/second

    'mirar arriba-abajo
    if Multikey(SC_Q) Then
      If worldMap(int(posX + dirX * moveSpeed), int(posY)) = 0 Then posX += dirX * moveSpeed
      If worldMap(int(posX), int(posY + dirY * moveSpeed)) = 0 Then posY += dirY * moveSpeed
    End If

    if Multikey(SC_A) Then
      If worldMap(int(posX - dirX * moveSpeed), int(posY)) = 0 Then posX -= dirX * moveSpeed
      If worldMap(int(posX), int(posY - dirY * moveSpeed)) = 0 Then posY -= dirY * moveSpeed
    End If
    
    'move forward if no wall in front of you
    if MultiKey(SC_LSHIFT) Then velavance=2.5 Else velavance=1.2
    If MultiKey(SC_UP) Or Multikey(SC_W)  Then
      If worldMap(int(posX + dirX * moveSpeed), int(posY)) = 0 Then posX += dirX * moveSpeed
      If worldMap(int(posX), int(posY + dirY * moveSpeed)) = 0 Then posY += dirY * moveSpeed
    End If

    if Multikey(SC_DOWN) or Multikey(SC_S) Then
      If worldMap(int(posX - dirX * moveSpeed), int(posY)) = 0 Then posX -= dirX * moveSpeed
      If worldMap(int(posX), int(posY - dirY * moveSpeed)) = 0 Then posY -= dirY * moveSpeed
    End If
    
    'rotate to the right
    if Multikey(SC_RIGHT) Or ratonmas Then
      'both camera direction and camera plane must be rotated
      Dim As Double oldDirX = dirX
      dirX = dirX * cos(-rotSpeed) - dirY * sin(-rotSpeed)
      dirY = oldDirX * sin(-rotSpeed) + dirY * cos(-rotSpeed)
      Dim As Double oldPlaneX = planeX
      planeX = planeX * cos(-rotSpeed) - planeY * sin(-rotSpeed)
      planeY = oldPlaneX * sin(-rotSpeed) + planeY * cos(-rotSpeed)
    End If

    if Multikey(SC_LEFT) Or ratonmenos Then
      'both camera direction and camera plane must be rotated
      Dim As Double oldDirX = dirX
      dirX = dirX * cos(rotSpeed) - dirY * sin(rotSpeed)
      dirY = oldDirX * sin(rotSpeed) + dirY * cos(rotSpeed)
      Dim As Double oldPlaneX = planeX
      planeX = planeX * cos(rotSpeed) - planeY * sin(rotSpeed)
      planeY = oldPlaneX * sin(rotSpeed) + planeY * cos(rotSpeed)
    EndIf

	If MultiKey(SC_ESCAPE) Then
		done = 1
	EndIf
	
	If InKey=Chr(255)+"k" Then End ' al pulsar la "X"   
	   
	Sleep 1
		
	GetMouse(xmouse,ymouse,mboton)
	
	If ratonmas Or ratonmenos Then 
		ratonmas=0
		ratonmenos=0
		If xmouse=xmouseold Then 
			SetMouse(screenWidth/2,screenHeight/2,0) ' centro el raton, y lo escondo
			GetMouse(xmouse,ymouse,mboton)
			xmouseold=xmouse
			velgiro=1
		EndIf
	EndIf
	

	If xmouse<xmouseold Then
		velgiro=(xmouseold-xmouse)/7
		xmouseold=xmouse
		ratonmenos=1
	EndIf

	If xmouse>xmouseold Then
		velgiro=(xmouse-xmouseold)/7
		xmouseold=xmouse
		ratonmas=1
	EndIf

	
Loop Until done

End