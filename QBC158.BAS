DECLARE SUB DsBox (pr$, v$, boxname$, success!, name$)
' QB-CAD version 1.58
' Made by George Jones and Stew Goss

DECLARE SUB DBox (pr$(), v(), acc(), num, boxname$, success)
DECLARE SUB STARTMOUSE ()
DECLARE SUB SHOWMOUSE ()
DECLARE SUB getmousecoord (BBB%, XXX%, YYY%)
DECLARE SUB MOUSEBOX (XXX1%, YYY1%, XXX2%, YYY2%)
DECLARE SUB HIDEMOUSE ()

DECLARE SUB fpaint (X1, Y1, X2, y2, x3, y3, col1, col2)
DECLARE SUB SetPal (c, r, G, b)
DECLARE SUB GetPalette (Attr%, Red%, Green%, Blue%)
DECLARE SUB button (x, y, s$)
DECLARE FUNCTION p (real, dist, mon)
DECLARE FUNCTION inside (X1, Y1, X2, y2, x3, y3, x, y)
DECLARE FUNCTION FileBox$ (title$, path$, ext$)

DECLARE FUNCTION isin (s1$, s2$)
'finds if s1$ is in s2$ eg "bob","bobo" returns 1
DECLARE FUNCTION cisin (s1$, s2$)
 'same as isin but without regard for case; "BoB","bobo" returns 1
DECLARE SUB pp (a)
DECLARE SUB mm (a)
 'these add c-like increment and decrement instead of a=a+1 you'd have a++.
 'Here it is pp a.  For decrement (a=a-1 and in c a--) it is mm a


DEFSNG A-Z
picnamebase$ = "qbr"
rendtimes = 0
registerd = 0
vers = 1.58
beta = 1
quality = 1
'1 would draw all, 2 would be every other, 3 would be every 3rd...

cursorinc = 3
vw = 1
col = 9
col2 = 1
cc = 9
stol = 10

'buttons
numb = 19
DIM btn(1 TO numb, 1 TO 2)

btn(1, 1) = 115 'create
btn(1, 2) = 90
btn(19, 1) = 130 'interactive rotate
btn(19, 2) = 90
btn(2, 1) = 145 'render
btn(2, 2) = 90
btn(3, 1) = 175 'help
btn(3, 2) = 90
btn(18, 1) = 160 'refresh
btn(18, 2) = 90
btn(4, 1) = 115 'save
btn(4, 2) = 75
btn(5, 1) = 145 'load
btn(5, 2) = 75
btn(6, 1) = 160 'export
btn(6, 2) = 75
btn(17, 1) = 175 'quit
btn(17, 2) = 75
btn(7, 1) = 115 'positive rotate
btn(7, 2) = 60
btn(8, 1) = 130 'negative rotate
btn(8, 2) = 60
btn(9, 1) = 145 'x rotate axis
btn(9, 2) = 60
btn(10, 1) = 160 'y rotate axis
btn(10, 2) = 60
btn(11, 1) = 175 'z rotate axis
btn(11, 2) = 60
btn(12, 1) = 115 'scale
btn(12, 2) = 45
btn(13, 1) = 130 'x scale axis
btn(13, 2) = 45
btn(14, 1) = 145 'y scale axis
btn(14, 2) = 45
btn(15, 1) = 160 'z scale axis
btn(15, 2) = 45
btn(16, 1) = 175 'all scale axis
btn(16, 2) = 45


v$ = STR$(vers)
v$ = LTRIM$(RTRIM$(v$))
IF vers < 1 THEN
	ver$ = "v0" + v$
ELSE
	ver$ = "v" + v$
END IF
IF beta = 1 THEN ver$ = ver$ + " beta"
CLS
COLOR 9, 0
PRINT "Welcome to Qbasic-CAD "; ver$
PRINT ""
PRINT ""
COLOR 10, 0
LOCATE 7, 1
PRINT "        ±±±±±±±   ±±±±±±         ±±±±±±        ±±        ±±±±±       "
PRINT "       ±       ±  ±±    ±       ±±    ±±      ±±±±       ±±  ±±     "
PRINT "       ±       ±  ±±   ±        ±±           ±±  ±±      ±±   ±±    "
PRINT "       ±       ±  ±±±±±   ÛÛÛÛ  ±±          ±±    ±±     ±±    ±±     "
PRINT "       ±       ±  ±±   ±        ±±         ±±±±±±±±±±    ±±    ±±   "
PRINT "       ±   ±±  ±  ±±    ±       ±±    ±±  ±±        ±±   ±±   ±±    "
PRINT "        ±±±±±±±   ±±±±±±         ±±±±±±  ±±          ±±  ±±±±±±      "
PRINT "            ±±"
COLOR 9, 0
	OPEN "qbc.cfg" FOR INPUT AS #1
	LINE INPUT #1, rec$  'Read entries from the file.
	IF rec$ = "stewiscool" THEN registerd = 1
	INPUT #1, startpath$
	CLOSE #1
OPEN "dir.tmp" FOR OUTPUT AS #1
	PRINT #1, startpath$
CLOSE #1

LOCATE 3, 1

IF registerd = 1 THEN
	PRINT ""
	PRINT "Thank you for registering"
	PRINT "you are a great person."

IF registerd <> 1 THEN
	PRINT ""
	PRINT "Please register by email"
	PRINT "at chaos01@juno.com"
	PRINT "20 second delay"
END IF
END IF

STARTMOUSE
CALL getmousecoord(b%, x%, y%)
sx = x%
sy = y%


intro:

CALL getmousecoord(b%, x%, y%)
IF b% = 1 THEN GOTO sstart
IF NOT INKEY$ = "" GOTO sstart
IF NOT sx = x% OR NOT sy = y% THEN GOTO sstart

GOTO intro



 SLEEP
sstart:

incr = .5


'Focal Length for perspective
mon = 300

v = 50

defr = 45
defs = 1.2
inc = 10
col = 9
col2 = 1
pi = 3.141592654#
c = 10
r = 70
segm = 4
sides = 8
h = 140
numv = c ^ 2
numf = (numv - c) * 2

REDIM f(1 TO numf, 1 TO 4)
REDIM v(1 TO numv, 1 TO 3)


'name$ = "noname00.b3d"

'name$ = "d:\george\qbprog\wglass.b3d"

start:
'Default settings
defr = 2
defs = 1.2
vw = 1
scx = 1
rotaxis = 1
onf = numf + 1
onv = numv + 1
fall = 1
vall = 1

SCREEN 9, , 0, 0
WINDOW (-110, 100)-(210, -100)

'GOSUB loadp
GOSUB gensphere

redr = 1
GOSUB drawit
GOSUB drawnum
redr = 0
STARTMOUSE
SHOWMOUSE

main:
'Main Routine

DO

	endit = 0
	redr = 0
	redrnums = 0
	pposx = posx
	pposy = posy
	getmousecoord BBB%, XXX%, YYY%
	posx = (300 / 640) * XXX% - 100
	posy = (-200 / 350) * YYY% + 100
	IF posx > 110 AND posy >= 40 THEN GOSUB what
	ponv = onv
       
	IF BBB% = 0 THEN holding = 0
	IF holding = 1 THEN
		IF vw = 1 THEN
		 v(onv, 1) = posx * (mon + v(onv, 3)) / mon
		 v(onv, 2) = posy * (mon + v(onv, 3)) / mon
		ELSEIF vw = 2 THEN
		 v(onv, 3) = posx * (mon + v(onv, 1)) / mon
		 v(onv, 2) = posy * (mon + v(onv, 1)) / mon
		ELSEIF vw = 3 THEN
		 v(onv, 1) = posx * (mon + v(onv, 2)) / mon
		 v(onv, 3) = posy * (mon + v(onv, 2)) / mon
		END IF
		IF posx <> pposx OR posy <> pposy THEN redr = 1
	END IF

	IF (BBB% = 1 AND (posx < 110 OR posy < 40)) THEN
		IF holding = 0 THEN
			GOSUB findclosestm
			redr = 1
			
		ELSE
			holding = 0
			redr = 1
			onv = numv + 1
		END IF
	END IF
	IF BBB% = 2 AND (posx < 110 OR posy < 40) THEN
		holding = 0
		redr = 1
	END IF
       
       
	IF infiniterotate = 1 THEN
		rot = defr * pi / 180
		GOSUB rotate
		redr = 1
	END IF


SELECT CASE INKEY$
	CASE "\"
		CLS
		INPUT "Rotation angle :", defr
		IF defr < .00001 THEN defr = .5
		GOSUB fresh
	CASE "i", "I"

		REDIM values(1)
		REDIM accepted(1, 1 TO 2)
		REDIM prompts$(1)
		accepted(1, 1) = .0001
		accepted(1, 2) = 10
		values(1) = incr
		prompts$(1) = "Increment"
		boxname$ = "Rendering Increment"
		num = 1
		CALL DBox(prompts$(), values(), accepted(), num, boxname$, success)
		IF success = 1 THEN incr = values(1)

	CASE "|"
		GOSUB render
	CASE "!"
		IF infiniterotate = 0 THEN infiniterotate = 1 ELSE infiniterotate = 0
	CASE "h", "H"
		GOSUB help
		GOSUB fresh
	CASE "c", "C"
		GOSUB creat
	CASE "s", "S"
		GOSUB save
	CASE "e", "E"
		GOSUB rawexport
	CASE "l", "L"
		GOSUB load
	CASE "1"
		IF vw <> 1 THEN
			vw = 1
			redr = 1
			redrnums = 1
		END IF
	CASE "2"
		IF vw <> 2 THEN
			vw = 2
			redr = 1
			redrnums = 1
		END IF
	CASE "3"
		IF vw <> 3 THEN
			vw = 3
			redr = 1
			redrnums = 1
		END IF
	CASE "q", "Q", CHR$(27)
		endit = 1
	

	CASE "["
		defs = defs - .1
		redrnums = 1
	CASE "]"
		defs = defs + .1
		redrnums = 1
	CASE "="
		defr = defr + 5
		redrnums = 1
       
	CASE "-"
		defr = defr - 5
		redrnums = 1
	CASE "+"
		defr = defr + 10
		redrnums = 1
	CASE "_"
		defr = defr - 10
		redrnums = 1
	CASE "r"
		infiniterotate = 0
		rot = -defr * pi / 180
		GOSUB rotate
	CASE "R"
		infiniterotate = 0
		rot = defr * pi / 180
		GOSUB rotate
	CASE ">"
		rotaxis = rotaxis + 1
		IF rotaxis = 4 THEN rotaxis = 1
		redrnums = 1
	CASE "<"
		rotaxis = rotaxis - 1
		IF rotaxis = 0 THEN rotaxis = 3
		redrnums = 1
	CASE "X"
		scx = scx + 1
		IF scx = 5 THEN scx = 1
		redrnums = 1
	CASE "a", "A"
		infiniterotate = 0
		onv = numv + 1
		onf = numf + 1
		vall = 1
		fall = 1
		redr = 1
	CASE "v"
		infiniterotate = 0
		onv = onv + 1
		IF onv = numv + 2 THEN onv = 1
		IF onv = numv + 1 THEN vall = 1
		IF onv <> numv + 1 THEN vall = 0
		fall = 1
		onf = numf + 1
		redr = 1
		redrnums = 1
	CASE "f"
		infiniterotate = 0
		onf = onf + 1
		IF onf = numf + 2 THEN onf = 1
		IF onf = numf + 1 THEN fall = 1
		IF onf <> numf + 1 THEN fall = 0
		vall = 1
		onv = numv + 1
		redr = 1
		redrnums = 1
	CASE "x"
		GOSUB sc
	CASE CHR$(0) + "H"
		'up
		dir = 1
		IF vall = 0 OR fall = 0 THEN
			GOSUB trans
		END IF
	CASE CHR$(0) + "P"
		'down
		dir = 2
		IF vall = 0 OR fall = 0 THEN
			GOSUB trans
		END IF
	CASE CHR$(0) + "K"
		'left
		dir = 3
		IF vall = 0 OR fall = 0 THEN
			GOSUB trans
		END IF
	CASE CHR$(0) + "M"
		'right
		dir = 4
		IF vall = 0 OR fall = 0 THEN
			GOSUB trans
		END IF
	CASE "P"
		mon = mon - 20
		redr = 1
		IF mon < 60 THEN
			mon = 60
			redr = 0
		END IF
		redrnums = 1
	CASE "p"
		mon = mon + 20
		redr = 1
		redrnums = 1
	CASE "~"
		GOSUB fresh
	END SELECT
	IF redr = 1 THEN GOSUB drawit
	IF redrnums = 1 THEN GOSUB drawnum
	redr = 0

LOOP UNTIL endit = 1

CLS
INPUT "Leaving QB-CAD? (Y/n)", leav$
	IF leav$ = "n" OR leav$ = "N" THEN
		GOSUB fresh
		GOTO main
		END IF
CLS

PRINT "         Thank you for using QB-CAD "; ver$; ".    "
PRINT ""
PRINT "Coments? Sugestions? Email me at sauron@laf.cioe.com"
PRINT "or email Stew at chaos01@juno.com."
PRINT ""
PRINT "To order the most recent version email qbcad@juno.com"

SLEEP

END

'End main routine

drawit:
HIDEMOUSE
LINE (-98, -98)-(98, 98), 0, BF
FOR fc = 1 TO numf STEP quality
	X1 = v(f(fc, 1), 1)
	Y1 = v(f(fc, 1), 2)
	z1 = v(f(fc, 1), 3)
	X2 = v(f(fc, 2), 1)
	y2 = v(f(fc, 2), 2)
	z2 = v(f(fc, 2), 3)
	x3 = v(f(fc, 3), 1)
	y3 = v(f(fc, 3), 2)
	z3 = v(f(fc, 3), 3)

'vw1 = xy ; vw2 = zy ; vw3 = xz

IF vw = 1 THEN
	px1 = p(X1, z1, mon)
	px2 = p(X2, z2, mon)
	px3 = p(x3, z3, mon)
	py1 = p(Y1, z1, mon)
	py2 = p(y2, z2, mon)
	py3 = p(y3, z3, mon)

	CALL fpaint(px1, py1, px2, py2, px3, py3, 9, 15)
END IF
IF vw = 2 THEN
	py1 = p(Y1, X1, mon)
	py2 = p(y2, X2, mon)
	py3 = p(y3, x3, mon)
	pz1 = -p(z1, X1, mon)
	pz2 = -p(z2, X2, mon)
	pz3 = -p(z3, x3, mon)
	CALL fpaint(pz1, py1, pz2, py2, pz3, py3, 9, 15)
END IF
IF vw = 3 THEN
	px1 = p(X1, Y1, mon)
	px2 = p(X2, y2, mon)
	px3 = p(x3, y3, mon)
	pz1 = p(z1, Y1, mon)
	pz2 = p(z2, y2, mon)
	pz3 = p(z3, y3, mon)
	CALL fpaint(px1, pz1, px2, pz2, px3, pz3, 9, 15)
END IF
NEXT fc

IF onf <> numf + 1 THEN
	fc = onf
	X1 = v(f(fc, 1), 1)
	Y1 = v(f(fc, 1), 2)
	z1 = v(f(fc, 1), 3)
	X2 = v(f(fc, 2), 1)
	y2 = v(f(fc, 2), 2)
	z2 = v(f(fc, 2), 3)
	x3 = v(f(fc, 3), 1)
	y3 = v(f(fc, 3), 2)
	z3 = v(f(fc, 3), 3)
  

IF vw = 1 THEN
	px1 = p(X1, z1, mon)
	px2 = p(X2, z2, mon)
	px3 = p(x3, z3, mon)
	py1 = p(Y1, z1, mon)
	py2 = p(y2, z2, mon)
	py3 = p(y3, z3, mon)
       
	CALL fpaint(px1, py1, px2, py2, px3, py3, 9, 15)
END IF
IF vw = 2 THEN
	py1 = p(Y1, X1, mon)
	py2 = p(y2, X2, mon)
	py3 = p(y3, x3, mon)
	pz1 = -p(z1, X1, mon)
	pz2 = -p(z2, X2, mon)
	pz3 = -p(z3, x3, mon)

	CALL fpaint(pz1, py1, pz2, py2, pz3, py3, 9, 15)
END IF
IF vw = 3 THEN
	px1 = p(X1, Y1, mon)
	px2 = p(X2, y2, mon)
	px3 = p(x3, y3, mon)
	pz1 = p(z1, Y1, mon)
	pz2 = p(z2, y2, mon)
	pz3 = p(z3, y3, mon)
       
	CALL fpaint(px1, pz1, px2, pz2, px3, pz3, 9, 15)
END IF

END IF

IF vall = 0 THEN
	X1 = v(onv, 1)
	Y1 = v(onv, 2)
	z1 = v(onv, 3)

	IF vw = 1 THEN
		px = p(X1, z1, mon)
		py = p(Y1, z1, mon)
		CIRCLE (px, py), 3, 12
	END IF
	IF vw = 2 THEN
		pz = -p(z1, X1, mon)
		py = p(Y1, X1, mon)
		CIRCLE (pz, py), 3, 12
	END IF
	IF vw = 3 THEN
		px = p(X1, Y1, mon)
		pz = p(z1, Y1, mon)
		CIRCLE (px, pz), 3, 12
	END IF
END IF


LINE (-100, 100)-(100, -100), col, B
LINE (-99, 99)-(99, -99), col2, B
'IF redrnums = 0 THEN PCOPY 0, 1
SHOWMOUSE
RETURN


rotate:
IF fall = 1 AND vall = 1 THEN
FOR vx = 1 TO numv
	x = v(vx, 1)
	y = v(vx, 2)
	z = v(vx, 3)
	IF rotaxis = 3 THEN
		v(vx, 1) = ((x * COS(rot)) - (y * SIN(rot)))
		v(vx, 2) = ((y * COS(rot)) + (x * SIN(rot)))
	END IF
	IF rotaxis = 1 THEN
		v(vx, 2) = ((y * COS(rot)) + (z * SIN(rot)))
		v(vx, 3) = ((z * COS(rot)) - (y * SIN(rot)))
	END IF
	IF rotaxis = 2 THEN
		v(vx, 1) = ((x * COS(rot)) - (z * SIN(rot)))
		v(vx, 3) = ((z * COS(rot)) + (x * SIN(rot)))
	END IF
NEXT vx
END IF

IF fall = 0 THEN

FOR dumb = 1 TO 3
	vx = f(onf, dumb)
	x = v(vx, 1)
	y = v(vx, 2)
	z = v(vx, 3)
	IF rotaxis = 3 THEN
		v(vx, 1) = ((x * COS(rot)) - (y * SIN(rot)))
		v(vx, 2) = ((y * COS(rot)) + (x * SIN(rot)))
	END IF
	IF rotaxis = 1 THEN
		v(vx, 2) = ((y * COS(rot)) + (z * SIN(rot)))
		v(vx, 3) = ((z * COS(rot)) - (y * SIN(rot)))
	END IF
	IF rotaxis = 2 THEN
		v(vx, 1) = ((x * COS(rot)) - (z * SIN(rot)))
		v(vx, 3) = ((z * COS(rot)) + (x * SIN(rot)))
	END IF
NEXT dumb
END IF

IF vall = 0 THEN
	vx = onv
	x = v(vx, 1)
	y = v(vx, 2)
	z = v(vx, 3)
	IF rotaxis = 3 THEN
		v(vx, 1) = ((x * COS(rot)) - (y * SIN(rot)))
		v(vx, 2) = ((y * COS(rot)) + (x * SIN(rot)))
	END IF
	IF rotaxis = 1 THEN
		v(vx, 2) = ((y * COS(rot)) + (z * SIN(rot)))
		v(vx, 3) = ((z * COS(rot)) - (y * SIN(rot)))
	END IF
	IF rotaxis = 2 THEN
		v(vx, 1) = ((x * COS(rot)) - (z * SIN(rot)))
		v(vx, 3) = ((z * COS(rot)) + (x * SIN(rot)))
	END IF
END IF

	GOSUB drawit
RETURN




sc:

IF vall = 1 AND fall = 1 THEN
FOR vx = 1 TO numv
	x = v(vx, 1)
	y = v(vx, 2)
	z = v(vx, 3)
	IF scx = 1 OR scx = 4 THEN v(vx, 1) = x * defs
	IF scx = 2 OR scx = 4 THEN v(vx, 2) = y * defs
	IF scx = 3 OR scx = 4 THEN v(vx, 3) = z * defs
NEXT vx
END IF

IF fall = 0 THEN
	FOR dumb = 1 TO 3
		vx = f(onf, dumb)
		x = v(vx, 1)
		y = v(vx, 2)
		z = v(vx, 3)
		IF scx = 1 OR scx = 4 THEN v(vx, 1) = x * defs
		IF scx = 2 OR scx = 4 THEN v(vx, 2) = y * defs
		IF scx = 3 OR scx = 4 THEN v(vx, 3) = z * defs
	NEXT dumb
END IF

IF vall = 0 THEN
	x = v(onv, 1)
	y = v(onv, 1)
	z = v(onv, 1)
	IF scx = 1 OR scx = 4 THEN v(onv, 1) = x * defs
	IF scx = 2 OR scx = 4 THEN v(onv, 2) = y * defs
	IF scx = 3 OR scx = 4 THEN v(onv, 3) = z * defs
END IF
GOSUB drawit
RETURN

trans:
	IF vall = 0 THEN
		IF dir = 1 THEN
			IF vw = 1 THEN v(onv, 2) = v(onv, 2) + inc
			IF vw = 2 THEN v(onv, 2) = v(onv, 2) + inc
			IF vw = 3 THEN v(onv, 3) = v(onv, 3) + inc
		END IF
		IF dir = 2 THEN
			IF vw = 1 THEN v(onv, 2) = v(onv, 2) - inc
			IF vw = 2 THEN v(onv, 2) = v(onv, 2) - inc
			IF vw = 3 THEN v(onv, 3) = v(onv, 3) - inc
		END IF
		IF dir = 3 THEN
			IF vw = 1 THEN v(onv, 1) = v(onv, 1) - inc
			IF vw = 2 THEN v(onv, 3) = v(onv, 3) + inc
			IF vw = 3 THEN v(onv, 1) = v(onv, 1) - inc
		END IF
		IF dir = 4 THEN
			IF vw = 1 THEN v(onv, 1) = v(onv, 1) + inc
			IF vw = 2 THEN v(onv, 3) = v(onv, 3) - inc
			IF vw = 3 THEN v(onv, 1) = v(onv, 1) + inc
		END IF
	END IF
	IF fall = 0 THEN
		FOR temp = 1 TO 3
			av = f(onf, temp)
			IF dir = 1 THEN
				IF vw = 1 THEN v(av, 2) = v(av, 2) + inc
				IF vw = 2 THEN v(av, 2) = v(av, 2) + inc
				IF vw = 3 THEN v(av, 3) = v(av, 3) + inc
			END IF
			IF dir = 2 THEN
				IF vw = 1 THEN v(av, 2) = v(av, 2) - inc
				IF vw = 2 THEN v(av, 2) = v(av, 2) - inc
				IF vw = 3 THEN v(av, 3) = v(av, 3) - inc
			END IF
			IF dir = 3 THEN
				IF vw = 1 THEN v(av, 1) = v(av, 1) - inc
				IF vw = 2 THEN v(av, 3) = v(av, 3) + inc
				IF vw = 3 THEN v(av, 1) = v(av, 1) - inc
			END IF
			IF dir = 4 THEN
				IF vw = 1 THEN v(av, 1) = v(av, 1) + inc
				IF vw = 2 THEN v(av, 3) = v(av, 3) - inc
				IF vw = 3 THEN v(av, 1) = v(av, 1) + inc
			END IF
		NEXT temp
	END IF
	redr = 1
RETURN
creat:
	HIDEMOUSE
	LINE (102, 98)-(198, -59), 0, BF
	SHOWMOUSE
	LOCATE 2, 60
	PRINT "Create"
	LOCATE 4, 60
	PRINT "B - Box"
	LOCATE 5, 60
	PRINT "C - Cylinder"
	LOCATE 6, 60
	PRINT "L - Lathe"
	LOCATE 7, 60
	PRINT "O - Cone"
	LOCATE 8, 60
	PRINT "S - Sphere"
	LOCATE 9, 60
	PRINT "T - Torus"
	LOCATE 10, 60
	PRINT "M - Mesh"
	LOCATE 11, 60
	PRINT "Q - Quit"
	ok = 0
	DO
		LOCATE 12, 60
		PRINT "B,C,L,O,S,T,M, Q"
		LOCATE 13, 60
		INPUT " : ", in$
		in$ = LCASE$(in$)
		SELECT CASE in$
		CASE "m"
			ok = 1
			GOSUB mesh
		CASE "s"
			ok = 1
			GOSUB sphere
		CASE "c"
			ok = 1
			GOSUB Cylinder
		CASE "b"
			ok = 1
			GOSUB box
		CASE "o"
		       ok = 1
		       GOSUB Cone
		CASE "t"
			SHELL "torus"
			name$ = "temp9999.b3d"
			GOSUB loadp
			ok = 1
		CASE "q"
			ok = 1
		CASE "l"
			SHELL "lathe"
			 CLOSE #1
				OPEN "lathe.qbc" FOR INPUT AS #1
				INPUT #1, yesno
				CLOSE #1
			
			IF yesno = 1 THEN
				name$ = "temp9999.b3d"
				GOSUB loadp
			END IF
			ok = 1
		END SELECT
	LOOP UNTIL ok = 1
	IF in$ <> "q" THEN
	 redr = 1
	 redrnums = 1
	END IF
RETURN

sphere:
		REDIM values(1 TO 2)
		REDIM accepted(1 TO 2, 1 TO 2)
		REDIM prompts$(1 TO 2)
		accepted(1, 1) = 3
		accepted(1, 2) = 40
		accepted(2, 1) = 1
		accepted(2, 2) = 100
		values(1) = 6
		values(2) = 50
		prompts$(1) = "Complexity"
		prompts$(2) = "Radius"
		boxname$ = "Sphere"
		num = 2
		CALL DBox(prompts$(), values(), accepted(), num, boxname$, success)
IF success = 1 THEN
	c = INT(values(1))
	r = values(2)

gensphere:
	'generate sphere
	numv = c ^ 2
	numf = (numv - c) * 2
	onv = numv + 1
	onf = numf + 1
	vall = 1
	fall = 1
	REDIM f(1 TO numf, 1 TO 4)
	REDIM v(1 TO numv, 1 TO 3)
	REDIM orderv(1 TO c, 1 TO c)
	yangle = -pi / 2
	y = r * SIN(yangle)
	nowv = 0
	nowf = 0
	FOR row = 1 TO c
		rad = r * COS(yangle)
		angle = 0
		FOR circ = 1 TO c
			nowv = nowv + 1
			x = COS(angle) * rad
			z = SIN(angle) * rad
			v(nowv, 1) = x
			v(nowv, 2) = y
			v(nowv, 3) = z
			orderv(row, circ) = nowv
		       
			angle = angle + ((2 * pi) / c)
		NEXT circ
		yangle = yangle + (pi / (c - 1))
		y = r * (SIN(yangle))
	NEXT row

	FOR row = 2 TO c
		FOR circ = 1 TO c
			nowf = nowf + 1
			IF circ < c THEN
				f(nowf, 1) = orderv(row, circ)
				f(nowf, 2) = orderv(row - 1, circ)
				f(nowf, 3) = orderv(row - 1, circ + 1)
			END IF
			IF circ = c THEN
				f(nowf, 1) = orderv(row, circ)
				f(nowf, 2) = orderv(row - 1, circ)
				f(nowf, 3) = orderv(row - 1, 1)
			END IF
		     
		       
			nowf = nowf + 1
			IF circ < c THEN
				f(nowf, 1) = orderv(row, circ)
				f(nowf, 2) = orderv(row - 1, circ + 1)
				f(nowf, 3) = orderv(row, circ + 1)
			END IF
			IF circ = c THEN
				f(nowf, 1) = orderv(row, circ)
				f(nowf, 2) = orderv(row - 1, 1)
				f(nowf, 3) = orderv(row, 1)
			END IF

		NEXT circ
	NEXT row
	redr = 1
	redrnums = 1
END IF
RETURN

box:
		REDIM values(1 TO 3)
		REDIM accepted(1 TO 3, 1 TO 2)
		REDIM prompts$(1 TO 3)
		accepted(1, 1) = 1
		accepted(1, 2) = 100
		accepted(2, 1) = 1
		accepted(2, 2) = 100
		accepted(3, 1) = 1
		accepted(3, 2) = 100
		values(1) = 30
		values(2) = 30
		values(3) = 30
		prompts$(1) = "Length"
		prompts$(2) = "Width"
		prompts$(3) = "Depth"
		boxname$ = "Box"
		num = 3
		CALL DBox(prompts$(), values(), accepted(), num, boxname$, success)
	IF success = 1 THEN
	l = values(1)
	w = values(2)
	d = values(3)
	numv = 8
	numf = 12
	onv = 9
	onf = 13
	vall = 1
	fall = 1
	REDIM f(1 TO numf, 1 TO 4)
	REDIM v(1 TO numv, 1 TO 3)
	REDIM xs(1 TO 8)
	REDIM ys(1 TO 8)
	REDIM zs(1 TO 8)
	xs(1) = w / 2
	xs(2) = -w / 2
	xs(3) = w / 2
	xs(4) = -w / 2
	xs(5) = w / 2
	xs(6) = -w / 2
	xs(7) = w / 2
	xs(8) = -w / 2
	ys(1) = l / 2
	ys(2) = l / 2
	ys(3) = -l / 2
	ys(4) = -l / 2
	ys(5) = l / 2
	ys(6) = l / 2
	ys(7) = -l / 2
	ys(8) = -l / 2
	zs(1) = -d / 2
	zs(2) = -d / 2
	zs(3) = -d / 2
	zs(4) = -d / 2
	zs(5) = d / 2
	zs(6) = d / 2
	zs(7) = d / 2
	zs(8) = d / 2
	FOR num = 1 TO 8
		v(num, 1) = xs(num)
		v(num, 2) = ys(num)
		v(num, 3) = zs(num)
	NEXT num
	f(1, 1) = 1
	f(1, 2) = 2
	f(1, 3) = 3
	f(2, 1) = 2
	f(2, 2) = 4
	f(2, 3) = 3
	f(3, 1) = 1
	f(3, 2) = 5
	f(3, 3) = 7
	f(4, 1) = 1
	f(4, 2) = 3
	f(4, 3) = 7
	f(5, 1) = 2
	f(5, 2) = 6
	f(5, 3) = 8
	f(6, 1) = 2
	f(6, 2) = 4
	f(6, 3) = 8
	f(7, 1) = 5
	f(7, 2) = 6
	f(7, 3) = 7
	f(8, 1) = 6
	f(8, 2) = 7
	f(8, 3) = 8
	f(9, 1) = 1
	f(9, 2) = 5
	f(9, 3) = 6
	f(10, 1) = 1
	f(10, 2) = 2
	f(10, 3) = 6
	f(11, 1) = 3
	f(11, 2) = 7
	f(11, 3) = 8
	f(12, 1) = 3
	f(12, 2) = 4
	f(12, 3) = 8
	redr = 1
	'redrnums = 1
	END IF
RETURN

Cylinder:
		REDIM values(1 TO 4)
		REDIM accepted(1 TO 4, 1 TO 2)
		REDIM prompts$(1 TO 4)
		accepted(1, 1) = 1
		accepted(1, 2) = 64
		accepted(2, 1) = 1
		accepted(2, 2) = 100
		accepted(3, 1) = 1
		accepted(3, 2) = 16
		accepted(4, 1) = 1
		accepted(4, 2) = 100
		values(1) = 12
		values(2) = 50
		values(3) = 1
		values(4) = 50
		prompts$(1) = "Sides"
		prompts$(2) = "Height"
		prompts$(3) = "Segments"
		prompts$(4) = "Radius"
		boxname$ = "Cylinder"
		num = 4
		CALL DBox(prompts$(), values(), accepted(), num, boxname$, success)
	IF success = 1 THEN
	sides = INT(values(1))
	h = values(2)
	segm = INT(values(3))
	r = values(4)
      
	'generate cylinder
	numv = (sides * (segm + 1)) + 2
	numf = 2 * (numv - 2)
	onv = numv + 1
	onf = numf + 1
	vall = 1
	fall = 1
	REDIM f(1 TO numf, 1 TO 4)
	REDIM v(1 TO numv, 1 TO 3)
	REDIM orderv(1 TO segm + 1, 1 TO sides)
	y = -h / 2
	nowv = 0
	nowf = 0
	rad = r
	FOR row = 1 TO (segm + 1)
		angle = 0
		FOR circ = 1 TO sides
			nowv = nowv + 1
		       
			x = COS(angle) * rad
			z = SIN(angle) * rad
			v(nowv, 1) = x
			v(nowv, 2) = y
			v(nowv, 3) = z
			orderv(row, circ) = nowv
		      
			angle = angle + ((2 * pi) / sides)
		NEXT circ
		y = y + (h / segm)
	NEXT row
       
	FOR row = 2 TO segm + 1
		FOR circ = 1 TO sides
			nowf = nowf + 1
			IF circ < sides THEN
				f(nowf, 1) = orderv(row, circ)
				f(nowf, 2) = orderv(row - 1, circ)
				f(nowf, 3) = orderv(row - 1, circ + 1)
			END IF
			IF circ = sides THEN
				f(nowf, 1) = orderv(row, circ)
				f(nowf, 2) = orderv(row - 1, circ)
				f(nowf, 3) = orderv(row - 1, 1)
			END IF
		       
			nowf = nowf + 1
			IF circ < sides THEN
				f(nowf, 1) = orderv(row, circ)
				f(nowf, 2) = orderv(row - 1, circ + 1)
				f(nowf, 3) = orderv(row, circ + 1)
			END IF
			IF circ = sides THEN
				f(nowf, 1) = orderv(row, circ)
				f(nowf, 2) = orderv(row - 1, 1)
				f(nowf, 3) = orderv(row, 1)
			END IF

		NEXT circ
	NEXT row
	v(nowv + 1, 1) = 0
	v(nowv + 1, 2) = -h / 2
	v(nowv + 1, 3) = 0
	v(nowv + 2, 1) = 0
	v(nowv + 2, 2) = h / 2
	v(nowv + 2, 3) = 0
	FOR vonv = 1 TO sides - 1
		nowf = nowf + 1
		f(nowf, 1) = vonv
		f(nowf, 2) = vonv + 1
		f(nowf, 3) = nowv + 1
	NEXT vonv
	nowf = nowf + 1
	vonv = sides
	f(nowf, 1) = vonv
	f(nowf, 2) = vonv + 1 - sides
	f(nowf, 3) = nowv + 1

	FOR vonv = nowv - sides + 1 TO nowv - 1
		nowf = nowf + 1
		f(nowf, 1) = vonv
		f(nowf, 2) = vonv + 1
		f(nowf, 3) = nowv + 2
	NEXT vonv
	nowf = nowf + 1
	vonv = nowv
	f(nowf, 1) = vonv
	f(nowf, 2) = vonv + 1 - sides
	f(nowf, 3) = nowv + 2
		
	redr = 1
	redrnums = 1
	END IF
RETURN

Cone:
		REDIM values(1 TO 3)
		REDIM accepted(1 TO 3, 1 TO 2)
		REDIM prompts$(1 TO 3)
		accepted(1, 1) = 1
		accepted(1, 2) = 64
		accepted(2, 1) = 1
		accepted(2, 2) = 100
		accepted(3, 1) = 1
		accepted(3, 2) = 100
		values(1) = 12
		values(2) = 75
		values(3) = 50
		prompts$(1) = "Sides"
		prompts$(2) = "Height"
		prompts$(3) = "Radius"
		boxname$ = "Cone"
		num = 3
		CALL DBox(prompts$(), values(), accepted(), num, boxname$, success)
	IF success = 1 THEN
       
	sides = INT(values(1))
	h = values(2)
	r = values(3)
	numv = sides + 2
	numf = 2 * sides
	onv = numv + 1
	onf = numf + 1
	vall = 1
	fall = 1
	REDIM f(1 TO numf, 1 TO 4)
	REDIM v(1 TO numv, 1 TO 3)
	v(1, 1) = 0
	v(1, 2) = -h / 2
	v(1, 3) = 0
	v(numv, 1) = 0
	v(numv, 2) = h / 2
	v(numv, 3) = 0
	angle = 0
	y = -h / 2
	nowf = 0
	FOR nowv = 2 TO sides + 1
		v(nowv, 1) = r * COS(angle)
		v(nowv, 2) = y
		v(nowv, 3) = r * SIN(angle)
		angle = angle + (2 * pi / sides)
	NEXT nowv
	FOR nowv = 2 TO numv - 2
		nowf = nowf + 1
		f(nowf, 1) = nowv
		f(nowf, 2) = nowv + 1
		f(nowf, 3) = 1
		nowf = nowf + 1
		f(nowf, 1) = nowv
		f(nowf, 2) = nowv + 1
		f(nowf, 3) = numv
	NEXT nowv
	nowv = numv - 1
	nowf = nowf + 1
	f(nowf, 1) = nowv
	f(nowf, 2) = 2
	f(nowf, 3) = 1
	nowf = nowf + 1
	f(nowf, 1) = nowv
	f(nowf, 2) = 2
	f(nowf, 3) = numv
	redr = 1
	'redrnums = 1
	END IF
RETURN

save:


		bad = 0
		REDIM pr$(1)
		REDIM v$(1)
		boxname$ = "Save file."
		nam$ = "untitled.b3d"
		success = 0
		CALL DsBox("Save as", nam$, boxname$, success, nam$)
OPEN "dir.tmp" FOR INPUT AS #1
	INPUT #1, path$
CLOSE #1
KILL "dir.tmp"


OPEN "stew.sav" FOR INPUT AS #1
	INPUT #1, name$
CLOSE #1
KILL "stew.sav"
name$ = path$ + name$

IF success = 1 THEN
	OPEN name$ FOR OUTPUT AS #1
	PRINT #1, numv
	PRINT #1, numf
	FOR a = 1 TO numv
		FOR b = 1 TO 3
			PRINT #1, v(a, b)
		NEXT b
	NEXT a
	FOR a = 1 TO numf
		FOR b = 1 TO 4
			PRINT #1, f(a, b)
		NEXT b
	NEXT a
       
	CLOSE #1

END IF
redr = 1
RETURN


rawexport:

LINE (101, 98)-(199, 32), 0, BF
LINE (101, 30)-(199, -54), 0, BF
       
	DO
		bad = 0
		LOCATE 11, 60
		PRINT "Save as      "
		LOCATE 12, 60
		INPUT " :", name$
		IF LEN(name$) > 12 THEN bad = 1
		lon = LEN(name$) - 3
		IF NOT MID$(name$, lon, 1) = "." THEN bad = 1
		IF bad = 1 THEN
			LOCATE 10, 60
			PRINT "invalid filename."
		END IF
	LOOP UNTIL bad = 0
       
	OPEN name$ FOR OUTPUT AS #1
	PRINT #1, "object1"

	FOR a = 1 TO numf
		p1 = v(f(a, 1), 1)
		p2 = v(f(a, 1), 2)
		p3 = v(f(a, 1), 3)
		p4 = v(f(a, 2), 1)
		p5 = v(f(a, 2), 2)
		p6 = v(f(a, 2), 3)
		p7 = v(f(a, 3), 1)
		p8 = v(f(a, 3), 2)
		p9 = v(f(a, 3), 3)

		IF ABS(p1) < .1 THEN p1 = 0
		IF ABS(p2) < .1 THEN p1 = 0
		IF ABS(p3) < .1 THEN p1 = 0
		IF ABS(p4) < .1 THEN p1 = 0
		IF ABS(p5) < .1 THEN p1 = 0
		IF ABS(p6) < .1 THEN p1 = 0
		IF ABS(p7) < .1 THEN p1 = 0
		IF ABS(p8) < .1 THEN p1 = 0
		IF ABS(p9) < .1 THEN p1 = 0
       
		PRINT #1, " "; p1; " "; p2; " "; p3; " "; p4; " "; p5; " "; p6; " "; p7; " "; p8; " "; p9
	NEXT a
	redr = 1
	redrnums = 1
	CLOSE #1
RETURN

load:
CLOSE #1
OPEN "qbc.cfg" FOR INPUT AS #2
	INPUT #2, dummy$
	INPUT #2, startpath$
CLOSE #2

name$ = FileBox$("Load", startpath$, "b3d")

IF LEN(name$) > 0 THEN
loadp:
	CLOSE #1
	OPEN name$ FOR INPUT AS #1
	INPUT #1, numv
	INPUT #1, numf
	REDIM f(1 TO numf, 1 TO 4)
	REDIM v(1 TO numv, 1 TO 3)
	FOR a = 1 TO numv
		FOR b = 1 TO 3
			INPUT #1, v(a, b)
		NEXT b
	NEXT a
	FOR a = 1 TO numf
		FOR b = 1 TO 4
			INPUT #1, f(a, b)
		NEXT b
	NEXT a
	redr = 1
	redrnums = 1
	CLOSE #1
'Default settings
vw = 1
scx = 1
rotaxis = 1
onf = numf + 1
onv = numv + 1
fall = 1
vall = 1
END IF
	IF RIGHT$(name$, 1) = "\" THEN
		DO
			hacked$ = RIGHT$(name$, 1)
			name$ = LEFT$(name$, LEN(name$) - 1)
			finalname$ = hacked$ + finalname$
		LOOP UNTIL hacked$ = "\"
		name$ = RIGHT$(finalname$, LEN(finalname$) - 1)
	END IF
RETURN


filelist:
SHELL "dir *.b3d >3dfiles.txt"
LINE (101, 98)-(199, -59), 0, BF
nfs = 0
OPEN "3dfiles.txt" FOR INPUT AS #1
REDIM namef$(1 TO 100)
DO
	done = 0
	INPUT #1, l$
	IF RIGHT$(l$, 4) = "free" THEN done = 1
	IF MID$(l$, 10, 3) = "B3D" OR MID$(l$, 10, 3) = "b3d" THEN
		nfs = nfs + 1
		namef$(nfs) = RTRIM$(LTRIM$(LEFT$(l$, 8))) + ".b3d"
	END IF
LOOP UNTIL done = 1
CLOSE #1
SHELL "erase 3dfiles.txt"
IF nfs > 30 THEN nfs = 30
onf = 1
HIDEMOUSE
GOSUB showfiles
STARTMOUSE
SHOWMOUSE

DO

 yep = 0
	sf = 0
	CALL getmousecoord(b%, x%, y%)
       
	IF b% = 1 THEN
		name$ = namef$(onf)
		yep = 1
	END IF
       
	SELECT CASE INKEY$
       
	CASE "Q"
		onf = numf + 1
		onv = numv + 1
		fall = 1
		vall = 1
		redr = 1
		drawnums = 1
		yep = 0
	CASE " "
		name$ = namef$(onf)
		yep = 1
	CASE CHR$(0) + "H"
		onf = onf - 1
		IF onf = 0 THEN onf = nfs
		sf = 1
	CASE CHR$(0) + "P"
		onf = onf + 1
		IF onf > nfs THEN onf = 1
		sf = 1
	END SELECT
       
	IF sf = 1 THEN GOSUB showfiles
LOOP UNTIL yep = 1

RETURN

showfiles:
LINE (102, 98)-(198, -50), 0, BF

FOR temp = 1 TO nfs
	IF temp = onf THEN
		LOCATE temp + 1, 58
		PRINT "¯"
	END IF
	LOCATE temp + 1, 59
	PRINT namef$(temp)

NEXT temp
RETURN
       


help:
	
	CLS
	PRINT , "         QB-CAD Help Screen"
	PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
	PRINT " C-Create object menu             ³ S-Save the current object"
	PRINT " E-Export to RAW triangle format  ³ L-Load an object"
	PRINT " R-Rotate Counterclockwise        ³ r-Rotate Clockwise"
	PRINT " X-Change scaling axis            ³ x-Scale"
	PRINT " a-Select all                     ³ v-Scroll through vertices"
	PRINT " f-Scroll through faces           ³ P-Increase Perspective"
	PRINT " p-Decrease Perspective           ³ Q-Quit"
	PRINT " I-Rendering Increment            ³ 1-Switch to Front view            "
	PRINT " 2-Switch to Left view            ³ 3-Switch to Top view "
						  
	PRINT ""
	
	PRINT "up     Move selected face/vertex 'up'"
	PRINT "down   Move selected face/vertex 'down'"
	PRINT "left   Move selected face/vertex 'left'"
	PRINT "right  Move selected face/vertex 'right'"
	PRINT "] / [  Increase/Decrease scale amount"
	PRINT "+ / -  Increase/Decrease rotation amount"
	PRINT "> / <  Change axis of rotation (x-y-z)/(z-y-x)"
	PRINT "~      Refresh screen"
	SLEEP
'        PCOPY 1, 0
	redr = 1
	redrnums = 1
RETURN

fresh:
	HIDEMOUSE
	CLS
	redr = 1
	redrnums = 1
	GOSUB drawit
	GOSUB drawnum
	redr = 0
	redrnums = 0
	SHOWMOUSE
RETURN


render:

SCREEN 13
WINDOW (0, 0)-(320, 200)

mmm = 200

SetPal 0, 0, 0, 0
SetPal 1, 63, 63, 63
SetPal 2, 45, 45, 45
SetPal 3, 20, 20, 20
SetPal 4, 63, 0, 0
SetPal 5, 0, 63, 0
SetPal 6, 0, 0, 63

pr = -1
pg = -1
pb = -1

r = 63
G = 63
b = 63

FOR n = 64 TO 127
 rc = n * (r / 64)
 gc = n * (G / 64)
 bc = n * (b / 64)
 SetPal n, rc, gc, bc
 LINE (n - 64, mmm - 38)-(n - 64, mmm - 48), n
NEXT n


LINE (2, mmm - 54)-(30, mmm - 65), 3, BF
LINE (3, mmm - 55)-(29, mmm - 64), 2, BF
LINE (4, mmm - 56)-(28, mmm - 63), 1, BF

LINE (35, mmm - 54)-(92, mmm - 65), 3, BF
LINE (36, mmm - 55)-(91, mmm - 64), 2, BF
LINE (37, mmm - 56)-(90, mmm - 63), 1, BF

COLOR 2
LOCATE 8, 2
PRINT "Ok"
LOCATE 8, 6
PRINT "Cancel"

STARTMOUSE
c = 7

LOCATE 2, 20
PRINT "Solid Color"
LOCATE 6, 9
PRINT "Dark to Light Color Range"


fin = 0
DO
 SHOWMOUSE
 IF (r <> pr OR G <> pg OR b <> pb) THEN
  HIDEMOUSE
  LINE (1, mmm - 1)-(70, mmm - 33), 3, BF
  LINE (2, mmm - 2)-(71, mmm - 34), 3, BF
  LINE (0, mmm - 0)-(69, mmm - 32), 1, BF
  LINE (1, mmm - 1)-(68, mmm - 31), 2, BF
  LINE (1, mmm - 6)-(64, mmm - 6), 0
  LINE (1, mmm - 16)-(64, mmm - 16), 0
  LINE (1, mmm - 26)-(64, mmm - 26), 0
  LINE (r - 1, mmm - 2)-(r + 1, mmm - 10), 3, BF
  LINE (G - 1, mmm - 12)-(G + 1, mmm - 20), 3, BF
  LINE (b - 1, mmm - 22)-(b + 1, mmm - 30), 3, BF
  LINE (65, mmm - 4)-(68, mmm - 8), 4, BF
  LINE (65, mmm - 14)-(68, mmm - 18), 5, BF
  LINE (65, mmm - 24)-(68, mmm - 28), 6, BF
  SetPal c, r, G, b
  LINE (78, mmm - 0)-(150, mmm - 33), 7, BF
 
  pr = r
  pg = G
  pb = b
 
  FOR cc = 64 TO 127
   rc = (cc - 64) * (r / 64)
   gc = (cc - 64) * (G / 64)
   bc = (cc - 64) * (b / 64)
   SetPal cc, rc, gc, bc
  NEXT cc

 SHOWMOUSE
 END IF

 getmousecoord bb%, xx%, yy%
 xx% = (320 / 640) * xx%
 yy% = yy%

 IF bb% = 1 THEN
  IF (xx% < 65) THEN
   IF ABS(yy% - 6) < 4 THEN r = xx%
   IF ABS(yy% - 16) < 4 THEN G = xx%
   IF ABS(yy% - 26) < 4 THEN b = xx%
   IF G > 63 THEN G = 63
   IF r > 63 THEN r = 63
   IF b > 63 THEN b = 63
   IF r < 0 THEN r = 0
   IF G < 0 THEN G = 0
   IF b < 0 THEN b = 0
  END IF
  IF (xx% >= 2 AND yy% >= 54 AND xx% <= 30 AND yy% <= 65) THEN
   fin = 1
   stoprender = 0
  END IF

  IF (xx% >= 35 AND yy% >= 54 AND xx% <= 92 AND yy% <= 65) THEN
   fin = 1
   stoprender = 1
  END IF
 
 END IF
LOOP UNTIL fin = 1

IF stoprender = 0 THEN

HIDEMOUSE
CLS
WINDOW (-160, 100)-(160, -100)

	hr$ = MID$(TIME$, 1, 2)
	mn$ = MID$(TIME$, 4, 2)
	sec$ = MID$(TIME$, 7, 2)
	h = VAL(hr$)
	m = VAL(mn$)
	s = VAL(sec$)
	ss = s + 60 * m + 3600 * h

ynn = 92
xnn = 0
FOR nn = 1 TO numf
	xnn = xnn + 1
	IF xnn > 300 THEN
		xnn = 1
		ynn = ynn - 1
	END IF
	PSET (-100 + xnn / 1.5, ynn), 40
	FOR tt = 1 TO 3
		this = v(f(nn, tt), 3)
		IF this < minz THEN minz = this
		IF this > maxz THEN maxz = this
	NEXT tt
NEXT nn

cplus = 96

cinc = ABS(maxz - minz + 2) / 64


xnn = 0
ynn = 92

FOR nn = 1 TO numf
	xnn = xnn + 1
	IF xnn > 300 THEN
		xnn = 1
		ynn = ynn - 1
	END IF
	      
	PSET (-100 + xnn / 1.5, ynn), 63

		z1 = v(f(nn, 1), 3)
		X1 = p(v(f(nn, 1), 1), z1, mon)
		Y1 = p(v(f(nn, 1), 2), z1, mon)

		z2 = v(f(nn, 2), 3)
		X2 = p(v(f(nn, 2), 1), z2, mon)
		y2 = p(v(f(nn, 2), 2), z2, mon)
	      
		z3 = v(f(nn, 3), 3)
		x3 = p(v(f(nn, 3), 1), z3, mon)
		y3 = p(v(f(nn, 3), 2), z3, mon)

IF X1 >= X2 AND X2 > x3 THEN
	bx = X1
	sx = x3
ELSEIF X1 >= x3 AND X2 <= x3 THEN
	bx = X1
	sx = X2
ELSEIF X2 >= X1 AND X1 > x3 THEN
	bx = X2
	sx = x3
ELSEIF X2 >= x3 AND X1 <= x3 THEN
	bx = X2
	sx = X1
ELSEIF x3 >= X2 AND X2 > X1 THEN
	bx = x3
	sx = X1
ELSEIF x3 >= X1 AND X2 <= X1 THEN
	bx = x3
	sx = X2
END IF

IF Y1 >= y2 AND y2 > y3 THEN
	by = Y1
	sy = y3
ELSEIF Y1 >= y3 AND y2 <= y3 THEN
	by = Y1
	sy = y2
ELSEIF y2 >= Y1 AND Y1 > y3 THEN
	by = y2
	sy = y3
ELSEIF y2 >= y3 AND Y1 <= y3 THEN
	by = y2
	sy = Y1
ELSEIF y3 >= y2 AND y2 > Y1 THEN
	by = y3
	sy = Y1
ELSEIF y3 >= Y1 AND y2 <= Y1 THEN
	by = y3
	sy = y2
END IF

FOR x = sx TO bx STEP incr
FOR y = sy TO by STEP incr

IF inside(X1, Y1, X2, y2, x3, y3, x, y) = 1 THEN

a = ((Y1 - y2) * (z1 - z3) - (Y1 - y3) * (z1 - z2))
b = ((X1 - X2) * (z1 - z3) - (X1 - x3) * (z1 - z2))
c = ((X1 - X2) * (Y1 - y3) - (X1 - x3) * (Y1 - y2))

IF c <> 0 THEN z = -(((X1 - x) * a - (Y1 - y) * b) / c + z1) / cinc + cplus
minus = 0
current = POINT(x, y)
IF z > current THEN

'IF (INT(z) - cplus) * cinc > maxz THEN z = z - 1
'IF (INT(z) - cplus) * cinc < minz THEN z = z + 1


	IF INT(z) > 127 THEN z = 127
	IF INT(z) < 64 THEN z = 64

PSET (x, y), INT(z)

END IF
END IF


NEXT y
NEXT x

'LINE (x1, y1)-(x2, y2), 15
'LINE (x2, y2)-(x3, y3), 15
'LINE (x3, y3)-(x1, y1), 15

IF INKEY$ = "q" THEN
	stew = 1
	EXIT FOR
	ELSE stew = 0
	END IF
NEXT nn

IF rendtimes < 10 THEN
 picname$ = picnamebase$ + "00" + STR$(rendtimes) + ".bmp"
ELSEIF rendtimes < 100 THEN
 picname$ = picnamebase$ + "0" + STR$(rendtimes) + ".bmp"
ELSE
 picname$ = picnamebase$ + STR$(rendtimes) + ".bmp"
END IF

'SaveBMP8 picname$, -160, 100, 160, -100

rendtimes = rendtimes + 1

IF stew = 1 THEN
	 '----------------------------------------------------------
	LOCATE 1, 26
	PRINT "Render incomplete."
END IF


IF stew = 0 THEN
	LOCATE 1, 26
	PRINT "Done."
	END IF

SLEEP
SCREEN 9, , 0, 0
WINDOW (-110, 100)-(210, -100)

	hr$ = MID$(TIME$, 1, 2)
	mn$ = MID$(TIME$, 4, 2)
	sec$ = MID$(TIME$, 7, 2)
	h = VAL(hr$)
	m = VAL(mn$)
	s = VAL(sec$)
	se = s + 60 * m + 3600 * h

LOCATE 1, 1
diff = se - ss
PRINT "Render took :"; diff; " sec."
SLEEP

'the whole render/don't render if ends here
END IF

IF stoprender = 1 THEN
 SCREEN 9, , 0, 0
 WINDOW (-110, 100)-(210, -100)
END IF

CLS
STARTMOUSE
SHOWMOUSE

GOSUB fresh


RETURN


drawnum:
HIDEMOUSE
LINE (-99.5, 99.5)-(99.5, -99.5), col2, B
LINE (-100, 100)-(100, -100), col, B

LINE (100, 100)-(200, -100), col, B
LINE (100.3, 99.3)-(199, -99.3), col2, B

LINE (100.7, -55.5)-(199, -55.5), col2
LINE (100, -56)-(200, -56), col
LINE (100.7, -56.5)-(199, -56.5), col2

LINE (100.7, 30.5)-(199, 30.5), col2
LINE (100.7, 31.8)-(199, 31.8), col2
LINE (100, 31)-(200, 31), col

IF redr = 1 THEN
LINE (101, 98)-(199, 32), 0, BF
LINE (101, 30)-(199, -54), 0, BF
f$ = "create.btn"
CALL button(110, 95, f$)
f$ = "introt.btn"
CALL button(125, 95, f$)
f$ = "render.btn"
CALL button(140, 95, f$)
f$ = "refresh.btn"
CALL button(155, 95, f$)
f$ = "help.btn"
CALL button(170, 95, f$)

f$ = "save.btn"
CALL button(110, 80, f$)
f$ = "load.btn"
CALL button(140, 80, f$)
f$ = "export.btn"
CALL button(155, 80, f$)
f$ = "exit.btn"
CALL button(170, 80, f$)


f$ = "rotp.btn"
CALL button(110, 65, f$)
f$ = "rotm.btn"
CALL button(125, 65, f$)
f$ = "x.btn"
CALL button(140, 65, f$)
f$ = "y.btn"
CALL button(155, 65, f$)
f$ = "z.btn"
CALL button(170, 65, f$)

f$ = "scale.btn"
CALL button(110, 50, f$)
f$ = "x.btn"
CALL button(125, 50, f$)
f$ = "y.btn"
CALL button(140, 50, f$)
f$ = "z.btn"
CALL button(155, 50, f$)
f$ = "a.btn"
CALL button(170, 50, f$)
END IF

COLOR 15
LOCATE 10, 59
SELECT CASE vw
CASE 1
PRINT "Front View"
CASE 2
PRINT "Left View"
CASE 3
PRINT "Top View"
END SELECT
LOCATE 11, 59
PRINT "Rotation Axis :";
IF rotaxis = 1 THEN PRINT "x"
IF rotaxis = 2 THEN PRINT "y"
IF rotaxis = 3 THEN PRINT "z"
LOCATE 12, 59
PRINT "Rotation Amt :";
PRINT USING "###"; defr

LOCATE 13, 59
PRINT "Scale amt:"; USING "##.##"; defs
LOCATE 14, 59
PRINT "Scale axis:";
IF scx = 1 THEN PRINT "x  "
IF scx = 2 THEN PRINT "y  "
IF scx = 3 THEN PRINT "z  "
IF scx = 4 THEN PRINT "all"
LOCATE 15, 59
PRINT "Object:"
LOCATE 16, 56
PRINT name$ + "  "
LOCATE 17, 59
PRINT "Face #:";
IF onf > numf THEN
	PRINT "all"
ELSE
	PRINT onf
END IF
LOCATE 18, 59
PRINT "Vertex #:";
IF onv > numv THEN
	PRINT "all"
ELSE
	PRINT onv
END IF

LOCATE 19, 59
PRINT "Perspective :"; mon

LOCATE 21, 58
PRINT "QB-CAD  "; ver$

redrnums = 0
redr = 0
SHOWMOUSE
RETURN


what:

oldsel = sel
dist = 100

FOR a = 1 TO numb

distc = ((posx - btn(a, 1)) ^ 2 + (posy - btn(a, 2)) ^ 2) ^ (1 / 2)
IF distc < dist THEN
 sel = a
 dist = distc
END IF

NEXT a

LOCATE 23, 61
IF oldsel <> sel THEN PRINT "                  "
LOCATE 23, 61
IF sel = 1 THEN
 IF BBB% = 0 THEN PRINT "Create"
 IF BBB% = 1 THEN GOSUB creat
END IF
IF sel = 2 THEN
 IF BBB% = 0 THEN PRINT "Render"
 IF BBB% = 1 THEN GOSUB render
END IF
IF sel = 3 THEN
 IF BBB% = 0 THEN PRINT "Help"
 IF BBB% = 1 THEN GOSUB help
END IF
IF sel = 4 THEN
 IF BBB% = 0 THEN PRINT "Save"
 IF BBB% = 1 THEN
	GOSUB save
	GOSUB fresh
	END IF

END IF
IF sel = 5 THEN
 IF BBB% = 0 THEN PRINT "Load"
 IF BBB% = 1 THEN GOSUB load
END IF
IF sel = 6 THEN
 IF BBB% = 0 THEN PRINT ".raw Export"
 IF BBB% = 1 THEN GOSUB rawexport
END IF
IF sel = 7 THEN
 IF BBB% = 0 THEN PRINT "Positive rotate"
 IF BBB% = 1 THEN
		infiniterotate = 0
		rot = -defr * pi / 180
		GOSUB rotate
 END IF
END IF
IF sel = 8 THEN
 IF BBB% = 0 THEN PRINT "Negative rotate"
 IF BBB% = 1 THEN
		infiniterotate = 0
		rot = defr * pi / 180
		GOSUB rotate
 END IF
END IF
IF sel = 9 THEN
 IF BBB% = 0 THEN PRINT "x rotate axis"
 IF BBB% = 1 THEN
  rotaxis = 1
  redrnums = 1
 END IF
END IF
IF sel = 10 THEN
 IF BBB% = 0 THEN PRINT "y rotate axis"
 IF BBB% = 1 THEN
  rotaxis = 2
  redrnums = 1
 END IF

END IF
IF sel = 11 THEN
 IF BBB% = 0 THEN PRINT "z rotate axis"
 IF BBB% = 1 THEN

  rotaxis = 3
  redrnums = 1


 END IF

END IF
IF sel = 12 THEN
 IF BBB% = 0 THEN PRINT "Scale"
 IF BBB% = 1 THEN GOSUB sc
END IF
IF sel = 13 THEN
 IF BBB% = 0 THEN PRINT "x scale axis"
 IF BBB% = 1 THEN
	scx = 1
	redrnums = 1
 END IF
END IF
IF sel = 14 THEN
 IF BBB% = 0 THEN PRINT "y scale axis"
 IF BBB% = 1 THEN
	scx = 2
	redrnums = 1
 END IF

END IF
IF sel = 15 THEN
 IF BBB% = 0 THEN PRINT "z scale axis"
 IF BBB% = 1 THEN
	scx = 3
	redrnums = 1
 END IF

END IF
IF sel = 16 THEN
 IF BBB% = 0 THEN PRINT "all scale axis"
 IF BBB% = 1 THEN
	scx = 4
	redrnums = 1
 END IF

END IF
IF sel = 17 THEN
 IF BBB% = 0 THEN PRINT "Exit"
 IF BBB% = 1 THEN
	endit = 1
 END IF
END IF

IF sel = 18 THEN
 IF BBB% = 0 THEN PRINT "refresh screen"
 IF BBB% = 1 THEN GOSUB fresh
END IF

IF sel = 19 THEN
 IF BBB% = 0 THEN PRINT "interactive rotate"
 IF BBB% = 1 THEN GOSUB introtate
END IF


RETURN


findclosestm:

	holding = 1
	sdist = stol
	FOR aa = 1 TO numv

		IF vw = 1 THEN
		 dist = ((posx - p(v(aa, 1), v(aa, 3), mon)) ^ 2 + (posy - p(v(aa, 2), v(aa, 3), mon)) ^ 2) ^ (1 / 2)
		ELSEIF vw = 2 THEN
		 dist = ((posx - p(v(aa, 3), v(aa, 1), mon)) ^ 2 + (posy - p(v(aa, 2), v(aa, 1), mon)) ^ 2) ^ (1 / 2)
		ELSEIF vw = 3 THEN
		 dist = ((posx - p(v(aa, 1), v(aa, 2), mon)) ^ 2 + (posy - p(v(aa, 3), v(aa, 2), mon)) ^ 2) ^ (1 / 2)
		END IF
		IF dist < sdist THEN
			sdist = dist
			onv = aa
		END IF
       NEXT aa
       IF sdist = stol THEN holding = 0
       
RETURN



introtate:
quality = 4
HIDEMOUSE
LINE (102, 98)-(198, 35), 0, BF
LOCATE 2, 59
PRINT "Left-Click and drag"
LOCATE 3, 59
PRINT "to rotate, Right-"
LOCATE 4, 59
PRINT "click to quit."
LOCATE 5, 59
PRINT "Some faces are not"
LOCATE 6, 59
PRINT "shown for speed."
SHOWMOUSE
 DO
	DO
	 getmousecoord BBB%, XXX%, YYY%
	LOOP UNTIL BBB% = 1 OR BBB% = 2
	posx = (300 / 640) * XXX% - 100
	posy = (-200 / 350) * YYY% + 100
	oxxx% = XXX%
	oyyy% = YYY%
	DO
	 IF BBB% <> 2 THEN
	  getmousecoord BBB%, XXX%, YYY%
	 END IF
	LOOP UNTIL (XXX% <> oxxx% OR YYY% <> oyyy%) OR BBB% = 2
	nposx = (300 / 640) * XXX% - 100
	nposy = (-200 / 350) * YYY% + 100
	rx = nposx - posx
	ry = nposy - posy
       
	IF (rx <> 0 OR ry <> 0) THEN
	 rot = -ry / 3
	 IF vw = 1 THEN
		 rotaxis = 1
	 ELSEIF vw = 2 THEN
		 rotaxis = 3
	 ELSEIF vw = 3 THEN
		 rotaxis = 1
		 rot = -rot
	 END IF
	 GOSUB rotate
	 rot = rx / 3
	 IF vw = 1 THEN
		 rotaxis = 2
	 ELSEIF vw = 2 THEN
		 rotaxis = 2
	 ELSEIF vw = 3 THEN
		 rotaxis = 3
	 END IF
	 GOSUB rotate
	 GOSUB drawit
	END IF
 LOOP UNTIL BBB% = 2
CLS
redr = 1
redrnums = 1
quality = 1
RETURN

mesh:
RETURN

SUB button (x, y, f$)

ai = .5
bi = .5

OPEN f$ FOR INPUT AS #1

FOR a = 1 TO 10
FOR b = 1 TO 10

aa = x + (a)
bb = y - (b)
       
INPUT #1, col
IF col > 0 THEN
 LINE (aa, bb)-(aa + ai, bb + bi), col, BF
END IF

NEXT b
NEXT a

CLOSE #1

END SUB

FUNCTION cisin (a$, b$)

 a1$ = LCASE$(a$)
 b1$ = LCASE$(b$)
 cisin = isin(a1$, b1$)
 
END FUNCTION

SUB DBox (pr$(), v(), acc(), num, boxname$, success)

HIDEMOUSE
PCOPY 0, 1
SHOWMOUSE

success = 1

'box and shadow

LINE (-65, 89)-(56, 69), 8, BF
LINE (-64, 90)-(57, 70), 8, BF
LINE (-65, 90)-(55, 70), 15, BF
LINE (-64, 89)-(54, 69), 0, BF

LINE (-65, 69)-(56, -41), 8, BF
LINE (-64, 70)-(57, -42), 8, BF
LINE (-65, 70)-(55, -40), 15, BF
LINE (-64, 69)-(54, -39), 0, BF


'ok,cancel buttons
LINE (-25, -26)-(-11, -38), 15, BF
LINE (-24, -27)-(-12, -37), 8, BF
LINE (-23, -28)-(-13, -36), 0, BF

LINE (-5, -26)-(24, -38), 15, BF
LINE (-4, -27)-(23, -37), 8, BF
LINE (-3, -28)-(22, -36), 0, BF

LOCATE 3, 15
PRINT boxname$

LOCATE 17, 23
PRINT "ok"
LOCATE 17, 28
PRINT "cancel"


y = 5
x = 14

 FOR a = 1 TO num
   LOCATE y, x
   PRINT pr$(a); ": "; v(a)
   y = y + 1
 NEXT a


'the main loop

DO

SHOWMOUSE

getmousecoord b%, x%, y%
posx = (300 / 640) * x% - 100
posy = (-200 / 350) * y% + 100

IF b% = 1 THEN

 FOR a = 1 TO num
  py1 = 74 - 8 * a
  py2 = 70 - 8 * a
  'clicking on a field
  IF posy <= py1 AND posy >= py2 THEN
   DO
    HIDEMOUSE
    valid = 1
    decimal = 0
    enter = 0
    value$ = ""
    LOCATE 4 + a, 17 + LEN(pr$(a))
    PRINT "_             "
    DO
     DO
      keystroke$ = INKEY$
      IF LEN(keystroke$) > 0 THEN keyval = ASC(keystroke$)
      IF LEN(keystroke$) > 0 AND keyval = 8 AND LEN(value$) > 0 THEN
       value$ = LEFT$(value$, LEN(value$) - 1)
       LOCATE 4 + a, 17 + LEN(pr$(a))
       PRINT "             "
       LOCATE 4 + a, 17 + LEN(pr$(a))
       PRINT value$
      END IF
      IF keyval > 0 AND keyval <> 13 AND (keyval < 45 OR keyval > 57 OR keyval = 47) THEN keystroke$ = ""
      IF LEN(value$) > 0 AND keystroke$ = "-" THEN keystroke$ = ""
      IF decimal = 1 AND keystroke$ = "." THEN keystroke$ = ""
     LOOP UNTIL LEN(keystroke$) >= 1
     IF keystroke$ = "." THEN decimal = 1
   
     IF keyval = 13 THEN
      enter = 1
     ELSE
      value$ = value$ + keystroke$
      LOCATE 4 + a, 17 + LEN(pr$(a))
      PRINT value$
     END IF
    LOOP UNTIL enter = 1
    valu = VAL(value$)
    IF valu < acc(a, 1) THEN
     valid = 0
     LOCATE 15, 20
     PRINT "Must be >="; acc(a, 1)
     LOCATE 4 + a, 17 + LEN(pr$(a))
     PRINT "_             "
    END IF
    IF valu > acc(a, 2) THEN
     valid = 0
     LOCATE 15, 20
     PRINT "Must be <="; acc(a, 2)
     LOCATE 4 + a, 17 + LEN(pr$(a))
     PRINT "_             "

    END IF
   LOOP UNTIL valid = 1
   v(a) = valu
   LOCATE 15, 20
   PRINT "             "
   END IF
  NEXT a

 IF posy >= -38 AND posy <= -26 THEN
  IF posx >= -25 AND posx <= -11 THEN done = 1
  IF posx >= -5 AND posx <= 24 THEN
   success = 0
   done = 1
  END IF
 END IF
END IF

LOOP UNTIL done = 1
HIDEMOUSE

PCOPY 1, 0
SHOWMOUSE
END SUB

SUB DsBox (pr$, v$, boxname$, success, default$)

HIDEMOUSE
PCOPY 0, 1
SHOWMOUSE

success = 1

'box and shadow

LINE (-65, 89)-(56, 69), 8, BF
LINE (-64, 90)-(57, 70), 8, BF
LINE (-65, 90)-(55, 70), 15, BF
LINE (-64, 89)-(54, 69), 0, BF

LINE (-65, 69)-(56, -41), 8, BF
LINE (-64, 70)-(57, -42), 8, BF
LINE (-65, 70)-(55, -40), 15, BF
LINE (-64, 69)-(54, -39), 0, BF


'ok,cancel buttons
LINE (-25, -26)-(-11, -38), 15, BF
LINE (-24, -27)-(-12, -37), 8, BF
LINE (-23, -28)-(-13, -36), 0, BF

LINE (-5, -26)-(24, -38), 15, BF
LINE (-4, -27)-(23, -37), 8, BF
LINE (-3, -28)-(22, -36), 0, BF

LOCATE 3, 15
PRINT boxname$

LOCATE 17, 23
PRINT "ok"
LOCATE 17, 28
PRINT "cancel"


y = 5
x = 14


   LOCATE y, x
   PRINT pr$; ": "; v$


'the main loop

DO

SHOWMOUSE

getmousecoord b%, x%, y%
posx = (300 / 640) * x% - 100
posy = (-200 / 350) * y% + 100

IF b% = 1 THEN

  a = 1
  py1 = 74 - 8 * a
  py2 = 70 - 8 * a
  'clicking on a field
  IF posy <= py1 AND posy >= py2 THEN
   DO
    HIDEMOUSE
    valid = 1
    decimal = 0
    enter = 0
    value$ = ""
    LOCATE 4 + a, 16 + LEN(pr$)
    PRINT "_             "
    DO
     DO
      keystroke$ = INKEY$
      'keyval = ASC(keystroke$)

      IF LEN(keystroke$) > 0 THEN keyval$ = keystroke$
      IF LEN(keystroke$) > 0 AND keyval$ = CHR$(8) AND LEN(value$) > 0 THEN
       value$ = LEFT$(value$, LEN(value$) - 1)
       LOCATE 4 + a, 17 + LEN(pr$)
       PRINT "             "
       LOCATE 4 + a, 17 + LEN(pr$)
       PRINT value$
      END IF
      IF LEN(value$) > 0 AND keystroke$ = "-" THEN keystroke$ = ""
      LOOP UNTIL LEN(keystroke$) >= 1
     keys = ASC(keyval$)

     IF keys = 13 THEN
      enter = 1
     ELSE
      value$ = value$ + keystroke$
      LOCATE 4 + a, 17 + LEN(pr$)
      PRINT value$
     END IF
    LOOP UNTIL enter = 1

   LOOP UNTIL valid = 1

   LOCATE 15, 20
   PRINT "             "
   END IF
  

 IF posy >= -38 AND posy <= -26 THEN
  IF posx >= -25 AND posx <= -11 THEN done = 1
  IF posx >= -5 AND posx <= 24 THEN
   success = 0
   done = 1
  END IF
 END IF
END IF
 name$ = value$
		IF name$ = "" THEN
		       name$ = default$
		END IF

		IF LEN(name$) > 15 THEN success = 0
		IF NOT RIGHT$(name$, 3) = "b3d" THEN name$ = name$ + ".b3d"
       
LOOP UNTIL done = 1

HIDEMOUSE
CLOSE #1
OPEN "stew.sav" FOR OUTPUT AS #1
PRINT #1, name$
CLOSE #1

PCOPY 1, 0
SHOWMOUSE


END SUB

FUNCTION FileBox$ (title$, path$, ext$)

DIM f$(1 TO 1000, 1 TO 2)

HIDEMOUSE
PCOPY 0, 1

LINE (-84, 88)-(112, 68), 1, BF
LINE (-85, 89)-(111, 69), 9, BF
LINE (-85, 90)-(110, 70), 15, BF
LINE (-84, 89)-(109, 69), 0, BF

LINE (-84, 70)-(112, -47), 1, BF
LINE (-85, 69)-(111, -46), 9, BF
LINE (-85, 70)-(110, -45), 15, BF
LINE (-84, 69)-(109, -44), 0, BF


'ok,cancel buttons
LINE (55, -26)-(69, -38), 15, B
LINE (56, -27)-(68, -37), 1, B

LINE (75, -26)-(104, -38), 15, B
LINE (76, -27)-(103, -37), 1, B

LOCATE 3, 9
PRINT title$

LOCATE 17, 43
PRINT "ok"
LOCATE 17, 48
PRINT "cancel"

'filebox lines
LINE (-76, 62)-(-16, -22), 15, B
LINE (-75, 61)-(-17, -21), 1, B
LINE (-16, 62)-(-6, -22), 15, B
LINE (-15, 61)-(-7, -21), 1, B
LINE (-14, 60)-(-8, -20), 0, B
LINE (-15, 55)-(-7, 55), 15
LINE (-15, -14)-(-7, -14), 15


'Arrow Buttons
'top button
LINE (-15, 56)-(-7, 61), 1, BF
LINE (-14, 56)-(-11, 60), 9
LINE (-11, 60)-(-8, 56), 9
LINE (-14, 56)-(-8, 56), 9
PAINT (-12, 57), 9


'bottom button
LINE (-15, -21)-(-7, -15), 1, BF
LINE (-14, -16)-(-11, -20), 9
LINE (-11, -20)-(-8, -16), 9
LINE (-14, -16)-(-8, -16), 9
PAINT (-12, -17), 9

'Current Path, Name text things

LOCATE 6, 43
PRINT "Current Path"

LINE (4, 45)-(108, 35), 15, B
LOCATE 8, 30
IF LEN(path$) > 25 THEN PRINT LEFT$(path$, 25) ELSE PRINT path$

LOCATE 10, 47
PRINT "Filename"

LINE (4, 13)-(108, 3), 15, B
'LOCATE 12, 30
'this is the locate for the filename but at this point there is none



GOSUB getfiles

'2 are needed for some reason.  I don't get it.
SHOWMOUSE
SHOWMOUSE

done = 0
fchange = 0
change = 1
at = 1

DO

 IF pchange = 1 THEN
	HIDEMOUSE
	LOCATE 8, 30
	PRINT "                         "
	LOCATE 8, 30
	IF LEN(path$) > 25 THEN PRINT LEFT$(path$, 25) ELSE PRINT path$
	SHOWMOUSE
 END IF

 IF fchange = 1 THEN
	HIDEMOUSE
	LOCATE 12, 30
	PRINT "            "
	LOCATE 12, 30
	PRINT name$
	fchange = 0
	SHOWMOUSE
 END IF

 IF change = 1 THEN
  IF pchange = 1 THEN GOSUB getfiles
  HIDEMOUSE
  LINE (-14, vert - 1)-(-8, vert + 1), 0, BF

  vert = 52 - (64 * ((at - 1) / (n - 10)))
  LINE (-14, vert - 1)-(-8, vert + 1), 9, BF

  LINE (-74, 60)-(-18, -20), 0, BF
  ycursor = 6
  FOR a = at TO at + 9
   LOCATE ycursor, 10
   IF f$(a, 2) = "dir" OR f$(a, 2) = "back" THEN
    PRINT "<"; f$(a, 1); ">"
   ELSE
    PRINT f$(a, 1)
   END IF
  pp ycursor
  NEXT a
 change = 0
 SHOWMOUSE
 END IF

 getmousecoord b%, x%, y%
 posx = (300 / 640) * x% - 100
 posy = (-200 / 350) * y% + 100

 'Check for button press and then decide what to do about it
 IF b% = 1 THEN

  'bottom scroller
  IF posx >= -17 AND posy >= -21 AND posx <= -7 AND posy <= -15 THEN
   oldat = at
   at = at + 9
   IF at > n - 9 THEN at = n - 9
   IF at <> oldat THEN change = 1
  END IF

  'top scroller
  IF posx >= -15 AND posy >= 56 AND posx <= -7 AND posy <= 61 THEN
   oldat = at
   at = at - 9
   IF at < 1 THEN at = 1
   IF oldat <> at THEN change = 1
  END IF

  'on the bar
  IF posx >= -15 AND posy < 61 AND posx <= -7 AND posy > -15 THEN
   oldat = at
   at = INT((52 - posy) * (n - 10) / 64) + 1
   IF at > n - 9 THEN at = n - 9
   IF at < 1 THEN at = 1
   IF at <> oldat THEN change = 1
  END IF
   
  IF posx >= -68 AND posx <= -19 AND posy <= 60 AND posy >= -18 THEN
   yloc = at + INT((108 - posy) / 8) - 6
   name$ = f$(yloc, 1)
   IF f$(yloc, 2) = "file" THEN
    name$ = RTRIM$(LEFT$(name$, 8)) + "." + RIGHT$(name$, 3)
    fchange = 1
   END IF
   IF f$(yloc, 2) = "dir" THEN
    at = 1
    IF RIGHT$(path$, 1) <> "\" THEN path$ = path$ + "\"
    path$ = path$ + LTRIM$(RTRIM$(f$(yloc, 1)))
    name$ = ""
    fchange = 1
    pchange = 1
    change = 1
   END IF
   IF f$(yloc, 2) = "back" THEN
   at = 1
    DO
     IF RIGHT$(path$, 1) = ":" THEN path$ = path$ + "\"
     hacked$ = RIGHT$(path$, 1)
     path$ = LEFT$(path$, LEN(path$) - 1)
    LOOP UNTIL hacked$ = "\"
    IF RIGHT$(path$, 1) = ":" THEN path$ = path$ + "\"
    at = 1
    GOSUB getfiles
    name$ = ""
    fchange = 1
    pchange = 1
    change = 1
   END IF
  END IF

  IF posx >= 4 AND posy <= 45 AND posx <= 108 AND posy >= 35 THEN
  
   HIDEMOUSE
    valid = 1
    decimal = 0
    enter = 0
    DO
     DO
      keystroke$ = INKEY$
      IF LEN(keystroke$) > 0 THEN keyval = ASC(keystroke$)
      IF LEN(keystroke$) > 0 AND keyval = 8 AND LEN(path$) > 0 THEN
       path$ = LEFT$(path$, LEN(path$) - 1)
       LOCATE 8, 30
       PRINT "                        "
       LOCATE 8, 30
       IF LEN(path$) < 25 THEN PRINT path$ ELSE PRINT LEFT$(path$, 25)
      END IF
      IF decimal = 1 AND keystroke$ = "." THEN keystroke$ = ""
     LOOP UNTIL LEN(keystroke$) >= 1
     IF keystroke$ = "." THEN decimal = 1
 
     IF keyval = 13 THEN
      enter = 1
     ELSE
     IF keyval <> 8 THEN
      path$ = path$ + keystroke$
      LOCATE 8, 30
      PRINT "                        "
      LOCATE 8, 30
      IF LEN(path$) < 25 THEN PRINT path$ ELSE PRINT LEFT$(path$, 25)
     END IF
     END IF

     LOOP UNTIL enter = 1
    
     LOCATE 8, 30
     PRINT "                        "
     LOCATE 8, 30
     IF LEN(path$) < 25 THEN PRINT path$ ELSE PRINT LEFT$(path$, 25)
    change = 1
    pchange = 1
    SHOWMOUSE
   END IF

 'ok,cancel
 IF posy <= -26 AND posy >= -38 THEN
  IF posx >= 55 AND posx <= 69 THEN
   ok = 1
   done = 1
  ELSEIF posx >= 75 AND posx <= 104 THEN
   done = 1
  END IF
 END IF


 END IF



LOOP UNTIL done = 1


a = 1
b = 2
IF a = b THEN 'which it should not
getfiles:
 at = 1

 FOR a = 1 TO 10
  f$(a, 1) = ""
  f$(a, 2) = ""
 NEXT a



LOCATE 18, 9
PRINT "                     "
LOCATE 17, 9
PRINT "Scanning directory..."
LOCATE 18, 9
SHELL "dir /ogne " + path$ + " >dir.txt"
'/ogne is for sorting
' o means sort, g is dir's first, n and e alphabetized

OPEN "dir.txt" FOR INPUT AS #1
n = 0
DO UNTIL EOF(1)

 LINE INPUT #1, l$
 IF LEFT$(l$, 1) <> " " THEN
  IF LCASE$(MID$(l$, 10, 3)) = LCASE$(ext$) THEN
   pp n 'uses pp function to increment n in a c-like fashion
   f$(n, 1) = LEFT$(l$, 12)
   f$(n, 2) = "file"
  ELSEIF isin("<DIR>", l$) AND RTRIM$(LEFT$(l$, 2)) <> "." THEN
   pp n
   f$(n, 1) = LEFT$(l$, 12)
   f$(n, 2) = "dir"
  END IF
  IF LEFT$(l$, 2) = ".." THEN
   f$(n, 1) = ".."
   f$(n, 2) = "back"
  END IF
 END IF
LOOP
CLOSE #1


LOCATE 17, 9
PRINT "                     "
pchange = 0

RETURN

END IF

IF ok = 0 THEN
 path$ = ""
 name$ = ""
END IF


PCOPY 1, 0
IF RIGHT$(path$, 1) <> "\" THEN path$ = path$ + "\"
IF LEN(name$) < 1 THEN path$ = ""

FileBox$ = path$ + name$
OPEN "dir.tmp" FOR OUTPUT AS #2
PRINT #2, path$
CLOSE #2


END FUNCTION

SUB fpaint (X1, Y1, X2, y2, x3, y3, col, col2)


	LINE (X1, Y1)-(X2, y2), col2
	LINE (X2, y2)-(x3, y3), col2
	LINE (x3, y3)-(X1, Y1), col2

END SUB

SUB getmousecoord (b%, x%, y%)
ASM$ = ""                                         'GetMouseCoord:
ASM$ = ASM$ + CHR$(&H55)                          'push bp
ASM$ = ASM$ + CHR$(&H89) + CHR$(&HE5)             'mov bp,sp
ASM$ = ASM$ + CHR$(&HB8) + MKI$(3)                'mov ax,3
ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H33)             'int 33h
ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H46) + CHR$(&H6) 'mov ax,[bp+06]
ASM$ = ASM$ + CHR$(&H93)                          'xchg bx,ax
ASM$ = ASM$ + CHR$(&H89) + CHR$(&H7)              'mov [bx],ax
ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8) 'mov bx, [bp+08]
ASM$ = ASM$ + CHR$(&H89) + CHR$(&H17)             'mov [bx],dx

ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HA) 'mov bx,[bp+10]
ASM$ = ASM$ + CHR$(&H89) + CHR$(&HF)              'mov [bx],cx
ASM$ = ASM$ + CHR$(&H5D)                          'pop bp
ASM$ = ASM$ + CHR$(&HCA) + MKI$(6)                'retf 06

DEF SEG = VARSEG(ASM$)
  CALL absolute(x%, y%, b%, SADD(ASM$))
DEF SEG

END SUB

DEFINT A-Z
SUB GetPalette (Attr%, Red%, Green%, Blue%)

OUT &H3C7, Attr%
Red% = INP(&H3C9)
Green% = INP(&H3C9)
Blue% = INP(&H3C9)

END SUB

DEFSNG A-Z
SUB HIDEMOUSE
ASM$ = ""                                         'HideMouse:
ASM$ = ASM$ + CHR$(&HB8) + MKI$(2)                'mov ax,2
ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H33)             'int 33h
ASM$ = ASM$ + CHR$(&HCB)                          'retf
DEF SEG = VARSEG(ASM$)
  CALL absolute(SADD(ASM$))
DEF SEG
END SUB

FUNCTION inside (X1, Y1, X2, y2, x3, y3, x, y)


mx = (X1 + X2 + x3) / 3
my = (Y1 + y2 + y3) / 3


num = 0
col = 9

FOR a = 1 TO 3
IF a = 1 THEN
     ax1 = X1
     ay1 = Y1
     ax2 = X2
     ay2 = y2

END IF
IF a = 2 THEN
	ax1 = X2
	ay1 = y2
	ax2 = x3
	ay2 = y3
END IF
IF a = 3 THEN
	ax1 = X1
	ay1 = Y1
	ax2 = x3
	ay2 = y3
END IF

dx = ax2 - ax1
dy = ay2 - ay1
IF dy = 0 THEN
	num = num + 1
ELSE
	b = x - (dx / dy) * y
	lxm = (dx / dy) * my + (ax1 - (dx / dy) * ay1)
	lx = (dx / dy) * y + (ax1 - (dx / dy) * ay1)
	cx = lxm - mx
	cxa = lx - x
	IF (cx <= 0 AND cxa <= 0) OR (cx >= 0 AND cxa >= 0) THEN num = num + 1
END IF
NEXT a



IF num = 3 THEN inside = 1
IF num <> 3 THEN inside = 0
END FUNCTION

FUNCTION isin (a$, b$)

isn = 1
IF LEN(a$) > LEN(b$) OR (LEN(a$) = LEN(b$) AND a$ <> b$) THEN isn = 0
IF isn = 1 THEN
 isn = 0
 FOR c = 1 TO (LEN(b$) - LEN(a$)) + 1
  IF MID$(b$, c, LEN(a$)) = a$ THEN isn = 1
 NEXT c
END IF
isin = isn
END FUNCTION

SUB mm (a)
 a = a - 1
END SUB

SUB MOUSEBOX (X1%, Y1%, X2%, y2%)
ASM$ = ""                                         'MouseBox:
ASM$ = ASM$ + CHR$(&H55)                          'push bp
ASM$ = ASM$ + CHR$(&H89) + CHR$(&HE5)             'mov bp,sp
ASM$ = ASM$ + CHR$(&HB8) + MKI$(7)                'mov ax,7
ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H4E) + CHR$(&H6) 'mov cx,[bp+06]
ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H56) + CHR$(&H8) 'mov dx,[bp+08]
ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H33)             'int 33h
ASM$ = ASM$ + CHR$(&H40)                          'inc ax
ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H4E) + CHR$(&HA) 'mov cx,[bp+10]
ASM$ = ASM$ + CHR$(&H8B) + CHR$(&H56) + CHR$(&HC) 'mov dx,[bp+12]
ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H33)             'int 33h
ASM$ = ASM$ + CHR$(&H5D)                          'pop bp
ASM$ = ASM$ + CHR$(&HCA) + MKI$(8)                'retf 08

DEF SEG = VARSEG(ASM$)
  CALL absolute(BYVAL y2, BYVAL Y1, BYVAL X2, BYVAL X1, SADD(ASM$))
DEF SEG

END SUB

FUNCTION p (real, dist, mon)


p = (mon * real) / (mon + dist)


END FUNCTION

SUB pp (a)

 a = a + 1

END SUB

SUB SetPal (c, r, G, b)

OUT &H3C8, c
OUT &H3C9, r
OUT &H3C9, G
OUT &H3C9, b


END SUB

SUB SHOWMOUSE
ASM$ = ""                                         'ShowMouse:
ASM$ = ASM$ + CHR$(&HB8) + MKI$(1)                'mov ax,1
ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H33)             'int 33h
ASM$ = ASM$ + CHR$(&HCB)                          'retf
DEF SEG = VARSEG(ASM$)
  CALL absolute(SADD(ASM$))
DEF SEG
END SUB

SUB STARTMOUSE
ASM$ = ""                                         'StartMouse:
ASM$ = ASM$ + CHR$(&H33) + CHR$(&HC0)             'xor ax,ax
ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H33)             'int 33h
ASM$ = ASM$ + CHR$(&HCB)                          'retf
DEF SEG = VARSEG(ASM$)
  CALL absolute(SADD(ASM$))
DEF SEG
END SUB

