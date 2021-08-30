DECLARE SUB DBox (p$(), v!(), acc!(), num!, boxname$, success!)
DECLARE SUB dialog (prompt$, nprompt!, aname!)
DECLARE SUB GETMOUSECOORD (b%, x%, y%)
DECLARE SUB HIDEMOUSE ()
DECLARE SUB STARTMOUSE ()
DECLARE SUB SHOWMOUSE ()
'QB-CAD Lathe
CLS
SCREEN 9

stol = 10
yesno = 0

WINDOW (-100, 100)-(200, -100)

pi = 3.141592654#

col = 9
col2 = 1

LINE (-100, 100)-(100, -100), col, B
LINE (-99, 99)-(99, -99), col2, B
LINE (100, 100)-(200, -100), col, B
LINE (101, 99)-(199, -99), col2, B
LINE (101, -60)-(199, -60), col2
LINE (100, -61)-(200, -61), col
LINE (101, -62)-(199, -62), col2
LINE (102, 98)-(198, -55), 0, BF



new:
HIDEMOUSE
LINE (102, 98)-(198, -50), 0, BF

'DO

                DIM values(1 TO 1)
                DIM accepted(1 TO 1, 1 TO 2)
                REDIM prompts$(1 TO 1)
                accepted(1, 1) = 1
                accepted(1, 2) = 100
               
                values(1) = 15
               
                prompts$(1) = "Verticies"
               
                boxname$ = "Lathe"
                num = 1
                CALL DBox(prompts$(), values(), accepted(), num, boxname$, success)
        IF success = 1 THEN

numv = values(1)

        LINE (102, 98)-(198, -15), 0, BF
STARTMOUSE
SHOWMOUSE

REDIM vert(1 TO numv, 1 TO 2)
y = -75
FOR nowv = 1 TO numv
        vert(nowv, 1) = 0
        vert(nowv, 2) = y
        y = y + (150 / (numv - 1))
NEXT nowv
onv = 1
redr = 1

GOSUB words
main2:
DO
        GETMOUSECOORD BBB%, XXX%, YYY%
        pposx = posx
        pposy = posy
        posx = (300 / 640) * XXX% - 100
        posy = (-200 / 350) * YYY% + 100
        IF holding = 1 THEN
                vert(onv, 1) = posx
                vert(onv, 2) = posy
                IF posx <> pposx OR posy <> pposy THEN redr = 1
        END IF

        IF BBB% = 1 THEN
                IF holding = 0 THEN
                        GOSUB findclosest
                ELSE
                        holding = 0
                        redr = 1
                END IF
        END IF
        IF BBB% = 2 THEN
                holding = 0
                redr = 1
        END IF
        IF BBB% = 0 THEN holding = 0

        SELECT CASE INKEY$
        CASE "n", "N"
                GOTO new
        CASE "s", "S"
                GOSUB save2
        CASE "l", "L"
                GOSUB load2
        CASE "e", "E"
                GOSUB export
        CASE "v"
                onv = onv + 1
                IF onv = numv + 1 THEN onv = 1
                redr = 1
        CASE "V"
                onv = onv - 1
                IF onv = 0 THEN onv = numv
                redr = 1
        CASE CHR$(0) + "H"
                'up
                redr = 1
                vert(onv, 2) = vert(onv, 2) + 2

        CASE CHR$(0) + "P"
                'down
                redr = 1
                vert(onv, 2) = vert(onv, 2) - 2
        CASE CHR$(0) + "K"
                'left
                redr = 1
                vert(onv, 1) = vert(onv, 1) - 2
        CASE CHR$(0) + "M"
                'right
                redr = 1
                vert(onv, 1) = vert(onv, 1) + 2
        CASE "q", "Q"
                endit = 1
        CASE "~"
                GOSUB fresh2
        CASE "+"
                IF NOT onv = numv THEN
                        GOSUB add
                ELSE
                        LOCATE 19, 60
                        PRINT "Cannot add past"
                        LOCATE 20, 60
                        PRINT "Ending vertex"
                END IF
        CASE "-"
                IF numv > 2 THEN
                        IF onv > 1 THEN
                                GOSUB subt
                        ELSE
                                LOCATE 19, 60
                                PRINT "Cannot subtract"
                                LOCATE 20, 60
                                PRINT "first vertex"
                        END IF
                ELSE
                        LOCATE 19, 60
                        PRINT "Shape must contain"
                        LOCATE 20, 60
                        PRINT "more than one vertex"
                END IF
        END SELECT
        IF redr = 1 THEN GOSUB drawit2
        IF redrwds = 1 THEN GOSUB words
LOOP UNTIL endit = 1
CLS
IF endit = 1 THEN GOTO tend

drawit2:
        LINE (-98, -98)-(98, 98), 0, BF
        LINE (0, -97)-(0, 97), 7
        FOR nowv = 1 TO numv - 1
                X1 = vert(nowv, 1)
                Y1 = vert(nowv, 2)
                X2 = vert(nowv + 1, 1)
                y2 = vert(nowv + 1, 2)
                LINE (X1, Y1)-(X2, y2), 9
                IF nowv = onv THEN
                        colnow = 14
                ELSE
                        colnow = 3
                END IF
                CIRCLE (X1, Y1), 1, colnow
        NEXT nowv
        nowv = numv
        X1 = vert(nowv, 1)
        Y1 = vert(nowv, 2)
        IF nowv = onv THEN
                colnow = 12
        ELSE
        colnow = 3
        END IF

        CIRCLE (X1, Y1), 1, colnow
        redr = 0
       
RETURN


export:
        vnum = numv
        LOCATE 5, 60
        INPUT "Number of slices :", slic
        numv = slic * vnum
        numf = 2 * numv - (slic * 2)
             
        REDIM f(1 TO numf, 1 TO 4)
        REDIM v(1 TO numv, 1 TO 3)
        REDIM orderv(1 TO vnum, 1 TO slic)
        nowv = 0
        FOR row = 1 TO vnum
                angle = 0
                y = vert(row, 2)
                rad = vert(row, 1)
                FOR circ = 1 TO slic
                        nowv = nowv + 1
                        x = COS(angle) * rad
                        z = SIN(angle) * rad
                        v(nowv, 1) = x
                        v(nowv, 2) = y
                        v(nowv, 3) = z
                        orderv(row, circ) = nowv
                        angle = angle + ((2 * pi) / slic)
                NEXT circ
        NEXT row

        FOR row = 2 TO vnum
                FOR circ = 1 TO slic
                        nowf = nowf + 1
                        IF circ < slic THEN
                                f(nowf, 1) = orderv(row, circ)
                                f(nowf, 2) = orderv(row - 1, circ)
                                f(nowf, 3) = orderv(row - 1, circ + 1)
                        END IF
                        IF circ = slic THEN
                                f(nowf, 1) = orderv(row, circ)
                                f(nowf, 2) = orderv(row - 1, circ)
                                f(nowf, 3) = orderv(row - 1, 1)
                        END IF
                  
                    
                        nowf = nowf + 1
                        IF circ < slic THEN
                                f(nowf, 1) = orderv(row, circ)
                                f(nowf, 2) = orderv(row - 1, circ + 1)
                                f(nowf, 3) = orderv(row, circ + 1)
                        END IF
                        IF circ = slic THEN
                                f(nowf, 1) = orderv(row, circ)
                                f(nowf, 2) = orderv(row - 1, 1)
                                f(nowf, 3) = orderv(row, 1)
                        END IF

                NEXT circ
        NEXT row
        redr = 1
        redrnums = 1

        name$ = "temp9999.b3d"
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
        redr = 1
        redrnums = 1
        CLOSE #1
        endit = 1
        numv = vnum
        yesno = 1

RETURN


words:
LINE (102, 98)-(198, -50), 0, BF

LOCATE 2, 61
PRINT "(E)xport Lathe"
LOCATE 3, 61
PRINT "(S)ave shape"
LOCATE 4, 61
PRINT "(L)oad shape"
LOCATE 5, 61
PRINT "(N)ew shape"
LOCATE 10, 61
PRINT "(V)ertex select"
LOCATE 11, 61
PRINT "+ Add vertex"
LOCATE 12, 61
PRINT "- Subtract vertex"
LOCATE 18, 61
PRINT "(Q)uit"
LOCATE 19, 61
PRINT "~ Refresh"
'LOCATE 27, 61
'PRINT "QB-CAD  "; ver$
'LOCATE 28, 61
'PRINT "George Jones"
redrwds = 0
RETURN


save2:
        DO
                bad = 0
                LOCATE 6, 60
                PRINT "Save as "
                LOCATE 7, 60
                INPUT " : ", name$
                IF LEN(name$) > 12 THEN bad = 1
                lon = LEN(name$) - 3
                IF NOT MID$(name$, lon, 1) = "." THEN bad = 1
                IF bad = 1 THEN
                        LOCATE 8, 60
                        PRINT "invalid filename."
                END IF
        LOOP UNTIL bad = 0
        OPEN name$ FOR OUTPUT AS #1
        PRINT #1, numv
        FOR temp = 1 TO numv
                PRINT #1, vert(temp, 1)
                PRINT #1, vert(temp, 2)
        NEXT temp
        CLOSE #1
        redr = 1
        redrwds = 1
RETURN



load2:
        GOSUB filelist2
        OPEN name$ FOR INPUT AS #1
        INPUT #1, numv
        REDIM vert(1 TO numv, 1 TO 2)
        FOR temp = 1 TO numv
                INPUT #1, vert(temp, 1)
                INPUT #1, vert(temp, 2)
        NEXT temp
        CLOSE #1
        redr = 1
        redrwds = 1
RETURN


filelist2:
SHELL "dir *.b2d >2dfiles.txt"
LINE (102, 98)-(198, -50), 0, BF
nfs = 0
OPEN "2dfiles.txt" FOR INPUT AS #1
REDIM namef$(1 TO 100)
DO
        done = 0
        INPUT #1, l$
        IF RIGHT$(l$, 4) = "free" THEN done = 1
        IF MID$(l$, 10, 3) = "B2D" OR MID$(l$, 10, 3) = "b2d" THEN
                nfs = nfs + 1
                namef$(nfs) = RTRIM$(LTRIM$(LEFT$(l$, 8))) + ".b2d"
        END IF
LOOP UNTIL done = 1
CLOSE #1
SHELL "erase 2dfiles.txt"
IF nfs > 30 THEN nfs = 30
onf = 1
GOSUB showfiles2
DO
        yep = 0
        sf = 0
        SELECT CASE INKEY$
        CASE ""
                GOSUB words
                GOTO main2
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
        IF sf = 1 THEN GOSUB showfiles2
LOOP UNTIL yep = 1

RETURN

showfiles2:
LINE (102, 98)-(198, -50), 0, BF

FOR temp = 1 TO nfs
        IF temp = onf THEN
                LOCATE temp + 1, 58
                PRINT "*"
        END IF
        LOCATE temp + 1, 59
        PRINT namef$(temp)

NEXT temp
RETURN

add:
        REDIM tvert(1 TO numv, 1 TO 2)
        FOR temp = 1 TO numv
                tvert(temp, 1) = vert(temp, 1)
                tvert(temp, 2) = vert(temp, 2)
        NEXT temp
        numv = numv + 1
        x = (vert(onv, 1) + vert(onv + 1, 1)) / 2
        y = (vert(onv, 2) + vert(onv + 1, 2)) / 2

      
        REDIM vert(1 TO numv, 1 TO 2)
        FOR temp = 1 TO numv
                IF temp <= onv THEN
                        vert(temp, 1) = tvert(temp, 1)
                        vert(temp, 2) = tvert(temp, 2)
                END IF
                IF temp = onv + 1 THEN
                        vert(temp, 1) = x
                        vert(temp, 2) = y
                END IF
                IF temp > onv + 1 THEN
                        vert(temp, 1) = tvert(temp - 1, 1)
                        vert(temp, 2) = tvert(temp - 1, 2)
                END IF
        NEXT temp
        redr = 1
        redrwds = 1
        onv = onv + 1
RETURN
      
subt:
        REDIM tvert(1 TO numv, 1 TO 2)
        FOR temp = 1 TO numv
                tvert(temp, 1) = vert(temp, 1)
                tvert(temp, 2) = vert(temp, 2)
        NEXT temp
        numv = numv - 1
     
        REDIM vert(1 TO numv, 1 TO 2)
        FOR temp = 1 TO numv
                IF temp < onv THEN
                        vert(temp, 1) = tvert(temp, 1)
                        vert(temp, 2) = tvert(temp, 2)
                END IF
                IF temp >= onv THEN
                        vert(temp, 1) = tvert(temp + 1, 1)
                        vert(temp, 2) = tvert(temp + 1, 2)
                END IF
        NEXT temp
        redr = 1
        redrwds = 1
        onv = onv - 1
RETURN

fresh2:
        HIDEMOUSE
        CLS
        LINE (-100, 100)-(100, -100), col, B
        LINE (-99, 99)-(99, -99), col2, B
        LINE (100, 100)-(200, -100), col, B
        LINE (101, 99)-(199, -99), col2, B
        LINE (101, -60)-(199, -60), col2
        LINE (100, -61)-(200, -61), col
        LINE (101, -62)-(199, -62), col2
        LINE (102, 98)-(198, -55), 0, BF
        GOSUB drawit2
        GOSUB words
        SHOWMOUSE
RETURN


findclosest:

        holding = 1
        sdist = stol
        FOR aa = 1 TO numv

                dist = ((posx - vert(aa, 1)) ^ 2 + (posy - vert(aa, 2)) ^ 2) ^ (1 / 2)
                IF dist < sdist THEN
                        sdist = dist
                        onv = aa
                END IF
       NEXT aa
       IF sdist = stol THEN holding = 0
RETURN

tend:
OPEN "lathe.qbc" FOR OUTPUT AS #1
PRINT #1, yesno
CLOSE #1
END IF
END

SUB DBox (p$(), v(), acc(), num, boxname$, success)

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

LOCATE 17, 22
PRINT "ok"
LOCATE 17, 27
PRINT "cancel"


y = 5
x = 14

 FOR a = 1 TO num
   LOCATE y, x
   PRINT p$(a); ": "; v(a)
   y = y + 1
 NEXT a


'the main loop

DO

SHOWMOUSE

GETMOUSECOORD b%, x%, y%
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
    LOCATE 4 + a, 17 + LEN(p$(a))
    PRINT "_             "
    DO
     DO
      keystroke$ = INKEY$
      IF LEN(keystroke$) > 0 THEN keyval = ASC(keystroke$)
      IF LEN(keystroke$) > 0 AND keyval = 8 AND LEN(value$) > 0 THEN
       value$ = LEFT$(value$, LEN(value$) - 1)
       LOCATE 4 + a, 17 + LEN(p$(a))
       PRINT "             "
       LOCATE 4 + a, 17 + LEN(p$(a))
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
      LOCATE 4 + a, 17 + LEN(p$(a))
      PRINT value$
     END IF
    LOOP UNTIL enter = 1
    valu = VAL(value$)
    IF valu < acc(a, 1) THEN
     valid = 0
     LOCATE 15, 20
     PRINT "Must be >="; acc(a, 1)
     LOCATE 4 + a, 17 + LEN(p$(a))
     PRINT "_             "
    END IF
    IF valu > acc(a, 2) THEN
     valid = 0
     LOCATE 15, 20
     PRINT "Must be <="; acc(a, 2)
     LOCATE 4 + a, 17 + LEN(p$(a))
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

SUB GETMOUSECOORD (b%, x%, y%)
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

SUB HIDEMOUSE
ASM$ = ""                                         'HideMouse:
ASM$ = ASM$ + CHR$(&HB8) + MKI$(2)                'mov ax,2
ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H33)             'int 33h
ASM$ = ASM$ + CHR$(&HCB)                          'retf
DEF SEG = VARSEG(ASM$)
  CALL absolute(SADD(ASM$))
DEF SEG

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

