CONST COPYRIGHTNUMBER = "Reversi V2.5"
CONST COPYRIGHT = "Copyright (C) Sosei"
CONST PROGRAMNAME = "REVERSI"
CONST COMPUTERNAME = "Snowman"
CONST PI = 3.141592653589793#
CONST DEGREE = PI / 180
CONST CIRCLEANGLE = 2 * PI
CONST SECOND = 1, MINUTE = 60, HOUR = 60, HALFDAY = 12
CONST DAY = 2& * HALFDAY * HOUR * MINUTE * SECOND
CONST BOARDSIZE = 8
CONST MAXUNITIME = 2 * MINUTE, MAXEXTRATIME = 6 * MINUTE
CONST WARNTIME = 10 * SECOND
CONST HUMAN = 0, COMPUTER = 1
CONST OVER = -1, BLACKSIDE = 0, WHITESIDE = 1
CONST BLACKWIN = -1, EQUAL = 0, WHITEWIN = 1
CONST BLANK = -1, BLACKSTONE = 0, WHITESTONE = 1
CONST BEFORETIME = 0, NOWTIME = 1
CONST ORIGINALSCREEN = -1, BLACKSTATESCREEN = 0, WHITESTATESCREEN = 1, BOARDSCREEN = 2, BUTTONSCREEN = 3
CONST SET = 0, USE = 1
CONST TRUE = -1, FALSE = 0
CONST SHOW = 1, DISAPPEAR = 0
CONST REPOSE = 0, THINK = 1, LOSE = 2, QUIET = 3, WIN = 4
CONST INIT = 0, REST = 1, TURN = 2
CONST FINISH = -1, RENEW = 0
CONST BLACK = 0, WHITE = 3, RED = 2, BLUE = 1
CONST ZEROOFCLOCKANGLE = PI / 2
CONST RECORDFILENAME = "REVERSI.REC"
CONST RECORDVERSION = 1
CONST MAXMOVES = BOARDSIZE * BOARDSIZE
CONST BLACKNAMEROW = 1, BLACKSTONESROW = 2
CONST WHITENAMEROW = 11, WHITESTONESROW = 12
CONST RESIGNKEYROW = 21
CONST MESSAGE1ROW = 23, MESSAGE2ROW = 24, COPYRIGHTROW = 25
CONST MSGLINEWIDTH = 40
CONST KEYBACKSPACE = 8
CONST KEYENTER = 13
CONST KEYLEFT = 75 + 256, KEYRIGHT = 77 + 256, KEYUP = 72 + 256, KEYDOWN = 80 + 256

TYPE PLAYDATA
    playname AS STRING * 10
    playtype AS INTEGER
END TYPE

TYPE MOVE
    x AS INTEGER
    y AS INTEGER
END TYPE

TYPE GAMERECORD
    version AS INTEGER
    blackname AS STRING * 10
    whitename AS STRING * 10
    blacktype AS INTEGER
    whitetype AS INTEGER
    movecount AS INTEGER
    winner AS INTEGER
    blackstones AS INTEGER
    whitestones AS INTEGER
    gamedate AS STRING * 10
    gametime AS STRING * 8
    endreason AS STRING * 10
END TYPE

DIM SHARED recordmovesx(1 TO MAXMOVES) AS INTEGER
DIM SHARED recordmovesy(1 TO MAXMOVES) AS INTEGER
DIM SHARED recordmovesside(1 TO MAXMOVES) AS INTEGER

DECLARE FUNCTION GetRawKey ()
DECLARE SUB GetBoardInput (x AS INTEGER, y AS INTEGER, confirmed AS INTEGER, nowside AS INTEGER, board() AS INTEGER)
DECLARE FUNCTION GetInputWithBlink$ (prompt AS STRING, row AS INTEGER, col AS INTEGER, maxlen AS INTEGER, acceptkeys AS STRING, waitenter AS INTEGER)
DECLARE SUB RandomSide (gameorder AS INTEGER, bothname() AS PLAYDATA)
DECLARE SUB InputName (humanside AS INTEGER, bothname() AS PLAYDATA)
DECLARE SUB ReversiScreen (screentype AS INTEGER, screenstate AS INTEGER)
DECLARE SUB BoardInit (board() AS INTEGER)
DECLARE SUB StoneNumShow (board() AS INTEGER, stonenum() AS INTEGER)
DECLARE SUB FaceShow (nowside AS INTEGER, facetype AS INTEGER)
DECLARE SUB Clock (nowside AS INTEGER, clockstate AS INTEGER, recoverscreen AS INTEGER, unitime() AS LONG, extratime() AS LONG, notime AS INTEGER)
DECLARE SUB TurnStone (board() AS INTEGER, x AS INTEGER, y AS INTEGER)
DECLARE SUB OverJudge (nowside AS INTEGER, board() AS INTEGER)
DECLARE SUB HumanThink (nowside AS INTEGER, board() AS INTEGER, x AS INTEGER, y AS INTEGER)
DECLARE SUB ComputerThink (nowside AS INTEGER, board() AS INTEGER, x AS INTEGER, y AS INTEGER)
DECLARE SUB StoneShow (stone AS INTEGER, x AS INTEGER, y AS INTEGER)
DECLARE SUB LightMark (nowside AS INTEGER, background AS INTEGER, x AS INTEGER, y AS INTEGER, cursor AS INTEGER)
DECLARE FUNCTION CheckDirection (board() AS INTEGER, x AS INTEGER, y AS INTEGER, dx AS INTEGER, dy AS INTEGER, stone AS INTEGER, oppstone AS INTEGER)
DECLARE FUNCTION CheckPlace (nowside AS INTEGER, board() AS INTEGER, x AS INTEGER, y AS INTEGER)
DECLARE FUNCTION GetValidMoves (nowside AS INTEGER, board() AS INTEGER, moves() AS MOVE)
DECLARE FUNCTION EvaluatePosition (board() AS INTEGER, nowside AS INTEGER, x AS INTEGER, y AS INTEGER)
DECLARE FUNCTION LetterRange$ (startchar AS STRING, endchar AS STRING)
DECLARE FUNCTION FormatDateYYYYMMDD$ (datestr AS STRING)
DECLARE FUNCTION WinSideJudge (nowside AS INTEGER, notime AS INTEGER, accpetlose AS INTEGER, stonenum() AS INTEGER)
DECLARE SUB WinShow (winside AS INTEGER)
DECLARE SUB Quit (choose AS INTEGER)
DECLARE SUB InitGameRecord (record AS GAMERECORD, bothname() AS PLAYDATA)
DECLARE SUB AddMoveToRecord (nowside AS INTEGER, x AS INTEGER, y AS INTEGER)
DECLARE SUB FinalizeGameRecord (record AS GAMERECORD, winside AS INTEGER, stonenum() AS INTEGER, endreason AS STRING)
DECLARE SUB SaveGameRecord (record AS GAMERECORD)

DIM nn AS STRING
DIM nowscreen AS INTEGER
DIM gameorder AS INTEGER
DIM bothname(BLACKSIDE TO WHITESIDE) AS PLAYDATA
DIM stonenum(BLACKSIDE TO WHITESIDE) AS INTEGER
DIM unitime(BLACKSIDE TO WHITESIDE, BEFORETIME TO NOWTIME) AS LONG
DIM extratime(BLACKSIDE TO WHITESIDE, BEFORETIME TO NOWTIME) AS LONG
DIM notime AS INTEGER
DIM accpetlose AS INTEGER
DIM board(1 TO BOARDSIZE, 1 TO BOARDSIZE) AS INTEGER
DIM nowside AS INTEGER
DIM x AS INTEGER
DIM y AS INTEGER
DIM winside AS INTEGER
DIM choose AS INTEGER
DIM currentgame AS GAMERECORD
DIM endreason AS STRING

nn = STRING$(INT(LOG(BOARDSIZE * BOARDSIZE) / LOG(10#)) + 1, "#")
nowscreen = ORIGINALSCREEN
gameorder = 0
SCREEN 1
PLAY "mb"
ON TIMER(SECOND) GOSUB Clocklabel
ON KEY(10) GOSUB keylabel
DO
    CLS
    LOCATE COPYRIGHTROW, 3: PRINT COPYRIGHTNUMBER; "   "; COPYRIGHT;
    CALL RandomSide(gameorder, bothname())
    stonenum(BLACKSIDE) = 0
    stonenum(WHITESIDE) = 0
    LOCATE BLACKNAMEROW, 1: PRINT "Name:"; bothname(BLACKSIDE).playname
    LOCATE BLACKSTONESROW, 1: PRINT USING "Stones:" + nn; stonenum(BLACKSIDE);
    CALL ReversiScreen(BLACKSTATESCREEN, SET)
    CALL FaceShow(BLACKSIDE, REPOSE)
    CALL Clock(BLACKSIDE, INIT, FALSE, unitime(), extratime(), notime)
    LOCATE WHITENAMEROW, 1: PRINT "Name:"; bothname(WHITESIDE).playname
    LOCATE WHITESTONESROW, 1: PRINT USING "Stones:" + nn; stonenum(WHITESIDE);
    CALL ReversiScreen(WHITESTATESCREEN, SET)
    CALL FaceShow(WHITESIDE, REPOSE)
    CALL Clock(WHITESIDE, INIT, FALSE, unitime(), extratime(), notime)
    CALL ReversiScreen(BOARDSCREEN, SET)
    IF gameorder = 0 THEN
        IF bothname(BLACKSIDE).playtype = HUMAN THEN
            CALL InputName(BLACKSIDE, bothname())
        END IF
        IF bothname(WHITESIDE).playtype = HUMAN THEN
            CALL InputName(WHITESIDE, bothname())
        END IF
    END IF
    CALL BoardInit(board())
    FOR x = 1 TO BOARDSIZE
        FOR y = 1 TO BOARDSIZE
            CALL StoneShow(board(x, y), x, y)
        NEXT y
    NEXT x
    CALL StoneNumShow(board(), stonenum())
    LOCATE RESIGNKEYROW, 1: PRINT "F10=Resign";
    nowside = BLACKSIDE
    accpetlose = FALSE
    x = 4
    y = 4
   
    CALL InitGameRecord(currentgame, bothname())
   
    gameorder = gameorder + 1
    KEY(10) ON
    PLAY "c8d8"
    DO UNTIL nowside = OVER
        CALL FaceShow(nowside, THINK)
        IF bothname(nowside).playtype = HUMAN THEN
            CALL HumanThink(nowside, board(), x, y)
        ELSEIF bothname(nowside).playtype = COMPUTER THEN
            CALL ComputerThink(nowside, board(), x, y)
        END IF
        CALL FaceShow(nowside, REPOSE)
        CALL TurnStone(board(), x, y)
        CALL StoneNumShow(board(), stonenum())
        CALL AddMoveToRecord(nowside, x, y)
        CALL OverJudge(nowside, board())
    LOOP

overlabel:
    TIMER OFF
    KEY(10) OFF
    winside = WinSideJudge(nowside, notime, accpetlose, stonenum())
   
    IF accpetlose = TRUE THEN
        endreason = "RESIGN"
    ELSEIF notime = TRUE THEN
        endreason = "TIME_OUT"
    ELSE
        endreason = "NORMAL_END"
    END IF
   
    CALL WinShow(winside)
    LOCATE RESIGNKEYROW, 1: PRINT SPACE$(10);
    CALL FinalizeGameRecord(currentgame, winside, stonenum(), endreason)
    CALL SaveGameRecord(currentgame)
    CALL Quit(choose)
LOOP UNTIL choose = FINISH
PLAY "mf"
SCREEN 0
CLS
END

Clocklabel:
KEY(10) STOP
CALL Clock(nowside, TURN, TRUE, unitime(), extratime(), notime)
IF notime = FALSE THEN
    KEY(10) ON
    RETURN
ELSEIF notime = TRUE THEN
    nowside = OVER
    RETURN overlabel
END IF

keylabel:
TIMER OFF
KEY(10) OFF
accpetlose = TRUE
RETURN overlabel

SUB AddMoveToRecord (nowside AS INTEGER, x AS INTEGER, y AS INTEGER)
    SHARED currentgame AS GAMERECORD
   
    currentgame.movecount = currentgame.movecount + 1
    IF currentgame.movecount <= MAXMOVES THEN
        recordmovesx(currentgame.movecount) = x
        recordmovesy(currentgame.movecount) = y
        recordmovesside(currentgame.movecount) = nowside
    END IF
END SUB

SUB BoardInit (board() AS INTEGER)
    DIM x AS INTEGER
    DIM y AS INTEGER
  
    FOR x = 1 TO BOARDSIZE
        FOR y = 1 TO BOARDSIZE
            board(x, y) = BLANK
        NEXT y
    NEXT x
    board(BOARDSIZE / 2, BOARDSIZE / 2 + 1) = BLACKSTONE
    board(BOARDSIZE / 2 + 1, BOARDSIZE / 2 + 1) = WHITESTONE
    board(BOARDSIZE / 2, BOARDSIZE / 2) = WHITESTONE
    board(BOARDSIZE / 2 + 1, BOARDSIZE / 2) = BLACKSTONE
END SUB

FUNCTION CheckDirection (board() AS INTEGER, x AS INTEGER, y AS INTEGER, dx AS INTEGER, dy AS INTEGER, stone AS INTEGER, oppstone AS INTEGER)
    DIM i AS INTEGER, j AS INTEGER
    
    i = x + dx
    j = y + dy
    
    IF i < 1 OR i > BOARDSIZE OR j < 1 OR j > BOARDSIZE THEN
        CheckDirection = FALSE
        EXIT FUNCTION
    END IF
    
    IF board(i, j) <> oppstone THEN
        CheckDirection = FALSE
        EXIT FUNCTION
    END IF
    
    i = i + dx
    j = j + dy
    DO WHILE i >= 1 AND i <= BOARDSIZE AND j >= 1 AND j <= BOARDSIZE
        IF board(i, j) = stone THEN
            CheckDirection = TRUE
            EXIT FUNCTION
        ELSEIF board(i, j) = BLANK THEN
            EXIT DO
        END IF
        i = i + dx
        j = j + dy
    LOOP
    
    CheckDirection = FALSE
END FUNCTION

FUNCTION CheckPlace (nowside AS INTEGER, board() AS INTEGER, x AS INTEGER, y AS INTEGER)
   
    DIM stone AS INTEGER
    DIM oppstone AS INTEGER
    DIM dx AS INTEGER, dy AS INTEGER
    
    IF board(x, y) <> BLANK THEN
        CheckPlace = FALSE
        EXIT FUNCTION
    END IF
    
    IF nowside = BLACKSIDE THEN
        stone = BLACKSTONE
        oppstone = WHITESTONE
    ELSE
        stone = WHITESTONE
        oppstone = BLACKSTONE
    END IF
    
    FOR dx = -1 TO 1
        FOR dy = -1 TO 1
            IF NOT (dx = 0 AND dy = 0) THEN
                IF CheckDirection(board(), x, y, dx, dy, stone, oppstone) THEN
                    CheckPlace = TRUE
                    EXIT FUNCTION
                END IF
            END IF
        NEXT dy
    NEXT dx
    
    CheckPlace = FALSE
END FUNCTION

SUB Clock (nowside AS INTEGER, clockstate AS INTEGER, recoverscreen AS INTEGER, unitime() AS LONG, extratime() AS LONG, notime AS INTEGER) STATIC
    CONST SA = CIRCLEANGLE / MINUTE
    CONST MA = CIRCLEANGLE / HOUR
    CONST HA = CIRCLEANGLE / HALFDAY
  
    SHARED nowscreen AS INTEGER
  
    DIM unitclockface AS INTEGER
    DIM unitclockpointer AS INTEGER
    DIM extraclockface AS INTEGER
    DIM extraclockpointer AS INTEGER
    DIM befscreen AS INTEGER
    DIM a AS SINGLE
    DIM ot AS INTEGER
    DIM nt AS INTEGER
    DIM os AS INTEGER
    DIM om AS INTEGER
    DIM oh AS INTEGER
    DIM unitsecond(BLACKSIDE TO WHITESIDE) AS INTEGER
    DIM unitminute(BLACKSIDE TO WHITESIDE) AS INTEGER
    DIM unithour(BLACKSIDE TO WHITESIDE) AS INTEGER
    DIM extrasecond(BLACKSIDE TO WHITESIDE) AS INTEGER
    DIM extraminute(BLACKSIDE TO WHITESIDE) AS INTEGER
    DIM extrahour(BLACKSIDE TO WHITESIDE) AS INTEGER
  
    IF clockstate = INIT THEN
        unitime(nowside, BEFORETIME) = -1
        unitime(nowside, NOWTIME) = MAXUNITIME
        extratime(nowside, BEFORETIME) = -1
        extratime(nowside, NOWTIME) = MAXEXTRATIME
        notime = FALSE
        unitclockface = TRUE: unitclockpointer = TRUE
        extraclockface = TRUE: extraclockpointer = TRUE
    ELSEIF clockstate = REST THEN
        unitime(nowside, BEFORETIME) = unitime(nowside, NOWTIME)
        unitime(nowside, NOWTIME) = MAXUNITIME
        unitclockface = FALSE: unitclockpointer = TRUE
        extraclockface = FALSE: extraclockpointer = FALSE
    ELSEIF clockstate = TURN THEN
        IF unitime(nowside, NOWTIME) > 0 THEN
            unitime(nowside, BEFORETIME) = unitime(nowside, NOWTIME)
            unitime(nowside, NOWTIME) = unitime(nowside, NOWTIME) - SECOND
            unitclockface = FALSE: unitclockpointer = TRUE
            extraclockface = FALSE: extraclockpointer = FALSE
        ELSEIF unitime(nowside, NOWTIME) = 0 THEN
            extratime(nowside, BEFORETIME) = extratime(nowside, NOWTIME)
            extratime(nowside, NOWTIME) = extratime(nowside, NOWTIME) - SECOND
            unitclockface = FALSE: unitclockpointer = FALSE
            extraclockface = FALSE: extraclockpointer = TRUE
            IF extratime(nowside, NOWTIME) = 0 THEN
                notime = TRUE
            END IF
        END IF
    END IF
    IF recoverscreen = TRUE THEN
        befscreen = nowscreen
    END IF
    IF nowside = BLACKSIDE THEN
        CALL ReversiScreen(BLACKSTATESCREEN, USE)
    ELSEIF nowside = WHITESIDE THEN
        CALL ReversiScreen(WHITESTATESCREEN, USE)
    END IF
    IF unitclockface = TRUE THEN
        CIRCLE (11, 11), 10, WHITE: PAINT (11, 11), BLACK, WHITE
        CIRCLE (11, 11), .5, WHITE
        FOR a = ZEROOFCLOCKANGLE TO ZEROOFCLOCKANGLE - CIRCLEANGLE STEP -(CIRCLEANGLE / HALFDAY)
            LINE (9 * COS(a) + 11, 9 * SIN(a) + 11)-(8.5 * COS(a) + 11, 8.5 * SIN(a) + 11), WHITE
        NEXT a
    END IF
    IF unitclockpointer = TRUE THEN
        IF unitime(nowside, NOWTIME) < WARNTIME THEN
            IF unitime(nowside, NOWTIME) < MAXUNITIME THEN
                PLAY "c8"
            END IF
        END IF
        ot = unitime(nowside, BEFORETIME)
        nt = unitime(nowside, NOWTIME)
        IF nt <> ot THEN
            os = ot MOD MINUTE
            unitsecond(nowside) = nt MOD MINUTE
            a = ZEROOFCLOCKANGLE - SA * os
            LINE (1 * COS(a) + 11, 1 * SIN(a) + 11)-(7 * COS(a) + 11, 7 * SIN(a) + 11), BLACK
            ot = INT(ot / MINUTE)
            nt = INT(nt / MINUTE)
            IF nt <> ot THEN
                om = ot MOD HOUR
                unitminute(nowside) = nt MOD HOUR
                a = ZEROOFCLOCKANGLE - MA * om
                LINE (1 * COS(a) + 11, 1 * SIN(a) + 11)-(6 * COS(a) + 11, 6 * SIN(a) + 11), BLACK
                ot = INT(ot / HOUR)
                nt = INT(nt / HOUR)
                IF nt <> ot THEN
                    oh = ot MOD HALFDAY
                    unithour(nowside) = nt MOD HALFDAY
                    a = ZEROOFCLOCKANGLE - HA * oh
                    LINE (1 * COS(a) + 11, 1 * SIN(a) + 11)-(4.5 * COS(a) + 11, 4.5 * SIN(a) + 11), BLACK
                END IF
            END IF
            a = ZEROOFCLOCKANGLE - SA * unitsecond(nowside)
            LINE (1 * COS(a) + 11, 1 * SIN(a) + 11)-(7 * COS(a) + 11, 7 * SIN(a) + 11), RED
            a = ZEROOFCLOCKANGLE - MA * unitminute(nowside)
            LINE (1 * COS(a) + 11, 1 * SIN(a) + 11)-(6 * COS(a) + 11, 6 * SIN(a) + 11), BLUE
            a = ZEROOFCLOCKANGLE - HA * unithour(nowside)
            LINE (1 * COS(a) + 11, 1 * SIN(a) + 11)-(4.5 * COS(a) + 11, 4.5 * SIN(a) + 11), WHITE
        END IF
    END IF
    IF extraclockface = TRUE THEN
        CIRCLE (35, 15.5), 14, WHITE: PAINT (35, 15.5), BLACK, WHITE
        CIRCLE (35, 15.5), .5, WHITE
        FOR a = ZEROOFCLOCKANGLE TO ZEROOFCLOCKANGLE - CIRCLEANGLE STEP -(CIRCLEANGLE / HALFDAY)
            LINE (13.5 * COS(a) + 35, 13.5 * SIN(a) + 15.5)-(12 * COS(a) + 35, 12 * SIN(a) + 15.5), WHITE
        NEXT a
    END IF
    IF extraclockpointer = TRUE THEN
        ot = extratime(nowside, BEFORETIME)
        nt = extratime(nowside, NOWTIME)
        IF nt <> ot THEN
            os = ot MOD MINUTE
            extrasecond(nowside) = nt MOD MINUTE
            IF extrasecond(nowside) = 0 THEN
                IF extratime(nowside, NOWTIME) < MAXEXTRATIME THEN
                    PLAY "e8"
                END IF
            END IF
            a = ZEROOFCLOCKANGLE - SA * os
            LINE (1 * COS(a) + 35, 1 * SIN(a) + 15.5)-(11 * COS(a) + 35, 11 * SIN(a) + 15.5), BLACK
            ot = INT(ot / MINUTE)
            nt = INT(nt / MINUTE)
            IF nt <> ot THEN
                om = ot MOD HOUR
                extraminute(nowside) = nt MOD HOUR
                a = ZEROOFCLOCKANGLE - MA * om
                LINE (1 * COS(a) + 35, 1 * SIN(a) + 15.5)-(9 * COS(a) + 35, 9 * SIN(a) + 15.5), BLACK
                ot = INT(ot / HOUR)
                nt = INT(nt / HOUR)
                IF nt <> ot THEN
                    oh = ot MOD HALFDAY
                    extrahour(nowside) = nt MOD HALFDAY
                    a = ZEROOFCLOCKANGLE - HA * oh
                    LINE (1 * COS(a) + 35, 1 * SIN(a) + 15.5)-(7 * COS(a) + 35, 7 * SIN(a) + 15.5), BLACK
                END IF
            END IF
            a = ZEROOFCLOCKANGLE - SA * extrasecond(nowside)
            LINE (1 * COS(a) + 35, 1 * SIN(a) + 15.5)-(11 * COS(a) + 35, 11 * SIN(a) + 15.5), RED
            a = ZEROOFCLOCKANGLE - MA * extraminute(nowside)
            LINE (1 * COS(a) + 35, 1 * SIN(a) + 15.5)-(9 * COS(a) + 35, 9 * SIN(a) + 15.5), BLUE
            a = ZEROOFCLOCKANGLE - HA * extrahour(nowside)
            LINE (1 * COS(a) + 35, 1 * SIN(a) + 15.5)-(7 * COS(a) + 35, 7 * SIN(a) + 15.5), WHITE
        END IF
    END IF
    IF recoverscreen = TRUE THEN
        CALL ReversiScreen(befscreen, USE)
    END IF
END SUB

SUB ComputerThink (nowside AS INTEGER, board() AS INTEGER, x AS INTEGER, y AS INTEGER)
   
    DIM moves(1 TO BOARDSIZE * BOARDSIZE) AS MOVE
    DIM movecount AS INTEGER
    DIM i AS INTEGER
    DIM bestmove AS INTEGER
    DIM bestscore AS INTEGER
    DIM currentscore AS INTEGER
    
    TIMER ON
    
    movecount = GetValidMoves(nowside, board(), moves())
   
    IF movecount = 0 THEN
        EXIT SUB
    END IF
   
    bestmove = 1
    bestscore = EvaluatePosition(board(), nowside, moves(1).x, moves(1).y)
   
    FOR i = 2 TO movecount
        currentscore = EvaluatePosition(board(), nowside, moves(i).x, moves(i).y)
        IF currentscore > bestscore THEN
            bestscore = currentscore
            bestmove = i
        END IF
    NEXT i
   
    RANDOMIZE TIMER
    IF RND < .2 THEN
        bestmove = INT(RND * movecount) + 1
    END IF
   
    x = moves(bestmove).x
    y = moves(bestmove).y
    
    
    IF nowside = BLACKSIDE THEN
        board(x, y) = BLACKSTONE
    ELSEIF nowside = WHITESIDE THEN
        board(x, y) = WHITESTONE
    END IF
    
    TIMER OFF
    CALL StoneShow(board(x, y), x, y)
END SUB

FUNCTION EvaluatePosition (board() AS INTEGER, nowside AS INTEGER, x AS INTEGER, y AS INTEGER)

    DIM score AS INTEGER
    DIM stone AS INTEGER
    DIM oppstone AS INTEGER
    DIM dx AS INTEGER, dy AS INTEGER
    DIM i AS INTEGER, j AS INTEGER
    DIM flipped AS INTEGER
   
    IF nowside = BLACKSIDE THEN
        stone = BLACKSTONE
        oppstone = WHITESTONE
    ELSE
        stone = WHITESTONE
        oppstone = BLACKSTONE
    END IF
   
    score = 0
   
    flipped = 0
    FOR dx = -1 TO 1
        FOR dy = -1 TO 1
            IF NOT (dx = 0 AND dy = 0) THEN
                i = x + dx
                j = y + dy
                IF i >= 1 AND i <= BOARDSIZE AND j >= 1 AND j <= BOARDSIZE THEN
                    IF board(i, j) = oppstone THEN
                        i = i + dx
                        j = j + dy
                        DO WHILE i >= 1 AND i <= BOARDSIZE AND j >= 1 AND j <= BOARDSIZE
                            IF board(i, j) = stone THEN
                                flipped = flipped + ABS(x - i) + ABS(y - j) - 1
                                EXIT DO
                            ELSEIF board(i, j) = BLANK THEN
                                EXIT DO
                            END IF
                            i = i + dx
                            j = j + dy
                        LOOP
                    END IF
                END IF
            END IF
        NEXT dy
    NEXT dx
   
    score = score + flipped
   
    IF (x = 1 AND y = 1) OR (x = 1 AND y = BOARDSIZE) OR (x = BOARDSIZE AND y = 1) OR (x = BOARDSIZE AND y = BOARDSIZE) THEN
        score = score + 100
    END IF
   
    IF x = 1 OR x = BOARDSIZE OR y = 1 OR y = BOARDSIZE THEN
        score = score + 10
    END IF
   
    IF (x = 2 AND y = 2) OR (x = 2 AND y = BOARDSIZE - 1) OR (x = BOARDSIZE - 1 AND y = 2) OR (x = BOARDSIZE - 1 AND y = BOARDSIZE - 1) THEN
        score = score - 50
    END IF
   
    IF (x = 1 AND (y = 2 OR y = BOARDSIZE - 1)) OR (x = BOARDSIZE AND (y = 2 OR y = BOARDSIZE - 1)) OR (y = 1 AND (x = 2 OR x = BOARDSIZE - 1)) OR (y = BOARDSIZE AND (x = 2 OR x = BOARDSIZE - 1)) THEN
        score = score - 30
    END IF
   
    IF x >= 3 AND x <= BOARDSIZE - 2 AND y >= 3 AND y <= BOARDSIZE - 2 THEN
        score = score + 5
    END IF
   
    EvaluatePosition = score
END FUNCTION

SUB FaceShow (nowside AS INTEGER, facetype AS INTEGER)
    DIM forecolor AS INTEGER
    DIM backcolor AS INTEGER

    IF nowside = BLACKSIDE THEN
        CALL ReversiScreen(BLACKSTATESCREEN, USE)
    ELSEIF nowside = WHITESIDE THEN
        CALL ReversiScreen(WHITESTATESCREEN, USE)
    END IF
    IF nowside = BLACKSIDE THEN
        forecolor = WHITE
        backcolor = BLACK
    ELSEIF nowside = WHITESIDE THEN
        forecolor = BLACK
        backcolor = WHITE
    END IF
    CIRCLE (6, 25), 3.9, RED
    PAINT (6, 25), BLUE, RED
    PAINT (6, 25), backcolor, RED
    IF facetype = THINK THEN
        CIRCLE (4.8, 26), .6, forecolor
        CIRCLE (7.2, 26), .6, forecolor
        LINE (5, 23)-(7, 23), forecolor
    ELSEIF facetype = LOSE THEN
        CIRCLE (4.8, 26), .6, forecolor
        CIRCLE (7.2, 26), .6, forecolor
        CIRCLE (6, 21), 2.4, forecolor, 60 * DEGREE, 120 * DEGREE
    ELSEIF facetype = QUIET THEN
        CIRCLE (4.8, 26), .6, forecolor
        CIRCLE (7.2, 26), .6, forecolor
        CIRCLE (6, 23), 1, forecolor, , , .4
    ELSEIF facetype = WIN THEN
        CIRCLE (4.8, 26), .6, forecolor
        CIRCLE (7.2, 26), .6, forecolor
        CIRCLE (6, 25), 2, forecolor, 240 * DEGREE, 300 * DEGREE
    END IF
END SUB

SUB FinalizeGameRecord (record AS GAMERECORD, winside AS INTEGER, stonenum() AS INTEGER, endreason AS STRING)
    record.winner = winside
    record.blackstones = stonenum(BLACKSIDE)
    record.whitestones = stonenum(WHITESIDE)
    record.endreason = endreason
END SUB

FUNCTION FormatDateYYYYMMDD$ (datestr AS STRING)
    DIM MonthDayPart AS STRING
    DIM YearPart AS STRING
   
    YearPart = RIGHT$(datestr, 4)
    MonthDayPart = LEFT$(datestr, 5)
   
    FormatDateYYYYMMDD = YearPart + "-" + MonthDayPart
END FUNCTION

SUB GetBoardInput (x AS INTEGER, y AS INTEGER, confirmed AS INTEGER, nowside AS INTEGER, board() AS INTEGER)
    CONST twinkletime = 1 / 3
    DIM keycode AS INTEGER
    DIM cursorvisible AS INTEGER
    DIM lastblinktime AS SINGLE
    DIM needredraw AS INTEGER
    
    confirmed = FALSE
    cursorvisible = SHOW
    lastblinktime = TIMER
    needredraw = TRUE
    
    DO
        IF TIMER - lastblinktime >= twinkletime THEN
            cursorvisible = 1 - cursorvisible
            needredraw = TRUE
            lastblinktime = TIMER
        END IF
        
        IF needredraw THEN
            CALL LightMark(nowside, board(x, y), x, y, cursorvisible)
            needredraw = FALSE
        END IF
        
        keycode = GetRawKey
        
        IF keycode > 0 THEN
            CALL LightMark(nowside, board(x, y), x, y, DISAPPEAR)
            
            SELECT CASE keycode
                CASE KEYLEFT
                    x = x - 1
                    IF x < 1 THEN x = BOARDSIZE
                CASE KEYRIGHT
                    x = x + 1
                    IF x > BOARDSIZE THEN x = 1
                CASE KEYUP
                    y = y + 1
                    IF y > BOARDSIZE THEN y = 1
                CASE KEYDOWN
                    y = y - 1
                    IF y < 1 THEN y = BOARDSIZE
                CASE KEYENTER
                    confirmed = TRUE
                    EXIT DO
            END SELECT
            needredraw = TRUE
        END IF
    LOOP
    
    CALL LightMark(nowside, board(x, y), x, y, DISAPPEAR)
END SUB

FUNCTION GetInputWithBlink$ (prompt AS STRING, row AS INTEGER, col AS INTEGER, maxlen AS INTEGER, acceptkeys AS STRING, waitenter AS INTEGER)
    CONST twinkletime = 1 / 6
    DIM reftime AS SINGLE
    DIM timebalance AS SINGLE
    DIM cw AS STRING * 1
    DIM ct AS STRING * 1
    DIM keycode AS INTEGER
    DIM keychar AS STRING
    DIM result AS STRING
    DIM i AS INTEGER
    
    result = ""
    i = 0
    cw = "?"
    ct = " "
    reftime = TIMER
    
    DO
        LOCATE row, col: PRINT prompt + result + cw + SPACE$(maxlen - LEN(prompt + result + cw));
        
        timebalance = TIMER - reftime
        timebalance = timebalance + DAY
        timebalance = timebalance - FIX(timebalance / DAY) * DAY
        IF timebalance >= twinkletime THEN
            SWAP cw, ct
            reftime = TIMER
        END IF
        
        keycode = GetRawKey
        
        IF keycode > 0 AND keycode < 256 THEN
            keychar = CHR$(keycode)
            IF keycode = KEYENTER THEN
                EXIT DO
            ELSEIF keycode = KEYBACKSPACE AND i > 0 THEN
                i = i - 1
                result = LEFT$(result, i)
            ELSEIF INSTR(acceptkeys, keychar) > 0 THEN
                result = result + keychar
                i = i + 1
                IF waitenter = FALSE THEN
                    EXIT DO
                END IF
            END IF
        END IF
    LOOP UNTIL i = maxlen

    GetInputWithBlink = result
END FUNCTION

FUNCTION GetRawKey
    DIM check AS STRING
    
    check = INKEY$
    IF check = "" THEN
        GetRawKey = 0
    ELSE
        IF ASC(check) = 0 AND LEN(check) > 1 THEN
            GetRawKey = ASC(MID$(check, 2, 1)) + 256
        ELSE
            GetRawKey = ASC(check)
        END IF
    END IF
END FUNCTION

FUNCTION GetValidMoves (nowside AS INTEGER, board() AS INTEGER, moves() AS MOVE)
    DIM x AS INTEGER, y AS INTEGER
    DIM count AS INTEGER
   
    count = 0
    FOR x = 1 TO BOARDSIZE
        FOR y = 1 TO BOARDSIZE
            IF board(x, y) = BLANK THEN
                IF CheckPlace(nowside, board(), x, y) THEN
                    count = count + 1
                    moves(count).x = x
                    moves(count).y = y
                END IF
            END IF
        NEXT y
    NEXT x
   
    GetValidMoves = count
END FUNCTION

SUB HumanThink (nowside AS INTEGER, board() AS INTEGER, x AS INTEGER, y AS INTEGER)
    SHARED timetrap AS INTEGER
    DIM confirmed AS INTEGER
    DIM validmove AS INTEGER
    
    TIMER ON
    
    LOCATE MESSAGE1ROW, 1
    PRINT SPACE$(MSGLINEWIDTH);
    
    DO
        confirmed = FALSE
        CALL GetBoardInput(x, y, confirmed, nowside, board())
        
        IF confirmed THEN
            validmove = CheckPlace(nowside, board(), x, y)
            
            IF validmove THEN
                EXIT DO
            ELSE
                BEEP
            END IF
        END IF
    LOOP
    
    IF nowside = BLACKSIDE THEN
        board(x, y) = BLACKSTONE
    ELSEIF nowside = WHITESIDE THEN
        board(x, y) = WHITESTONE
    END IF
    
    TIMER OFF
    CALL StoneShow(board(x, y), x, y)
END SUB

SUB InitGameRecord (record AS GAMERECORD, bothname() AS PLAYDATA)
    DIM i as INTEGER
    
    record.version = RECORDVERSION
    record.blackname = bothname(BLACKSIDE).playname
    record.whitename = bothname(WHITESIDE).playname
    record.blacktype = bothname(BLACKSIDE).playtype
    record.whitetype = bothname(WHITESIDE).playtype
    record.movecount = 0
    record.winner = EQUAL
    record.blackstones = 0
    record.whitestones = 0
    record.gamedate = FormatDateYYYYMMDD$(DATE$)
    record.gametime = TIME$
    record.endreason = "NORMAL_END"
   
    FOR i = 1 TO MAXMOVES
        recordmovesx(i) = 0
        recordmovesy(i) = 0
        recordmovesside(i) = 0
    NEXT i
END SUB

SUB InputName (humanside AS INTEGER, bothname() AS PLAYDATA)
    CONST BLACKW = 1
    CONST BLACKH = 6
    CONST WHITEW = 11
    CONST WHITEH = 6
   
    DIM w AS INTEGER
    DIM h AS INTEGER
    DIM humanname AS STRING
  
    IF humanside = BLACKSIDE THEN
        w = BLACKW
        h = BLACKH
    ELSEIF humanside = WHITESIDE THEN
        w = WHITEW
        h = WHITEH
    END IF
    
    humanname = GetInputWithBlink("", w, h, 10, LetterRange("A", "Z") + LetterRange("a", "z") + " -", TRUE)
    
    bothname(humanside).playname = humanname
    LOCATE w, h: PRINT bothname(humanside).playname
END SUB

FUNCTION LetterRange$ (startchar AS STRING, endchar AS STRING)
    DIM result AS STRING
    DIM i AS INTEGER
    
    result = ""
    FOR i = ASC(startchar) TO ASC(endchar)
        result = result + CHR$(i)
    NEXT i
    LetterRange = result
END FUNCTION

SUB LightMark (nowside AS INTEGER, background AS INTEGER, x AS INTEGER, y AS INTEGER, cursor AS INTEGER)
    CALL ReversiScreen(BOARDSCREEN, USE)
    IF cursor = SHOW THEN
        IF background = BLANK THEN
            IF nowside = BLACKSIDE THEN
                CIRCLE (x - .5, y - .5), .2, BLACK
                PAINT (x - .5, y - .5), BLACK, BLACK
            ELSEIF nowside = WHITESIDE THEN
                CIRCLE (x - .5, y - .5), .2, WHITE
                PAINT (x - .5, y - .5), WHITE, WHITE
            END IF
        ELSE
            CIRCLE (x - .5, y - .5), .2, RED
            PAINT (x - .5, y - .5), RED, RED
        END IF
    ELSEIF cursor = DISAPPEAR THEN
        IF background = BLANK THEN
            PAINT (x - .5, y - .5), BLUE, BLUE
        ELSEIF background = BLACKSTONE THEN
            PAINT (x - .5, y - .5), BLACK, BLACK
        ELSEIF background = WHITESTONE THEN
            PAINT (x - .5, y - .5), WHITE, WHITE
        END IF
    END IF
END SUB

SUB OverJudge (nowside AS INTEGER, board() AS INTEGER)
    DIM refnowside AS INTEGER
    DIM x AS INTEGER
    DIM y AS INTEGER
  
    IF nowside = OVER THEN EXIT SUB
  
    refnowside = nowside
    DO
        IF nowside = BLACKSIDE THEN
            nowside = WHITESIDE
        ELSEIF nowside = WHITESIDE THEN
            nowside = BLACKSIDE
        END IF
        FOR x = 1 TO BOARDSIZE
            FOR y = 1 TO BOARDSIZE
                IF board(x, y) = BLANK THEN
                    IF CheckPlace(nowside, board(), x, y) = TRUE THEN
                        EXIT SUB
                    END IF
                END IF
            NEXT y
        NEXT x
    LOOP UNTIL nowside = refnowside
    nowside = OVER
END SUB

SUB Quit (choose AS INTEGER)
    DIM check AS STRING
    
    check = UCASE$(GetInputWithBlink("Quit(Y/N)", MESSAGE2ROW, 1, MESSAGE1ROW, "YNyn", FALSE))
    
    IF check = "Y" THEN
        choose = FINISH
    ELSE
        choose = RENEW
    END IF
    
    LOCATE MESSAGE1ROW, 1
    PRINT SPACE$(MSGLINEWIDTH);
    LOCATE MESSAGE2ROW, 1
    PRINT SPACE$(MSGLINEWIDTH);
END SUB

SUB RandomSide (gameorder AS INTEGER, bothname() AS PLAYDATA)
    DIM cl AS STRING
   
    IF gameorder = 0 THEN
        cl = UCASE$(COMMAND$)
        IF cl = "/C-C" THEN
            bothname(BLACKSIDE).playname = COMPUTERNAME
            bothname(BLACKSIDE).playtype = COMPUTER
            bothname(WHITESIDE).playname = COMPUTERNAME
            bothname(WHITESIDE).playtype = COMPUTER
        ELSEIF cl = "/H-H" THEN
            bothname(BLACKSIDE).playname = ""
            bothname(BLACKSIDE).playtype = HUMAN
            bothname(WHITESIDE).playname = ""
            bothname(WHITESIDE).playtype = HUMAN
        ELSE
            RANDOMIZE TIMER
            IF RND < .5 THEN
                bothname(BLACKSIDE).playname = ""
                bothname(BLACKSIDE).playtype = HUMAN
                bothname(WHITESIDE).playname = COMPUTERNAME
                bothname(WHITESIDE).playtype = COMPUTER
            ELSE
                bothname(BLACKSIDE).playname = COMPUTERNAME
                bothname(BLACKSIDE).playtype = COMPUTER
                bothname(WHITESIDE).playname = ""
                bothname(WHITESIDE).playtype = HUMAN
            END IF
        END IF
    ELSE
        SWAP bothname(BLACKSIDE).playname, bothname(WHITESIDE).playname
        SWAP bothname(BLACKSIDE).playtype, bothname(WHITESIDE).playtype
    END IF
END SUB

SUB ReversiScreen (screentype AS INTEGER, screenstate AS INTEGER)
    SHARED nowscreen AS INTEGER

    DIM i AS INTEGER

    IF screentype = BLACKSTATESCREEN THEN
        IF screenstate = SET THEN
            nowscreen = BLACKSTATESCREEN
            VIEW (1, 18)-(120, 77), BLUE, WHITE
            WINDOW (1, 1)-(50, 30)
        ELSEIF screenstate = USE AND nowscreen <> BLACKSTATESCREEN THEN
            nowscreen = BLACKSTATESCREEN
            VIEW (1, 18)-(120, 77)
            WINDOW (1, 1)-(50, 30)
        END IF
    ELSEIF screentype = WHITESTATESCREEN THEN
        IF screenstate = SET THEN
            nowscreen = WHITESTATESCREEN
            VIEW (1, 98)-(120, 157), BLUE, WHITE
            WINDOW (1, 1)-(50, 30)
        ELSEIF screenstate = USE AND nowscreen <> WHITESTATESCREEN THEN
            nowscreen = WHITESTATESCREEN
            VIEW (1, 98)-(120, 157)
            WINDOW (1, 1)-(50, 30)
        END IF
    ELSEIF screentype = BOARDSCREEN THEN
        IF screenstate = SET THEN
            nowscreen = BOARDSCREEN
            VIEW (126, 1)-(318, 161), BLUE, WHITE
            WINDOW (0, 0)-(BOARDSIZE, BOARDSIZE)
            FOR i = 0 TO BOARDSIZE
                LINE (i, 0)-(i, BOARDSIZE), BLACK
            NEXT i
            FOR i = 0 TO BOARDSIZE
                LINE (0, i)-(BOARDSIZE, i), BLACK
            NEXT i
        ELSEIF screenstate = USE AND nowscreen <> BOARDSCREEN THEN
            nowscreen = BOARDSCREEN
            VIEW (126, 1)-(318, 161)
            WINDOW (0, 0)-(BOARDSIZE, BOARDSIZE)
        END IF
    ELSEIF screentype = BUTTONSCREEN THEN
    END IF
END SUB

SUB SaveGameRecord (record AS GAMERECORD)
    DIM fileNum AS INTEGER
    DIM i AS INTEGER
    DIM blackname AS STRING
    DIM whitename AS STRING
    DIM resulttext AS STRING
    DIM sidechar AS STRING
    DIM colletter AS STRING
    DIM rownum AS STRING
    DIM reasontext AS STRING
   
    fileNum = FREEFILE
   
    OPEN RECORDFILENAME FOR APPEND AS fileNum
   
    blackname = RTRIM$(record.blackname)
    whitename = RTRIM$(record.whitename)
    IF blackname = "" THEN blackname = "Black"
    IF whitename = "" THEN whitename = "White"
   
    PRINT #fileNum, "=============================="
    PRINT #fileNum, "Version: "; LTRIM$(STR$(record.version))
    PRINT #fileNum, "Game: "; record.gamedate; " "; record.gametime
    PRINT #fileNum, "Players:"
    PRINT #fileNum, "  Black: "; blackname;
    IF record.blacktype = COMPUTER THEN PRINT #fileNum, " (Computer)" ELSE PRINT #fileNum, " (Human)"
    PRINT #fileNum, "  White: "; whitename;
    IF record.whitetype = COMPUTER THEN PRINT #fileNum, " (Computer)" ELSE PRINT #fileNum, " (Human)"
    PRINT #fileNum, "Board: "; LTRIM$(STR$(BOARDSIZE)); "x"; LTRIM$(STR$(BOARDSIZE))
    PRINT #fileNum, ""
   
    PRINT #fileNum, "Moves ("; record.movecount; "moves):"
   
    FOR i = 1 TO record.movecount
        IF recordmovesside(i) = BLACKSIDE THEN
            sidechar = "Black"
        ELSE
            sidechar = "White"
        END IF
      
        colletter = CHR$(ASC("a") + (recordmovesx(i) - 1))
        rownum = LTRIM$(STR$(recordmovesy(i)))
      
        IF i MOD 2 = 1 THEN
            PRINT #fileNum, USING "####: & &&"; i; sidechar; colletter; rownum;
        ELSE
            PRINT #fileNum, USING " - & &&"; sidechar; colletter; rownum
        END IF
    NEXT i
   
    IF record.movecount MOD 2 = 1 THEN
        PRINT #fileNum, ""
    END IF
   
    PRINT #fileNum, ""
    PRINT #fileNum, "Result:"
   
    SELECT CASE record.winner
        CASE BLACKWIN
            resulttext = "Black wins"
        CASE WHITEWIN
            resulttext = "White wins"
        CASE EQUAL
            resulttext = "Draw"
    END SELECT
   
    PRINT #fileNum, "  "; resulttext
    PRINT #fileNum, "  Black stones: "; LTRIM$(STR$(record.blackstones))
    PRINT #fileNum, "  White stones: "; LTRIM$(STR$(record.whitestones))
    PRINT #fileNum, "  Final score: "; LTRIM$(STR$(record.blackstones)); " - "; LTRIM$(STR$(record.whitestones))
    SELECT CASE record.endreason
        CASE "NORMAL_END"
            reasontext = "Normal end"
        CASE "TIME_OUT"
            reasontext = "Time out"
        CASE "RESIGN"
            reasontext = "Resignation"
    END SELECT
    PRINT #fileNum, "  End reason: "; reasontext
    PRINT #fileNum, "=============================="
    PRINT #fileNum, ""
   
    CLOSE fileNum
   
    LOCATE MESSAGE1ROW, 1
    PRINT SPACE$(MSGLINEWIDTH);
    LOCATE MESSAGE1ROW, 1
    PRINT "Game saved to "; RECORDFILENAME
END SUB

SUB StoneNumShow (board() AS INTEGER, stonenum() AS INTEGER)
    SHARED nn AS STRING
  
    DIM x AS INTEGER
    DIM y AS INTEGER
  
    stonenum(BLACKSIDE) = 0
    stonenum(WHITESIDE) = 0
    FOR x = 1 TO BOARDSIZE
        FOR y = 1 TO BOARDSIZE
            IF board(x, y) = BLACKSTONE THEN
                stonenum(BLACKSIDE) = stonenum(BLACKSIDE) + 1
            ELSEIF board(x, y) = WHITESTONE THEN
                stonenum(WHITESIDE) = stonenum(WHITESIDE) + 1
            END IF
        NEXT y
    NEXT x
    LOCATE BLACKSTONESROW, 8: PRINT USING nn; stonenum(BLACKSIDE);
    LOCATE WHITESTONESROW, 8: PRINT USING nn; stonenum(WHITESIDE);
END SUB

SUB StoneShow (stone AS INTEGER, x AS INTEGER, y AS INTEGER)
  
    CALL ReversiScreen(BOARDSCREEN, USE)
    IF stone = BLACKSTONE THEN
        CIRCLE (x - .5, y - .5), .4, BLACK
        PAINT (x - .5, y - .5), BLACK, BLACK
    ELSEIF stone = WHITESTONE THEN
        CIRCLE (x - .5, y - .5), .4, WHITE
        PAINT (x - .5, y - .5), WHITE, WHITE
    END IF

END SUB

SUB TurnStone (board() AS INTEGER, x AS INTEGER, y AS INTEGER)
    DIM stone AS INTEGER
    DIM xplus AS INTEGER
    DIM yplus AS INTEGER
    DIM xpointer AS INTEGER
    DIM ypointer AS INTEGER
    DIM i AS INTEGER
    DIM j AS INTEGER

    stone = board(x, y)
    FOR xplus = -1 TO 1
        FOR yplus = -1 TO 1
            xpointer = x + xplus
            ypointer = y + yplus
            DO WHILE (xpointer >= 1 AND xpointer <= BOARDSIZE) AND (ypointer >= 1 AND ypointer <= BOARDSIZE)
                IF board(xpointer, ypointer) = stone THEN
                    i = x + xplus
                    j = y + yplus
                    DO UNTIL i = xpointer AND j = ypointer
                        board(i, j) = stone
                        CALL StoneShow(board(i, j), i, j)
                        i = i + xplus
                        j = j + yplus
                    LOOP
                    EXIT DO
                ELSEIF board(xpointer, ypointer) = BLANK THEN
                    EXIT DO
                END IF
                xpointer = xpointer + xplus
                ypointer = ypointer + yplus
            LOOP
        NEXT yplus
    NEXT xplus
END SUB

SUB WinShow (winside AS INTEGER)
    IF winside = BLACKWIN THEN
        CALL FaceShow(BLACKSIDE, WIN)
        CALL FaceShow(WHITESIDE, LOSE)
    ELSEIF winside = WHITEWIN THEN
        CALL FaceShow(BLACKSIDE, LOSE)
        CALL FaceShow(WHITESIDE, WIN)
    ELSEIF winside = EQUAL THEN
        CALL FaceShow(BLACKSIDE, QUIET)
        CALL FaceShow(WHITESIDE, QUIET)
    END IF
END SUB

FUNCTION WinSideJudge (nowside AS INTEGER, notime AS INTEGER, accpetlose AS INTEGER, stonenum() AS INTEGER)
    IF nowside = OVER THEN
        IF stonenum(BLACKSIDE) > stonenum(WHITESIDE) THEN
            WinSideJudge = BLACKWIN
        ELSEIF stonenum(BLACKSIDE) = stonenum(WHITESIDE) THEN
            WinSideJudge = EQUAL
        ELSEIF stonenum(BLACKSIDE) < stonenum(WHITESIDE) THEN
            WinSideJudge = WHITEWIN
        END IF
    ELSEIF nowside <> OVER THEN
        IF notime = TRUE OR accpetlose = TRUE THEN
            IF nowside = BLACKSIDE THEN
                WinSideJudge = WHITEWIN
            ELSEIF nowside = WHITESIDE THEN
                WinSideJudge = BLACKWIN
            END IF
        END IF
    END IF
END FUNCTION