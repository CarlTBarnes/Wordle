!Wordle tool by Carl Barnes based on Mike Hanson's tool who did most of the work
!   . Different Scoring that is simpler. Not sure if its better. Change _Carl_Score to EQUATE(0) to use MH scoring
!   . Added List of Words with Unknown Letters HunterQ for 2nd guess to find more letters e.g. SOARE then UNTIL
!   . Possible list when zero letters found excludes words with Dup Letters because at first need to find letters
!   . 'Ltr' button shows letter scoring
!   . Buttons - "Run" another Instance, and "New" clears for new game
!   . Reduced icons to 48x48 that are 8 Kb vs 150 Kb 128x128 looked better but were larger than the word file
!---------------------------------------------------------------------
!Carl revised scoring
!   For any letters that are in position that would NOT affect score
![x] IMM on the STRING(1) Entry so as soon as key Letter it recalculates without pressing Tab
![x] If a Hunter word has doubles (letter repeat) it would be a poor Hunter word so exclude
![ ] Delete Key to remove words from List once I decide I do not want them (but a compute would put them back so would need DeletedQ) too complicated
![x] POPUP to config some settings, add '...' button with Popup
![x]    option for Possible to just show Wordle2300 words, would make it easy _Wordle2300_4_Possible_
![ ]    Last S scoring adjustment
![x] Checkboxes for Yellow letter seem hard to see. Maybe Yellow Box behind (Done), or Latched with Icon
![ ] LetterKeyClass.TakeEvent allow Right Click on Keyboard Popup so can do all data entry with mouse
![x] Double click on Word in LIST to copy to clipboard so can paste into my game guess entry
![x] UpLow letters so UPPER found letters
!    [ ] Kind of ugly. Maybe better if Color with styles, might be pretty. Simpler show word twice once UpLow

                              PROGRAM
_Carl_Score_            BYTE(1)         !1 = use Carl's scoring of Letters that is simple, (0) = Mike's
_Score_S_               LONG            !S Score so can deduct if 'S' in last position. MH idea that looks good
                                        !  Wordle  2316 Clue words have  2%   36 of  2316 words with last S, none plural e.g. Press
                                        !  Wordle 12972 All  words have 31% 3958 of 12972 words have last S
_Wordle2300_LoadOnly_   EQUATE(0)       !Wordle uses about 2316 words for Secret word. Limit Load to these makes really easy. Can also examine letter scoring.
_Wordle2300_4_LetterScore_ BYTE(0)      !Letter scoring uses wordle 2300 even thought all 13,000 words are loaded
_Wordle2300_4_Possible_ BYTE(0)         !Limit Possilbe to Wordle 2300 only, probably makes it too easy
_UpLow_Possible_        BYTE(0)         !In Possible word show found letters UPPER
_UpLow_Hunter_          BYTE(1)         !In Hunt words show found letters UPPER
_Hunt_Req1_FoundLetter_ BYTE(0)         !Hunt Count will be >= 1, some think its a cheat to try 5 new letters

  INCLUDE('Errors.clw'),ONCE
  INCLUDE('Keycodes.clw'),ONCE
  !INCLUDE('AnyScreen.inc'),ONCE        !Carl took out AS since it slows build/test, should work

!  INCLUDE('CBWndPreview.INC'),ONCE     !TODO remove before upload to Github
!G:WndPrvCls   CBWndPreviewClass        !Can use for QueueReflection

WORD_LENGTH                   EQUATE(5)

                              MAP
LoadWords                       PROCEDURE
CountDoubles                    PROCEDURE
ScoreWords                      PROCEDURE
UI                              PROCEDURE
QueueCopy                       PROCEDURE(QUEUE FromQ,QUEUE TOQ)
ViewQueue_LetterQ               PROCEDURE()
ViewQueue_WordQ                 PROCEDURE()
 !MH                               INCLUDE('STDebug.inc')  part of Super Stuff
                              END

WordQ                         QUEUE                     !Dictionary of 5-letter words, also used for  PossibleQ DoubleQ
Word                            STRING(WORD_LENGTH)     
Score                           LONG                    !Sort so words with popular letters first, words with J,Q,X are last
DupLetters                      BYTE                    !Count of Duplicate Letters so can omit word at times like when no letters are known
Secret2300                      BYTE                    !Is in Wordle 2300 secret words, used with _Wordle2300_4_Possible_
                              END

                              ITEMIZE(0)
LetterState:Unknown             EQUATE      !Silver - letters we have no idea if they are in the secret word
LetterState:Nowhere             EQUATE      !Gray   - letters that are NOT in the secret word
LetterState:Missed              EQUATE      !Yellow - letters in the word but position is wrong
LetterState:Correct             EQUATE      !Green  - letters in the correct positon
                              END

LetterKeyClass                CLASS,TYPE                    !Class to manage clickable letter keys
State                           BYTE                        !From LetterState:*
StringFEQ                       SIGNED                      !Letter key STRING
RegionFEQ                       SIGNED                      !Created REGION overlapping STRING
Construct                       PROCEDURE
InitControl                     PROCEDURE(SIGNED StringFEQ) !Initialize screen for letter key STRING
Display                         PROCEDURE                   !Adjust display for the current State
SetState                        PROCEDURE(BYTE State)       !Set a particular LetterState:*
TakeEvent                       PROCEDURE                   !Watch for user clicking on the REGION
                              END

LetterScoreWordCount          LONG                          !Usually Records(WordQ) but could be limited _Wordle2300_4_LetterScore_
LetterQ                       QUEUE                         !Queue of all 26 letters
Letter                          STRING(1)                   !Letter A,B,C...X,Y,Z
Score                           LONG            !@n_5       !Score of letter, could be MH or CB if _Carl_Score_=True
ScoreCB                         LONG            !@n_5       !Carl Letter Score is 20th % i.e. ScoreCB/TotalCB*10*2
TotalCB                         LONG            !@n_5       !Carl Letter Total Score, number of words with the letter, does NOT count double letters
ScoreMH                         LONG            !@n_5       !Total score of letter - Mike MH version, does count doubles - if _Carl_Score_
ScorePosGrp                     GROUP                       !Group of 5 x LONG for DIM 5 ,OVER
ScorePos1                          LONG         !@n_5       !Score of ScorePos[1], this works in LIST while DIM gets error 6
ScorePos2                          LONG         !@n_5
ScorePos3                          LONG         !@n_5
ScorePos4                          LONG         !@n_5
ScorePos5                          LONG         !@n_5
                               END
Handler                         &LetterKeyClass    !omit            !Instantiated Letter Class object
ScorePos                        LONG,DIM(5),OVER(ScorePosGrp)  !omit !Score of letter in each word character position (1-5)
                              END

Letters                       CLASS                                      !Class to manage all letters as a collection
Construct                       PROCEDURE
Destruct                        PROCEDURE
FetchLetter                     PROCEDURE(STRING Letter)                 !Utility to fetch LetterQ record
AccumScore                      PROCEDURE(STRING Letter,BYTE Pos,BOOL DupLetter)        !Add up Letter score during dictionary analysis
GetScore                        PROCEDURE(CONST *STRING Letter,BYTE Pos, CONST *STRING Word),LONG   !Get the score for a letter in specified position
InitControl                     PROCEDURE(SIGNED StringFEQ)              !Setup the control for a particular letter key
SetState                        PROCEDURE(STRING Letter,BYTE State)      !Set the state for a particular letter
Display                         PROCEDURE(STRING Correct,STRING Missed)  !Update the display for all letter keys
TakeEvent                       PROCEDURE                                !Handle letter key events (passing down as necessary)
FailsNowhere                    PROCEDURE(STRING Word),BOOL              !Does word contain any letters with LetterState:Nowhere?
ClearStates                     PROCEDURE()                              !Set All Letters to Unknown state for NEW Button
                              END
SettingsINI     EQUATE('.\Wordle_Tool_Settings.INI')
  CODE
  _UpLow_Possible_ = GETINI('Config','UpLow_Possible',_UpLow_Possible_,SettingsINI)
  _UpLow_Hunter_   = GETINI('Config','UpLow_Hunter'  ,_UpLow_Hunter_  ,SettingsINI)
  _Wordle2300_4_Possible_ = GETINI('Config','Wordle2300_4_Possible',_Wordle2300_4_Possible_,SettingsINI)
  _Hunt_Req1_FoundLetter_ = GETINI('Config','Hunt_Req1_FoundLetter',_Hunt_Req1_FoundLetter_,SettingsINI)
  SYSTEM{PROP:PropVScroll}=1                    !Thumb proportional gives hint size of Possibles list   Carl
  SYSTEM{7A7Dh}=MSGMODE:CANCOPY   !c11 PROP:MsgModeDefault
  LoadWords()
  !CountDoubles()
  ScoreWords()
  UI()
  PUTINI('Config','UpLow_Possible',_UpLow_Possible_,SettingsINI)
  PUTINI('Config','UpLow_Hunter',_UpLow_Hunter_,SettingsINI)
  PUTINI('Config','Wordle2300_4_Possible',_Wordle2300_4_Possible_,SettingsINI)
  PUTINI('Config','Hunt_Req1_FoundLetter',_Hunt_Req1_FoundLetter_,SettingsINI)
  RETURN
!==============================================================================
LoadWords                     PROCEDURE
                              MAP
LoadWord                        PROCEDURE(STRING Word, BYTE pSecret2300)
                              END
!WordsAlphaTxt                   EQUATE('words_alpha_len5.txt')  !Carl wrote smaller 5 letter word file with WriteWordsFileWith_5_Letters()
WordsFilename                   STRING(FILE:MaxFilePath),STATIC
WordsFile                       FILE,DRIVER('ASCII'),NAME(WordsFilename),PRE(WF)
                                  RECORD
Word                                STRING(30)
                                  END
                                END
IsWordle2300Word  BYTE(True)  !Worlde Secret words are limited to first 2316 before AAHED -----
  CODE
  WordsFilename = 'PlayWordle_Words.TXT'                    !12972 Words file from Worlde game
  OPEN(WordsFile, 40h)  !Carl was 22h) now ReadOnly DenyNone
  IF ERRORCODE() THEN                                        !Carl
     Message('Error on Open Words File: ' & WordsFilename &'||'& Error(),'Alert')
  END
  SET(WordsFile)
  LOOP
    NEXT(WordsFile)
    IF ERRORCODE() <> NoError THEN BREAK.
    IF WF:Word[1:5]='-----' THEN              !Secret words end before 'aahed' where I put a '-----'
       IF _Wordle2300_LoadOnly_ THEN BREAK.   !Only Secret words
       IsWordle2300Word = FALSE
       ! FREE(WordQ)                          !Free the 2300 so all BUT the 2300
       CYCLE
    END
    IF LEN(CLIP(WF:Word)) <> WORD_LENGTH THEN CYCLE.
    LoadWord(WF:Word,IsWordle2300Word)
  END
  CLOSE(WordsFile)
  IF ~RECORDS(WordQ) THEN                                    !Carl
     Message('Found Zero Words: ' & WordsFilename ,'Alert')
  END

  WordQ.Word = 'SOARE'
  GET(WordQ, WordQ.Word)
  IF ERRORCODE() <> NoError
    LoadWord('SOARE',False)
  END
  ! G:WndPrvCls.QueueReflection(WordQ,'WordQ')
  RETURN
!--------------------------------------
LoadWord                      PROCEDURE(STRING Word, BYTE pSecret2300)
N BYTE,AUTO
  CODE
  CLEAR(WordQ)
  WordQ.Word = UPPER(Word)
  WordQ.Secret2300 = pSecret2300
  CASE WordQ.Word[1]          !My Wordle words file can have Comments with ; or - in [1]
  OF 'A' TO 'Z'
     LOOP N = 2 TO WORD_LENGTH
          IF INSTRING(Word[N], Word[1:N-1]) THEN
             WordQ.DupLetters += 1
          END
     END
     ADD(WordQ)
  END
  RETURN
!==============================================================================
CountDoubles                  PROCEDURE
W                               LONG,AUTO
C                               BYTE,AUTO
D                               BYTE,AUTO
DoubleQ                         WordQ
  CODE
W LOOP W = 1 TO RECORDS(WordQ)
    GET(WordQ, W)
    LOOP C = 1 TO WORD_LENGTH-1
      LOOP D = C+1 TO WORD_LENGTH
        IF WordQ.Word[D] = WordQ.Word[C]
          DoubleQ = WordQ
          ADD(DoubleQ)
          CYCLE W
        END
      END
    END
  END
  ! ST::DebugQueue(DoubleQ)

!==============================================================================
ScoreWords                    PROCEDURE
                              MAP
ScoreWord                       PROCEDURE(STRING Word),LONG
                              END
W       LONG,AUTO
C       BYTE,AUTO
IsDupLetter BOOL,AUTO   !Carl scoring Dup Letters do NOT affect Letter score so ASSES counts S Once
  CODE
  LetterScoreWordCount = 0
  LOOP W = RECORDS(WordQ) TO 1 BY -1
    GET(WordQ, W)
!   IF ~INSTRING('A',WordQ.Word,1) THEN DELETE(WordQ) ; CYCLE.   !Test only words with 'A' certain letter
    IF _Wordle2300_4_LetterScore_ AND ~WordQ.Secret2300 THEN CYCLE.
    LetterScoreWordCount += 1
    LOOP C = 1 TO WORD_LENGTH
       IF C>1 AND INSTRING(WordQ.Word[C], WordQ.Word[1:C-1]) THEN
          IsDupLetter = 1
       ELSE
          IsDupLetter = 0
       END
       Letters.AccumScore(WordQ.Word[C], C, IsDupLetter)
    END
  END
  !--- Carl wants to Score simpler ------------------
!  WordCnt=RECORDS(WordQ)
  LOOP C = 1 TO Records(LetterQ)
      GET(LetterQ,C)
!      LetterQ.ScoreMH = LetterQ.Score
      LetterQ.ScoreCB = ROUND( LetterQ.TotalCB / LetterScoreWordCount * 10 * 2,1)    !Not 100% but Tenths ... or 20ths
      IF _Carl_Score_ THEN
         LetterQ.Score = LetterQ.ScoreCB
      ELSE
         LetterQ.Score = LetterQ.ScoreMH
      END
      PUT(LetterQ)
      IF LetterQ.Letter = 'S' THEN
         _Score_S_ = CHOOSE(_Carl_Score_=True, LetterQ.ScoreCB, LetterQ.ScorePos[5])
      END
  END
  !--- Carl wants to Score simpler ------------------
  SORT(LetterQ, -LetterQ.Score, -LetterQ.Letter)

  LOOP W = 1 TO RECORDS(WordQ)
    GET(WordQ, W)
    WordQ.Score = ScoreWord(WordQ.Word)
    PUT(WordQ)
  END
  SORT(WordQ, -WordQ.Score, WordQ.Word)
  ! G:WndPrvCls.QueueReflection(WordQ,'WordQ',1)     ! ST::DebugQueue(WordQ, 'Word Scores')
  RETURN
!--------------------------------------
ScoreWord                     PROCEDURE(STRING Word)!,LONG
N                               BYTE
Score                           LONG(0)
  CODE
  LOOP N = 1 TO WORD_LENGTH
    !Carl thought this NOT scoring Dup letters was bad idea, but Nope without this words like ASSES score highest
    !     the goal is for Possible to suggest words to find letters so less repeated letters helps
    !IF ~_Carl_Score_ THEN                                       !Only do this for MH Scoring
       IF N > 1 AND INSTRING(Word[N], Word[1:N-1]) THEN CYCLE.  !Already scored this letter in this word?
    !END
    Score += Letters.GetScore(Word[N], N,Word)
  END
  RETURN Score

!==============================================================================
UI                            PROCEDURE
                              MAP
InitWindow                      PROCEDURE
NewGame                         PROCEDURE
Compute                         PROCEDURE
ConfigPopup                     PROCEDURE
                              END

CorrectGroup                    GROUP,PRE
Correct1                          STRING(1)
Correct2                          STRING(1)
Correct3                          STRING(1)
Correct4                          STRING(1)
Correct5                          STRING(1)
                                END
Correct                         STRING(WORD_LENGTH),OVER(CorrectGroup)

!Idea: Make Missing like Correct. Change MissedGroup.Letter a &STRING &= Missed1.
!             Will CLEAR() work? This way do NOT need to Concat Missing each Accept

Missing                         STRING(WORD_LENGTH)   !Concat Missed1.Letter 2,3 etc
MissedArrayType                 GROUP,TYPE
Letter                            STRING(1)
Pos                               BYTE,DIM(WORD_LENGTH)
                                END
MissedGroup                     GROUP,TYPE
Letter                            STRING(1)
Pos1                              BYTE
Pos2                              BYTE
Pos3                              BYTE
Pos4                              BYTE
Pos5                              BYTE
                                END
Missed1                         LIKE(MissedGroup),PRE(Missed1)
Missed2                         LIKE(MissedGroup),PRE(Missed2)
Missed3                         LIKE(MissedGroup),PRE(Missed3)
Missed4                         LIKE(MissedGroup),PRE(Missed4)
Missed5                         LIKE(MissedGroup),PRE(Missed5)

PossibleQ                       QUEUE
WordShow                            STRING(WORD_LENGTH)     !Word may have UPR letters that are Correct or Missed ... color?
WordTip                             STRING(WORD_LENGTH)     !UpLow seems ugly so ToolTip shows word in UPPER
Score                               LONG                    !Sort so words with popular letters first, words with J,Q,X are last
Word                                STRING(WORD_LENGTH)
                                END

HunterQ                         QUEUE                       !Hunt Words with Letters that have Unknown status
WordShow                            STRING(WORD_LENGTH)     !May have Found UPPER if _UpLow_Hunter_=True
WordTip                             STRING(WORD_LENGTH)     !UpLow seems ugly so ToolTip shows word in UPPER
Score                               LONG
FoundLetterCount                    BYTE    !Count of Letters (Correct + Missed) to sort Last after words with no Found letters
Word                                STRING(WORD_LENGTH)     !Always Lower Case for Sort
                                END

Window WINDOW('Wordle Tool'),AT(,,268,142),CENTER,GRAY,SYSTEM,ICON('WordleTool48.ico'),FONT('Segoe UI',14,COLOR:White), |
            COLOR(COLOR:Black),RESIZE
        PROMPT('&Correct:'),AT(4,4),USE(?Correct:Prompt),TRN
        ENTRY(@s1),AT(48,4,11),USE(Correct1),IMM,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
        ENTRY(@s1),AT(61,4,11),USE(Correct2),IMM,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
        ENTRY(@s1),AT(74,4,11),USE(Correct3),IMM,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
        ENTRY(@s1),AT(87,4,11),USE(Correct4),IMM,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
        ENTRY(@s1),AT(100,4,11),USE(Correct5),IMM,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
        STRING('1       2        3       4       5'),AT(52,18),USE(?Poz12345),TRN,FONT(,10)
        STRING('1       2        3       4       5'),AT(52,95),USE(?Poz12345_2),TRN,HIDE,FONT(,10)
        PROMPT('&Missed:'),AT(4,24),USE(?Missed1:Prompt),TRN
        BUTTON('New'),AT(4,44,18,12),USE(?NewGameBtn),SKIP,FONT(,10),TIP('Clear all letters to begin new game'),TRN
        BUTTON('Run'),AT(4,61,18,12),USE(?RunAgainBtn),SKIP,FONT(,10),TIP('Run another instance of this tool'),TRN
        BUTTON('Ltr'),AT(4,78,18,12),USE(?Debug:LetterQ),SKIP,FONT(,10),TIP('View Letter Scoring'),TRN
        ENTRY(@s1),AT(33,24,11),USE(Missed1:Letter),IMM,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(03B9FB4H),UPR
        CHECK,AT(51,25,8),USE(Missed1:Pos1),TRN
        CHECK,AT(64,25,8),USE(Missed1:Pos2),TRN
        CHECK,AT(77,25,8),USE(Missed1:Pos3),TRN
        CHECK,AT(90,25,8),USE(Missed1:Pos4),TRN
        CHECK,AT(103,25,8),USE(Missed1:Pos5),TRN
        ENTRY(@s1),AT(33,39,11),USE(Missed2:Letter),IMM,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(03B9FB4H),UPR
        CHECK,AT(51,40,8),USE(Missed2:Pos1),TRN
        CHECK,AT(64,40,8),USE(Missed2:Pos2),TRN
        CHECK,AT(77,40,8),USE(Missed2:Pos3),TRN
        CHECK,AT(90,40,8),USE(Missed2:Pos4),TRN
        CHECK,AT(103,40,8),USE(Missed2:Pos5),TRN
        ENTRY(@s1),AT(33,54,11),USE(Missed3:Letter),IMM,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(03B9FB4H),UPR
        CHECK,AT(51,55,8),USE(Missed3:Pos1),TRN
        CHECK,AT(64,55,8),USE(Missed3:Pos2),TRN
        CHECK,AT(77,55,8),USE(Missed3:Pos3),TRN
        CHECK,AT(90,55,8),USE(Missed3:Pos4),TRN
        CHECK,AT(103,55,8),USE(Missed3:Pos5),TRN
        ENTRY(@s1),AT(33,69,11),USE(Missed4:Letter),IMM,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(03B9FB4H),UPR
        CHECK,AT(51,70,8),USE(Missed4:Pos1),TRN
        CHECK,AT(64,70,8),USE(Missed4:Pos2),TRN
        CHECK,AT(77,70,8),USE(Missed4:Pos3),TRN
        CHECK,AT(90,70,8),USE(Missed4:Pos4),TRN
        CHECK,AT(103,70,8),USE(Missed4:Pos5),TRN
        ENTRY(@s1),AT(33,84,11),USE(Missed5:Letter),IMM,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(03B9FB4H),UPR
        CHECK,AT(51,85,8),USE(Missed5:Pos1),TRN
        CHECK,AT(64,85,8),USE(Missed5:Pos2),TRN
        CHECK,AT(77,85,8),USE(Missed5:Pos3),TRN
        CHECK,AT(90,85,8),USE(Missed5:Pos4),TRN
        CHECK,AT(103,85,8),USE(Missed5:Pos5),TRN
        BOX,AT(48,26,64,8),USE(?BoxMiss1),FILL(03B9FB4H),LINEWIDTH(1)
        BOX,AT(48,41,64,8),USE(?BoxMiss2),FILL(03B9FB4H),LINEWIDTH(1)
        BOX,AT(48,56,64,8),USE(?BoxMiss3),FILL(03B9FB4H),LINEWIDTH(1)
        BOX,AT(48,71,64,8),USE(?BoxMiss4),FILL(03B9FB4H),LINEWIDTH(1)
        BOX,AT(48,86,64,8),USE(?BoxMiss5),FILL(03B9FB4H),LINEWIDTH(1)
        STRING('Q'),AT(4,104,10,10),USE(?Letter:Q),CENTER,COLOR(COLOR:Gray)
        STRING('W'),AT(16,104,10,10),USE(?Letter:W),CENTER,COLOR(COLOR:Gray)
        STRING('E'),AT(28,104,10,10),USE(?Letter:E),CENTER,COLOR(COLOR:Gray)
        STRING('R'),AT(40,104,10,10),USE(?Letter:R),CENTER,COLOR(COLOR:Gray)
        STRING('T'),AT(52,104,10,10),USE(?Letter:T),CENTER,COLOR(COLOR:Gray)
        STRING('Y'),AT(64,104,10,10),USE(?Letter:Y),CENTER,COLOR(COLOR:Gray)
        STRING('U'),AT(76,104,10,10),USE(?Letter:U),CENTER,COLOR(COLOR:Gray)
        STRING('I'),AT(88,104,10,10),USE(?Letter:I),CENTER,COLOR(COLOR:Gray)
        STRING('O'),AT(100,104,10,10),USE(?Letter:O),CENTER,COLOR(COLOR:Gray)
        STRING('P'),AT(112,104,10,10),USE(?Letter:P),CENTER,COLOR(COLOR:Gray)
        STRING('A'),AT(10,116,10,10),USE(?Letter:A),CENTER,COLOR(COLOR:Gray)
        STRING('S'),AT(22,116,10,10),USE(?Letter:S),CENTER,COLOR(COLOR:Gray)
        STRING('D'),AT(34,116,10,10),USE(?Letter:D),CENTER,COLOR(COLOR:Gray)
        STRING('F'),AT(46,116,10,10),USE(?Letter:F),CENTER,COLOR(COLOR:Gray)
        STRING('G'),AT(58,116,10,10),USE(?Letter:G),CENTER,COLOR(COLOR:Gray)
        STRING('H'),AT(70,116,10,10),USE(?Letter:H),CENTER,COLOR(COLOR:Gray)
        STRING('J'),AT(82,116,10,10),USE(?Letter:J),CENTER,COLOR(COLOR:Gray)
        STRING('K'),AT(94,116,10,10),USE(?Letter:K),CENTER,COLOR(COLOR:Gray)
        STRING('L'),AT(106,116,10,10),USE(?Letter:L),CENTER,COLOR(COLOR:Gray)
        STRING('Z'),AT(22,128,10,10),USE(?Letter:Z),CENTER,COLOR(COLOR:Gray)
        STRING('X'),AT(34,128,10,10),USE(?Letter:X),CENTER,COLOR(COLOR:Gray)
        STRING('C'),AT(46,128,10,10),USE(?Letter:C),CENTER,COLOR(COLOR:Gray)
        STRING('V'),AT(58,128,10,10),USE(?Letter:V),CENTER,COLOR(COLOR:Gray)
        STRING('B'),AT(70,128,10,10),USE(?Letter:B),CENTER,COLOR(COLOR:Gray)
        STRING('N'),AT(82,128,10,10),USE(?Letter:N),CENTER,COLOR(COLOR:Gray)
        STRING('M'),AT(94,128,10,10),USE(?Letter:M),CENTER,COLOR(COLOR:Gray)
        STRING('Possible Words'),AT(127,3),USE(?Head_PossibleQ),FONT(,12)
        BUTTON('...'),AT(177,3,11,8),USE(?ConfigPopupBtn),SKIP,TIP('Configure Settings ...'),TRN
        LIST,AT(127,13,62,126),USE(?PossibleList),FLAT,NOBAR,VSCROLL,FONT('Consolas'),TIP('Possible Words and Score'),VCR, |
                FROM(PossibleQ),FORMAT('24L(2)|MP@S5@24L(2)|M@s6@')
        STRING('New Letter Words'),AT(196,3),USE(?Head_HunterQ),FONT(,12)
        CHECK('1 '),AT(253,3),USE(_Hunt_Req1_FoundLetter_),SKIP,FONT(,12),TIP('Words contain at least 1 letter found so ' & |
                '4 new letters.<13,10>Some consider it cheating to guess words with 5 new letters.<13,10,13,10>Ideally y' & |
                'ou want the missed letters in a new position.')
        LIST,AT(196,13,68,126),USE(?HunterList),FLAT,NOBAR,VSCROLL,FONT('Consolas'),TIP('Hunter -- Words with only new l' & |
                'etters words and score<13,10>Third column shows count of found letters.'),VCR,FROM(HunterQ), |
                FORMAT('24L(2)|MP@S5@24L(2)|M@s6@Q''Letter Score''15L(2)|M@n1b@Q''Count of Letters Correct + Missed''')
    END

  CODE
  OPEN(Window)
  InitWindow()
  ACCEPT
    Letters.TakeEvent()
    Missing=CLIP(Missed1.Letter) &     |
            CLIP(Missed2.Letter) &     |
            CLIP(Missed3.Letter) &     |
            CLIP(Missed4.Letter) &     |
            CLIP(Missed5.Letter)
    CASE EVENT()
    OF EVENT:OpenWindow
      Compute()
    OF EVENT:NewSelection                   !Carl added IMM to ENTRY so as Letter typed can instantly update without tabbing
       IF ?{PROP:Type}=Create:Entry THEN        !AND LEN(CLIP(?{PROP:ScreenText})) =1 THEN
          UPDATE
          POST(EVENT:Accepted,?)
          CYCLE
       ELSIF KEYCODE()=MouseLeft2                                                         !Double click on List
          CASE FIELD()                       
          OF ?PossibleList
              GET(PossibleQ,CHOICE(?PossibleList)) ; SETCLIPBOARD(UPPER(PossibleQ.Word))  !So can paste into my game
          OF ?HunterList
              GET(HunterQ,CHOICE(?HunterList)) ; SETCLIPBOARD(UPPER(HunterQ.Word))  !So can paste into my game
          END
       END
    OF EVENT:Accepted
       CASE ACCEPTED()
       OF ?RunAgainBtn    ; RUN(COMMAND('0'))         ; CYCLE
       OF ?Debug:LetterQ  ; START(ViewQueue_LetterQ ) ; CYCLE
       OF ?NewGameBtn     ; NewGame()
       OF ?ConfigPopupBtn ; ConfigPopup()
       END
       Letters.Display(Correct, Missing)
       Compute()                 !Carl moved down after Letters.Display
    END
  END
  RETURN
!--------------------------------------
InitWindow                    PROCEDURE
StringFEQ                       SIGNED,AUTO
  CODE
  LOOP StringFEQ = ?Letter:Q TO ?Letter:M
    Letters.InitControl(StringFEQ)
  END
  0{PROP:MinHeight} = 0{PROP:Height} / 2
  0{PROP:MaxHeight} = 0{PROP:Height}
  0{PROP:MinWidth}  = ?HunterList{PROP:XPos}-2
  0{PROP:MaxWidth}  = 0{PROP:Width}
!--------------------------------------
NewGame PROCEDURE()
    CODE
    CLEAR(Correct)
    CLEAR(Missing)
    CLEAR(Missed1)
    CLEAR(Missed2)
    CLEAR(Missed3)
    CLEAR(Missed4)
    CLEAR(Missed5)
    Letters.ClearStates()
    DISPLAY
    RETURN
!--------------------------------------
Compute                       PROCEDURE
                              MAP
FailsCorrect                    PROCEDURE,BOOL
FailsMissed                     PROCEDURE(*MissedGroup Missed),BOOL
HuntScan                        PROCEDURE()
HuntFailsMissed                 PROCEDURE(*MissedGroup Missed),BOOL
AddLetterString                 PROCEDURE(*PSTRING LetterList, STRING ALetter)
                              END
W   LONG,AUTO
WC  BYTE,AUTO
LettersFound        PSTRING(WORD_LENGTH * 3 + 1)    !Correct and Missed
LettersFoundLower   LIKE(LettersFound)
LettersNoWhere      PSTRING(27)
HaveLettersFound    BOOL                            !Are there Correct or Missing
  CODE
  IF Correct OR Missing THEN HaveLettersFound=True.
  LettersFoundLower=LOWER(CLIP(Correct) & CLIP(Missing))
  FREE(PossibleQ)
  IF ~Missed1.Letter THEN CLEAR(Missed1).   !If Letter blank then clear CheckBox BYTEs
  IF ~Missed2.Letter THEN CLEAR(Missed2).   !TODO better way like a Queue of &Missed refs
  IF ~Missed3.Letter THEN CLEAR(Missed3).   !     so can loop them all and only do this when needed
  IF ~Missed4.Letter THEN CLEAR(Missed4).
  IF ~Missed5.Letter THEN CLEAR(Missed5).
  LOOP W = 1 TO RECORDS(WordQ)
    GET(WordQ, W)

    IF ~HaveLettersFound AND WordQ.DupLetters THEN CYCLE.           !With Zero Letters omit word with Dups as wasted guess
    IF _Wordle2300_4_Possible_ AND ~WordQ.Secret2300 THEN CYCLE.    !Just Secret 2300 in List

    IF FailsCorrect() THEN CYCLE.

    IF FailsMissed(Missed1) THEN CYCLE.
    IF FailsMissed(Missed2) THEN CYCLE.
    IF FailsMissed(Missed3) THEN CYCLE.
    IF FailsMissed(Missed4) THEN CYCLE.
    IF FailsMissed(Missed5) THEN CYCLE.

    IF Letters.FailsNowhere(WordQ.Word) THEN CYCLE.

    PossibleQ.Word     = WordQ.Word             !This will be Upper
    PossibleQ.WordShow = LOWER(WordQ.Word)
    PossibleQ.WordTip  = ''
    PossibleQ.Score    = WordQ.Score
    IF _UpLow_Possible_ THEN            !Show Found UPPER to help understand... UGLY? 
       LOOP WC=1 TO WORD_LENGTH
            IF INSTRING(PossibleQ.WordShow[WC],LettersFoundLower,1) THEN 
               PossibleQ.WordShow[WC] = UPPER(PossibleQ.WordShow[WC])
               PossibleQ.WordTip = WordQ.Word             !This will be Upper
            END
       END
    END
    ADD(PossibleQ)
  END
  DISPLAY(?PossibleList)

  FREE(HunterQ)
  HuntScan()
  DISPLAY()

  ?Head_PossibleQ{PROP:Tip}='Possible words containing letters found, <13,10>but not gray letters no where in secret word.' & |
                            '<13,10,13,10>'& RECORDS(PossibleQ) &' possible words from '& RECORDS(WordQ) &' total words.'
  ?Head_HunterQ{PROP:Tip}  ='Words containing letters not found <13,10>and not gray letters no where in secret word.' & |
                            '<13,10>These words can be used to find new letters.' & |
                            '<13,10,13,10>'& RECORDS(HunterQ) &' words from '& RECORDS(WordQ) &' total words.'
  RETURN
!--------------------------------------
HuntScan PROCEDURE()
X   LONG,AUTO
L   BYTE,AUTO
FoundInWord BYTE,AUTO
LettersFoundCorrect LIKE(LettersFound),AUTO
HaveMissingLetters  BOOL
UpLowWord STRING(WORD_LENGTH),AUTO  !May have Found Letters in UPPER if _UpLow_Hunter_=True
    CODE
    LettersFound=''
    LettersNoWhere=''
    LOOP X=1 TO SIZE(Correct)
         AddLetterString(LettersFound,Correct[X])
    END
    LettersFoundCorrect = LettersFound
    LOOP X=1 TO SIZE(Missing)
         AddLetterString(LettersFound,Missing[X])
    END
    IF Missing AND LettersFound <> LettersFoundCorrect THEN
       HaveMissingLetters= True
    END

    LOOP L = 1 TO RECORDS(LetterQ)
       GET(LetterQ, L)
       IF LetterQ.Handler.State = LetterState:Nowhere
          AddLetterString(LettersNoWhere,LetterQ.Letter)
       END
    END
!    0{PROP:Text}='LettersFound=' & LettersFound &'  LettersNoWhere='& LettersNoWhere
    IF ~LettersFound AND ~LettersNoWhere THEN RETURN.  !If all letters are good there's no point

WL: LOOP W = 1 TO RECORDS(WordQ)
      GET(WordQ, W)
      IF WordQ.DupLetters THEN CYCLE.   !Letters repeat in Word so not a good Hunt for new letters word

      UpLowWord=LOWER(WordQ.Word)
      FoundInWord=0
      LOOP X=1 TO WORD_LENGTH
           IF INSTRING(WordQ.Word[X],LettersNoWhere) THEN
              CYCLE WL:
           END
           IF INSTRING(WordQ.Word[X],LettersFound) THEN
              FoundInWord += 1
              IF _UpLow_Hunter_ THEN 
                 UpLowWord[X] = UPPER(UpLowWord[X])
              END
           END
!Use Word.DupLetters
!           IF X>1 AND INSTRING(WordQ.Word[X],WordQ.Word[1 : X-1]) !Letter repeats in Word so not a good Hunter word
!              CYCLE WL:
!           END
      END
      IF FoundInWord > 2 THEN   !If we have 3 found letters then
         CYCLE WL:
      ELSIF FoundInWord = 0 AND _Hunt_Req1_FoundLetter_ THEN
         CYCLE WL:
      END

      !If the Word has Missed Letters in place then toss the word for Hunt it tells us nothing new
      IF HaveMissingLetters AND FoundInWord THEN
         IF HuntFailsMissed(Missed1) THEN CYCLE WL: .
         IF HuntFailsMissed(Missed2) THEN CYCLE WL: .
         IF HuntFailsMissed(Missed3) THEN CYCLE WL: .
         IF HuntFailsMissed(Missed4) THEN CYCLE WL: .
         IF HuntFailsMissed(Missed5) THEN CYCLE WL: .
      END

      HunterQ.Word     = LOWER(WordQ.Word)
      HunterQ.WordShow = UpLowWord
      HunterQ.WordTip  = CHOOSE(UpLowWord=HunterQ.Word,'',WordQ.Word)  !Tip with Upper Word if Mixed Letters
      HunterQ.Score    = WordQ.Score
      HunterQ.FoundLetterCount = FoundInWord
      ADD(HunterQ)
    END
    SORT(HunterQ,HunterQ.FoundLetterCount,-HunterQ.Score,HunterQ.Word)
    RETURN
!--------------------------------------
AddLetterString PROCEDURE(*PSTRING LetterList, STRING ALetter)
    CODE
    IF ALetter AND ~INSTRING(ALetter,LetterList,1) THEN
       LetterList = LetterList & ALetter
    END
    RETURN
!--------------------------------------
FailsCorrect                  PROCEDURE!,BOOL
X                               LONG
  CODE
  IF Correct <> ''
    LOOP X = 1 TO WORD_LENGTH
      CASE Correct[X]
      OF '' OROF WordQ.Word[X]
        !No-op
      ELSE
        RETURN TRUE
      END
    END
  END
  RETURN FALSE
!--------------------------------------
FailsMissed                   PROCEDURE(*MissedGroup Missed)!,BOOL
M   LIKE(MissedArrayType),PRE(M),OVER(Missed)  !Has Pos[] Array for Pos1 ... Pos5
C   BYTE,AUTO
  CODE
  IF Missed.Letter <> '' THEN
    !The letter needs to be somewhere in the word
    IF NOT INSTRING(Missed.Letter, WordQ.Word)
       RETURN TRUE
    END

    !But not in a position were it's specified not to be
    LOOP C = 1 TO WORD_LENGTH
      IF M.Pos[C] AND WordQ.Word[C] = M.Letter THEN RETURN TRUE.
    END
  END
  RETURN FALSE
!--------------------------------------
HuntFailsMissed    PROCEDURE(*MissedGroup Missed)!,BOOL
M   LIKE(MissedArrayType),PRE(M),OVER(Missed)
C   BYTE,AUTO
  CODE
  IF Missed.Letter <> '' THEN
    !Not in a position were it's specified not to be
    LOOP C = 1 TO WORD_LENGTH
      IF M.Pos[C] AND WordQ.Word[C] = M.Letter THEN RETURN TRUE.
    END
  END
  RETURN FALSE
!--------------------------------------
ConfigPopup PROCEDURE()
PopNo BYTE,AUTO
    CODE
    PopNo=POPUP('~Configure by Checking Items' & |  !#1            
                '|-' & |
                '|'& CHOOSE(~_UpLow_Possible_,'-','+') &'Possible Words - UPPER Found Letters' & |  !#2
                '|'& CHOOSE(~_UpLow_Hunter_,'-','+') &'New Letter Words - UPPER Found Letters' & |  !#3
                '|-' & |
                '|'& CHOOSE(~_Wordle2300_4_Possible_,'-','+') &'Limit to Wordle 2316 Words (too easy)' & |  !#4
                '')
     
    CASE PopNo
    OF  1                                           ! # 1  Configure by Checking Items
    OF  2 ; _UpLow_Possible_ = 1-_UpLow_Possible_   ! # 2  Possible Words - UPPER Found Letters
    OF  3 ; _UpLow_Hunter_   = 1-_UpLow_Hunter_     ! # 3  New Letter Words - UPPER Found Letters
    OF  4 ; _Wordle2300_4_Possible_ = 1-_Wordle2300_4_Possible_    ! # 4  Limit to Wordle 2316 Words (too easy)
    END !CASE Popup
    RETURN 
!==============================================================================
LetterKeyClass.Construct      PROCEDURE
  CODE
  SELF.State = LetterState:Unknown

!==============================================================================
LetterKeyClass.InitControl    PROCEDURE(SIGNED StringFEQ)
X                               LONG,AUTO
Y                               LONG,AUTO
W                               LONG,AUTO
H                               LONG,AUTO
  CODE
  SELF.StringFEQ = StringFEQ
  SELF.RegionFEQ = CREATE(0, CREATE:region)
  GETPOSITION(SELF.StringFEQ, X, Y, W, H)
  SETPOSITION(SELF.RegionFEQ, X, Y, W, H)
  UNHIDE(SELF.RegionFEQ)
  SELF.Display()

!==============================================================================
LetterKeyClass.Display        PROCEDURE
                              MAP
SetColor                        PROCEDURE(LONG Background,LONG Foreground=COLOR:White)
                              END
  CODE
  ASSERT(SELF.StringFEQ <> 0, 'LetterKeyClass.Display StringFEQ=0')
  CASE SELF.State
    ;OF LetterState:Unknown;  SetColor(8684417                 )  !Gray
    ;OF LetterState:Nowhere;  SetColor(3947066, 00E0E0E0h      )  !Dark Gray  c11 COLOR:LightGray
    ;OF LetterState:Missed ;  SetColor(3907508                 )  !Yellow
    ;OF LetterState:Correct;  SetColor(5147986                 )  !Green
  END

  SELF.RegionFEQ{PROP:Cursor} = CHOOSE(~INLIST(SELF.State, LetterState:Missed, LetterState:Correct), |
      CURSOR:Hand, CURSOR:None)

!--------------------------------------
SetColor                      PROCEDURE(LONG Background,LONG Foreground)
  CODE
  SELF.StringFEQ{PROP:Background} = Background
  SELF.StringFEQ{PROP:FontColor } = Foreground

!==============================================================================
LetterKeyClass.SetState       PROCEDURE(BYTE State)
  CODE
  SELF.State = State
  SELF.Display()

!==============================================================================
!Allow toggling the letter key between Nowhere and Unknown.
!If it's Missing or Correct, then beep and leave as is.
LetterKeyClass.TakeEvent      PROCEDURE
  CODE
  ASSERT(SELF.RegionFEQ <> 0, 'LetterKeyClass.TakeEvent RegionFEQ=0')

  IF ACCEPTED() = SELF.RegionFEQ
    IF KEYCODE() = MouseRight AND SELF.State <= LetterState:Nowhere THEN  !TODO allow Right-Click
       DO PopupRtn
       RETURN
    END
    CASE SELF.State
      OF LetterState:Unknown ;  SELF.SetState(LetterState:Nowhere)
      OF LetterState:Nowhere ;  SELF.SetState(LetterState:Unknown)
    ELSE                     ;  BEEP
    END
  END

PopupRtn ROUTINE
!TODO Popup - code may be messy and complicated
!           - goal to do all data entry thru keyboard buttons (regions) at bottom with mouse seems nice
    p#=POPUP('Todo Idea|-|Unknown|'& LetterQ.Letter &' No Where' & |
             '|Missed  {{1|2|3|4|5}' & |
             '|Correct {{1|2|3|4|5}' & |
             '')
!Colors do not word well, maybe make a simple Icon of Yellow, Green, Gray Box, but the Checkbox cannot be used
!             '|['& PROP:FillColor &'('& 03B9FB4H &']Missed  {{1|2|3|4|5}' & |
!             '|['& PROP:FillColor &'('& 04E8D52H &']Correct {{1|2|3|4|5}' & |
!             '')

!             '|[' & PROP:FontColor &'(' & COLOR:White &'),'& PROP:FillColor &'(' & COLOR:Green &']Correct {{1|2|3|4|5}')
             ![31762(16777215),31994(32768)]
!             '|Missed {{1|2|3|4|5}|Correct {{1|2|3|4|5}')
!Could color these
!==============================================================================
Letters.Construct             PROCEDURE
                              MAP
AddLetter                       PROCEDURE(STRING Letter)
                              END
C                               BYTE,AUTO
  CODE
  LOOP C = VAL('A') TO VAL('Z')
    AddLetter(CHR(C))
  END

!--------------------------------------
AddLetter                     PROCEDURE(STRING Letter)
  CODE
  CLEAR(LetterQ)
  LetterQ.Letter   = Letter
  LetterQ.Handler &= NEW LetterKeyClass
  ADD(LetterQ, LetterQ.Letter)

!==============================================================================
Letters.Destruct              PROCEDURE
  CODE
  LOOP WHILE RECORDS(LetterQ)
    GET(LetterQ, 1)
    DISPOSE(LetterQ.Handler)
    DELETE(LetterQ)
  END

!==============================================================================
Letters.FetchLetter           PROCEDURE(STRING Letter)
  CODE
  LetterQ.Letter = Letter
  GET(LetterQ, LetterQ.Letter)
  ASSERT(ERRORCODE()=NoError, 'Missing Letter "'& Letter &'" in call to Letters.FetchLetter')

!==============================================================================
Letters.AccumScore            PROCEDURE(STRING Letter,BYTE Pos,BOOL DupLetter)
  CODE
  SELF.FetchLetter(Letter)
!TODO this should accum into ScoreMH and ScoreCB fields
  IF ~DupLetter THEN  !Do not count mutiple
     LetterQ.TotalCB    += 1
  END
  LetterQ.ScoreMH       += 1
  LetterQ.ScorePos[Pos] += 1
  PUT(LetterQ)

!==============================================================================
!Letters.GetScore              PROCEDURE(STRING Letter,BYTE Pos)!,LONG
!  CODE
!  SELF.FetchLetter(Letter)
!  RETURN LetterQ.Score + CHOOSE(Letter='S' AND Pos=WORD_LENGTH, 0, LetterQ.ScorePos[Pos])  !If 'S' is last it does not get position points

Letters.GetScore              PROCEDURE(CONST *STRING Letter,BYTE Pos, CONST *STRING Word)!,LONG
Score LONG,AUTO
  CODE
  SELF.FetchLetter(Letter)
  Score = LetterQ.Score
!TODO rethink this code. Now that I don't score Dup Letters the 2nd 'S' of PRESS will never get here
!     is the idea the Vowel+S should be ignored
  IF Pos=WORD_LENGTH AND Letter='S'    |        !Letter is last S
  AND ~INSTRING(Word[Pos-1],'SAIOU')  THEN      !Previous Letter is Not S (e.g. PRESS) or not Vowel except E (e.g. VIRUS)
      Score -= _Score_S_                        !Probably Plural so last 'S' gets no score added
  END
  RETURN Score
!   It gives priority to the 2300 list knowing if a word ends in 'S' it is not usually a Plural... I think
!==============================================================================
Letters.InitControl           PROCEDURE(SIGNED StringFEQ)
  CODE
  SELF.FetchLetter(StringFEQ{PROP:Text})
  LetterQ.Handler.InitControl(StringFEQ)

!==============================================================================
Letters.SetState              PROCEDURE(STRING Letter,BYTE State)
  CODE
  SELF.FetchLetter(Letter)
  LetterQ.Handler.SetState(State)

!==============================================================================
Letters.ClearStates    PROCEDURE()
L BYTE,AUTO
  CODE
  LOOP L = 1 TO RECORDS(LetterQ)
    GET(LetterQ, L)
    LetterQ.Handler.SetState(LetterState:Unknown)
  END
!==============================================================================
Letters.Display               PROCEDURE(STRING Correct,STRING Missed)
L                               BYTE,AUTO
  CODE
  LOOP L = 1 TO RECORDS(LetterQ)
    GET(LetterQ, L)
    IF INSTRING(LetterQ.Letter, Correct)
      LetterQ.Handler.SetState(LetterState:Correct)
    ELSIF INSTRING(LetterQ.Letter, Missed)
      LetterQ.Handler.SetState(LetterState:Missed)
    ELSIF LetterQ.Handler.State <> LetterState:Nowhere
      LetterQ.Handler.SetState(LetterState:Unknown)
    END
  END

!==============================================================================
Letters.TakeEvent             PROCEDURE
L                               BYTE,AUTO
  CODE
  LOOP L = 1 TO RECORDS(LetterQ)
    GET(LetterQ, L)
    LetterQ.Handler.TakeEvent()
  END

!==============================================================================
Letters.FailsNowhere          PROCEDURE(STRING Word)!,BOOL
L                               BYTE,AUTO
  CODE
  LOOP L = 1 TO RECORDS(LetterQ)
    GET(LetterQ, L)
    IF LetterQ.Handler.State = LetterState:Nowhere
      IF INSTRING(LetterQ.Letter, Word)
        RETURN TRUE
      END
    END
  END
  RETURN FALSE
!================================================================
QueueCopy PROCEDURE(QUEUE FromQ,QUEUE TOQ)
R LONG,AUTO
    CODE
    LOOP R=1 TO RECORDS(FromQ)
        GET(FromQ,R)
        ToQ=FromQ
        ADD(ToQ)
    END
    RETURN
!================================================================
ViewQueue_LetterQ PROCEDURE()
!FYI Much of this procedure generated by "List Format Parser"
!    using the "Queue to Format" tab the "Window + List" button - https://github.com/CarlTBarnes/List-Format-Parser
LtrsAbcQ    LetterQ
Ltrs123Q    LetterQ
LX          BYTE,AUTO
ListWindow WINDOW('LetterQ'),AT(,,313,251),GRAY,AUTO,SYSTEM,ICON('WordleTool48.ico'),FONT('Segoe UI',10)
        LIST,AT(4,3,252,226),USE(?LIST:LtrsAbcQ),VSCROLL,FROM(LtrsAbcQ),FORMAT('20C|M~By<13,10>Ltr~@s1@25R(2)|M~<13,10>S' & |
                'core~C(0)@n_6@25C|M~CB<13,10>Score~@n2@25R(2)|M~CB<13,10>Total~C(0)@n7@25R(2)|M~MH<13,10>Score~C(0)@n7@' & |
                '[25R(2)|M~- 1 -~C(0)@n7@#7#25R(2)|M~- 2 -~C(0)@n7@25R(2)|M~- 3 -~C(0)@n7@25R(2)|M~- 4 -~C(0)@n7@25R(2)|' & |
                'M~- 5 -~C(0)@n7@]~MH Letter Position Score~')
        LIST,AT(262,3,45,226),USE(?LIST:Ltrs123Q),VSCROLL,FROM(Ltrs123Q),FORMAT('25R(2)|M~By<13,10>Score~C(0)@n_6@#2#20C' & |
                '(0)~<13,10>Ltr~C(0)@s1@#1#')
        BUTTON('Cl&ose'),AT(25,233,37),USE(?CloseBtn),STD(STD:Close)
        BUTTON('&Copy'),AT(79,233,37),USE(?CopyBtn),KEY(CtrlC),TIP('Copy LetterQ to clipboard tab delimited to paste int' & |
                'o Excel')
        BUTTON('&About'),AT(133,233,37),USE(?AboutBtn)
        BUTTON('&WordQ'),AT(187,233,37),USE(?WordQBtn),TIP('View WordQ in List')
        CHECK('Score 2300'),AT(261,232),USE(_Wordle2300_4_LetterScore_),SKIP,FONT(,8),TIP('Check to recalc scores based ' & |
                'on just 2300 secret words<13,10>New idea, not sure this works right.')
        CHECK('CB Score'),AT(261,240),USE(_Carl_Score_),SKIP,FONT(,8),TIP('UnCheck to recalc scores used by tool based o' & |
                'n MH scoring.')
    END
Fmt123Q ANY
FmtAbcQ ANY
    CODE
    OPEN(ListWindow)
    Fmt123Q=?LIST:Ltrs123Q{PROP:Format}
    FmtAbcQ=?LIST:LtrsAbcQ{PROP:Format}
    DO LoadQRtn
    ?LIST:Ltrs123Q{PROP:NoTheme}=1
    ?LIST:LtrsAbcQ{PROP:NoTheme}=1
    ACCEPT
      CASE ACCEPTED()
      OF ?CopyBtn  ; DO Copy2ClipRtn
      OF ?AboutBtn ; DO AboutRtn
      OF ?WordQBtn ; START(ViewQueue_WordQ)
      OF ?_Carl_Score_ 
      OROF ?_Wordle2300_4_LetterScore_ ; DO ReScoreRtn
      END
      CASE FIELD()
      OF ?LIST:LtrsAbcQ
         GET(LtrsAbcQ,CHOICE(?LIST:LtrsAbcQ))
         CASE EVENT()
         OF EVENT:NewSelection
            ! CASE KEYCODE()
            ! OF MouseLeft2
            ! OF MouseRight
            ! END
         END
      END
    END !ACCEPT
    CLOSE(ListWindow)

LoadQRtn ROUTINE
    FREE(Ltrs123Q) ; FREE(LtrsAbcQ)
    QueueCopy(LetterQ,Ltrs123Q)
    QueueCopy(LetterQ,LtrsAbcQ)
    SORT(LtrsAbcQ, LtrsAbcQ.Letter)
    SORT(Ltrs123Q,-Ltrs123Q.Score,Ltrs123Q.Letter)
    0{PROP:Text} = 'LetterQ  Last S='& _Score_S_ &', Based on '& LetterScoreWordCount &' of '& RECORDS(WordQ) & ' Words'
    IF _Carl_Score_ THEN
      ?LIST:Ltrs123Q{PROPLIST:Picture,1}='@n2'  ; ?LIST:LtrsAbcQ{PROPLIST:Picture,2}='@n2'
      ?LIST:Ltrs123Q{PROPLIST:CenterOffset,1}=0 ; ?LIST:LtrsAbcQ{PROPLIST:CenterOffset,2}=0
      ?LIST:Ltrs123Q{PROPLIST:Center,1}=1       ; ?LIST:LtrsAbcQ{PROPLIST:Center,2}=1
    ELSE
      ?LIST:Ltrs123Q{PROP:Format} = Fmt123Q
      ?LIST:LtrsAbcQ{PROP:Format} = FmtAbcQ
    END
ReScoreRtn ROUTINE
    LOOP LX=1 TO RECORDS(LetterQ)
        GET(LetterQ,LX)
        Ltrs123Q = LetterQ                      !Save the current LetterQ to 123Q
        CLEAR(LetterQ)                          !So can clear the whole shebang and not worry about fields
        LetterQ.Letter   = Ltrs123Q.Letter      !then restore the 2 that are needed
        LetterQ.Handler &= Ltrs123Q.Handler
        PUT(LetterQ)
    END
    ScoreWords()
    DO LoadQRtn
    DISPLAY
    POST(EVENT:Accepted,,1)
    EXIT
AboutRtn ROUTINE
    Message('"CB Total" is total words that contain the Letter. Duplicate letters do NOT count.' & |
            '||"CB Score" is Ratio of Words containing letter X 20 so ends up 0 to 10.' & |
            '||CB Word Score is sum of letter scores not including duplicate letters.' & |
            '|||"MH Letter Position Score" is the word count that contains the Letter in the 5 positions.' & |
            '||"MH Score" is total if the 5 letter position scores. Duplicate letters DO count so different than CB.' & |
            '||MH Letter Position Score can be interesting e.g. comparing first guess word SOARE to AROSE.' & |
            '|I prefer AROSE as a common word, but "S" is in position [1] of 366 words vs only 141 for "A" in [1].' & |
            '|The 366/141 are for the 2,300 words, for the 18,000 words "S" / "A" are 5936/5330 so the same.' & |
            '||This letter scoring is based on all 13,000 words while Wordle uses 2,300 so has different totals.' & |
            '|Check the "Score on 2300" box to score on just the 2300 words.' & |
            '','Letter Scores','~WordleTool48.ico')
Copy2ClipRtn ROUTINE
    DATA
LtrsCB  ANY
WordCnt LONG
X LONG
    CODE
    WordCnt=LetterScoreWordCount   !RECORDS(WordQ)
    LtrsCB = 'Letter<9>Score<9>CB Score<9>CB Total<9>%Words CB<9>MH Score<9>Pos 1 <9>Pos 2 <9>Pos 3 <9>Pos 4 <9>Pos 5 <9>%Words MH<9><9>Words=' & WordCnt
    LOOP X = 1 TO Records(LtrsAbcQ)
       GET(LtrsAbcQ,X)
       LtrsCB = LtrsCB & LEFT('<13,10>' & |
                     LtrsAbcQ.Letter & |
             '<9>' & LtrsAbcQ.Score & |
             '<9>' & LtrsAbcQ.ScoreCB  & |
             '<9>' & LtrsAbcQ.TotalCB  & |
             '<9>' & ROUND( LtrsAbcQ.TotalCB / WordCnt * 100,1)  & |
             '<9>' & LtrsAbcQ.ScoreMH  & |
             '<9>' & LtrsAbcQ.ScorePos[1] & |
             '<9>' & LtrsAbcQ.ScorePos[2] & |
             '<9>' & LtrsAbcQ.ScorePos[3] & |
             '<9>' & LtrsAbcQ.ScorePos[4] & |
             '<9>' & LtrsAbcQ.ScorePos[5] & |
             '<9>' & ROUND( LtrsAbcQ.ScoreMH / WordCnt * 100,1)  & |
             '')
    END
!  LtrsCB = LtrsCB & '<13,10><13,10,9,9>The Score is the number of words with this letter'
    SETCLIPBOARD(LtrsCB)
    Message('LetterQ on clipboard tab delimitted for Excel.','Wordle Tool')
    EXIT
!================================================================
ViewQueue_WordQ PROCEDURE()
!FYI Much of this procedure generated by "List Format Parser"
!    using the "Queue to Format" tab the "Window + List" button - https://github.com/CarlTBarnes/List-Format-Parser 

QNdx        LONG
SortHow     BYTE
FilterDups  BYTE
Filter2300  BYTE
WordZ       QUEUE(WordQ)
Ltr1            STRING(1)
Ltr5            STRING(1)
            END
SelectWord  STRING(WORD_LENGTH),AUTO
       
ListWindow WINDOW('WordQ'),AT(,,148,228),GRAY,SYSTEM,ICON('WordleTool48.ico'),STATUS,FONT('Segoe UI',10),RESIZE
        LIST,AT(4,3,56,10),USE(SortHow),SKIP,FONT(,8),DROP(9),FROM('Sort by Score|#0|Sort by Word|#1|Sort by Dups|#2|Ltr' & |
                ' 1 + Score|#3|Ltr 5 + Score|#4'),FORMAT('20L(2)')
        LIST,AT(63,3,38,10),USE(FilterDups),SKIP,FONT(,8),TIP('Filter list to show only words with/without Duplicate Letters'), |
                DROP(9,54),FROM('All Dups Yes+No|#0|Yes Dup Letters|#1|No Dup Letters|#2'),FORMAT('20L(2)')
        LIST,AT(104,3,38,10),USE(Filter2300),SKIP,FONT(,8),TIP('Show only (or not) the 2300 words<13,10>used for Wordle ' & |
                'game secret words'),DROP(9,60),FROM('All 13k Words|#0|2300 Game Words|#1|Not 2300 Words|#2'),FORMAT('20L(2)')
        LIST,AT(4,16,138),FULL,USE(?LIST:WordZ),VSCROLL,VCR,FROM(WordZ),FORMAT('36L(2)|FM~Word~C(0)@s5@?30R(2)|FM~Score~' & |
                'C(0)@n_5@30C|M~Dup <13,10>Letters~@n1b@30C|M~Wordle<13,10>2300~@n1b@')
    END
    
    CODE
    OPEN(ListWindow)
    0{PROP:MinWidth}  = 0{PROP:Width} ; 0{PROP:MinHeight} = 0{PROP:Height} / 3
    0{PROP:MaxWidth}  = 0{PROP:Width}
    ACCEPT 
      CASE EVENT()
      OF EVENT:OpenWindow   ; DO LoadWordZRtn
      END
      CASE ACCEPTED()
      OF   ?FilterDups 
      OROF ?Filter2300
            DO Select1Rtn
            DO LoadWordZRtn
            DO Select2Rtn
      OF ?SortHow
            DO Select1Rtn
            DO SortRtn 
            DO Select2Rtn
      END
    END !ACCEPT
    CLOSE(ListWindow)

Select1Rtn ROUTINE
    GET(WordZ,CHOICE(?LIST:WordZ)) 
    SelectWord = CHOOSE(~ERRORCODE(),WordZ.Word,'')
Select2Rtn ROUTINE
    WordZ.Word = SelectWord 
    GET(WordZ,WordZ.Word)
    SELECT(?LIST:WordZ,CHOOSE(~ERRORCODE(),POINTER(WordZ),1))
    EXIT
    
LoadWordZRtn ROUTINE
    FREE(WordZ)
    LOOP QNdx=1 TO RECORDS(WordQ)
       GET(WordQ,QNdx)
       CASE FilterDups
       OF 1 ; IF WordQ.DupLetters = 0 THEN CYCLE.
       OF 2 ; IF WordQ.DupLetters > 0 THEN CYCLE.
       END
       CASE Filter2300
       OF 1 ; IF WordQ.Secret2300 = 0 THEN CYCLE.
       OF 2 ; IF WordQ.Secret2300 > 0 THEN CYCLE.
       END       
       WordZ = WordQ
       WordZ.Ltr1 = WordQ.Word[1]
       WordZ.Ltr5 = WordQ.Word[5]
       ADD(WordZ)
    END
    0{PROP:StatusText}='Showing '& RECORDS(WordZ) &' of '& RECORDS(WordQ) & ' words'
    IF SortHow > 0 THEN DO SortRtn.
    DISPLAY
    EXIT

SortRtn ROUTINE
    CASE SortHow
    OF 0 ; SORT(WordZ,-WordZ.Score,WordZ.Word)
    OF 1 ; SORT(WordZ,WordZ.Word)
    OF 2 ; SORT(WordZ,-WordZ.DupLetters,WordZ.Word)
    OF 3 ; SORT(WordZ,WordZ.Ltr1,-WordZ.Score,WordZ.Word)   !Nailing the 1st Letter helps a lot
    OF 4 ; SORT(WordZ,WordZ.Ltr5,-WordZ.Score,WordZ.Word)   !1st + Last makes it easy to guess them middle 3
    END
    EXIT