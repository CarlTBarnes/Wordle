!Play Wordle by Carl Barnes - February 2022
!Based on Mike Hanson's Wordle Solver that had MOST of this code done, especially the cosmetics. Thanks Mike!
!Working on Solver this helps to be able to play a game, and replay, and know the secret answer to see if the suggestions are logical.
!File PlayWordle_Words.TXT has official Wordle words. First 2316 are used for game secret words.
!--------------------------------------------------------------------------------------
![ ] Could make the Keyboard at the bottom work like the game, gives visual clue letter status ??
![ ] Hard Mode - Any revealed hints must be used in subsequent guesses
!--------------------------------------------------------------------------------------
                              PROGRAM
  INCLUDE('Keycodes.clw'),ONCE
  INCLUDE('Errors.clw'),ONCE
  !INCLUDE('AnyScreen.inc'),ONCE    !This can work but I see it as slowing my build / test process

!  INCLUDE('CBWndPreview.INC'),ONCE
!G:WndPrvCls   CBWndPreviewClass

WORD_LENGTH                   EQUATE(5)

                              MAP
LoadWords                       PROCEDURE
UI                              PROCEDURE
RandomWord                      PROCEDURE(),STRING
QueueCopy                       PROCEDURE(QUEUE FromQ,QUEUE TOQ)
                              END

WordQ                         QUEUE                         !Dictionary of 5-letter words
Word                            STRING(WORD_LENGTH)
                              END
WordQ_RecordsForPuzzle        LONG   !First 2300 are for Puzzle secret word, ends at '-----' in PlayWordle_Words.TXT

                              ITEMIZE(0)
LetterState:Unknown             EQUATE
LetterState:Nowhere             EQUATE
LetterState:Missed              EQUATE
LetterState:Correct             EQUATE
                              END

LetterKeyClass                CLASS,TYPE                    !Class to manage clickable letter keys
State                           BYTE                        !From LetterState:*
StringFEQ                       SIGNED                      !Letter key STRING
! R egionFEQ                       SIGNED                    !Created REGION for Keyboard lette, restore to make keys work
Construct                       PROCEDURE
InitControl                     PROCEDURE(SIGNED StringFEQ) !Initialize screen for letter key STRING
Display                         PROCEDURE                   !Adjust display for the current State
SetState                        PROCEDURE(BYTE State)       !Set a particular LetterState:*
                              END

LetterQ                       QUEUE                         !Queue of all 26 letters
Letter                          STRING(1)                   !Letter
Handler                         &LetterKeyClass                !Instantiated object
                              END

Letters                       CLASS                                      !Class to manage all letters as a collection
Construct                       PROCEDURE
Destruct                        PROCEDURE
FetchLetter                     PROCEDURE(STRING Letter)                 !Utility to fetch LetterQ record
InitControl                     PROCEDURE(SIGNED StringFEQ)              !Setup the control for a particular letter key
SetState                        PROCEDURE(STRING Letter,BYTE State)      !Set the state for a particular letter
DisplayLetters                  PROCEDURE(STRING Correct,STRING Missed,STRING NoWhere) !Update the display for all letter keys
FailsNowhere                    PROCEDURE(STRING Word),BOOL              !Does word contain any letters with LetterState:Nowhere?
                              END
SettingsINI     EQUATE('.\Play_Wordle_Settings.INI')
  CODE
  SYSTEM{PROP:PropVScroll}=1                    !Thumb proportional gives hint size of Possibles list   Carl
  SYSTEM{7A7Dh}=MSGMODE:CANCOPY   !c11 PROP:MsgModeDefault
  LoadWords()
  LOOP TODAY() % 100 + CLOCK() % 200 TIMES ; IF RANDOM(1,WordQ_RecordsForPuzzle). ; END  !Pump randoms
  UI()

!==============================================================================
UI          PROCEDURE
                MAP
InitWindow          PROCEDURE
Compute             PROCEDURE
NewGameInit         PROCEDURE
PopupGuess          PROCEDURE
TakeGuess           PROCEDURE
TakeGuessOK         PROCEDURE
TakeSecret          PROCEDURE
TakeSecretUnHide    PROCEDURE
DeleteGuessHistory  PROCEDURE
               END

CorrectGroup                    GROUP,PRE
Correct1                          STRING(1)
Correct2                          STRING(1)
Correct3                          STRING(1)
Correct4                          STRING(1)
Correct5                          STRING(1)
                                END
Correct                         STRING(WORD_LENGTH),OVER(CorrectGroup)
MissedLetters                   STRING(21)      !Letters not in position
NoWhereLetters                  STRING(21)      !Letters not in word

UnHideSecret    BYTE
SecretWord      STRING(WORD_LENGTH)
SecretBefore    STRING(WORD_LENGTH)
GuessWord       STRING(WORD_LENGTH)

HistoryQ    QUEUE,PRE(HisQ)
Number         BYTE
Letter1        STRING(1)
Style1         LONG
Letter2        STRING(1)
Style2         LONG
Letter3        STRING(1)
Style3         LONG
Letter4        STRING(1)
Style4         LONG
Letter5        STRING(1)
Style5         LONG
GuessWord        STRING(5)
        END
HistoryQ_Over GROUP,PRE(),OVER(HistoryQ)
Number_Ovr     BYTE
HisQ:Array  GROUP,PRE(),DIM(5)
Letter             STRING(1)
Style              LONG
                END
              END

Window WINDOW('Wordle Game'),AT(,,287,146),CENTER,GRAY,SYSTEM,ICON('PlayWordle48.ico'),FONT('Segoe UI',14,COLOR:White), |
            COLOR(COLOR:Black),RESIZE
        PROMPT('Secret:'),AT(4,4),USE(?Secrt:Prompt),TRN
        CHECK('Sho&w / Change'),AT(80,6),USE(UnHideSecret),SKIP,TRN,FONT(,10),TIP('Show the Secret word<13,10>and allow ' & |
                'changing the word')
        ENTRY(@s5),AT(35,4,38),USE(SecretWord),SKIP,FLAT,FONT(,,,FONT:bold),TIP('Secret Word'),UPR,PASSWORD,READONLY, |
                ALRT(EnterKey)
        PROMPT('&Guess:'),AT(4,20),USE(?Guess:Prompt),TRN
        ENTRY(@s5),AT(35,20,38),USE(GuessWord),FLAT,FONT(,,COLOR:Navy,FONT:bold),COLOR(COLOR:White,COLOR:White,COLOR:Navy), |
                TIP('Most Vowels: ARISE AROSE SOARE ADIEU<13,10>Fewest Guess: SLICE TRIED CRANE CLOSE TRAIN SLATE TRACE ' & |
                '<13,10>Always Solve: ADEPT CLAMP PLAID SCALP CLASP DEPOT'),UPR,ALRT(EnterKey)
        BUTTON('&Enter'),AT(80,20,29,12),USE(?GuessBtn),FONT(,12),TIP('Process Guess'),TRN
        BUTTON('...'),AT(114,20,11,12),USE(?PopupGuessBtn),FONT(,12),TIP('Popup of suggest first guess words like SOARE'),TRN
        GROUP,AT(2,36,123,48),USE(?GroupLettersCMN)
            PROMPT('&Correct:'),AT(4,39),USE(?Correct:Prompt),TRN
            GROUP,AT(35,38,63,13),USE(?GroupCorrect)
                ENTRY(@s1),AT(35,38,11),USE(Correct1),SKIP,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
                ENTRY(@s1),AT(48,38,11),USE(Correct2),SKIP,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
                ENTRY(@s1),AT(61,38,11),USE(Correct3),SKIP,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
                ENTRY(@s1),AT(74,38,11),USE(Correct4),SKIP,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
                ENTRY(@s1),AT(87,38,11),USE(Correct5),SKIP,FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
            END
            PROMPT('&Missed:'),AT(4,56),USE(?Missed:Prompt),TRN
            ENTRY(@s21),AT(35,54,63),USE(MissedLetters),SKIP,FLAT,FONT(,,,FONT:bold),COLOR(03B9FB4H),TIP('Letters guesse' & |
                    'd but in the wrong position aka Missed'),UPR,READONLY
            PROMPT('&Failed:'),AT(4,71),USE(?NoWhereLetters:Prompt),TRN
            ENTRY(@s21),AT(35,70,87,12),USE(NoWhereLetters),SKIP,FLAT,FONT(,,,FONT:bold),COLOR(03C3A3AH),TIP('Letters gu' & |
                    'essed but NOT in the Secret Word<13,10>THse letters show as dark grey in the puzzle'),UPR,READONLY
        END
        GROUP,AT(2,88,123,38),USE(?GroupQWERTY)
            STRING('Q'),AT(4,90,10,10),USE(?Letter:Q),CENTER,COLOR(COLOR:Gray)
            STRING('W'),AT(16,90,10,10),USE(?Letter:W),CENTER,COLOR(COLOR:Gray)
            STRING('E'),AT(28,90,10,10),USE(?Letter:E),CENTER,COLOR(COLOR:Gray)
            STRING('R'),AT(40,90,10,10),USE(?Letter:R),CENTER,COLOR(COLOR:Gray)
            STRING('T'),AT(52,90,10,10),USE(?Letter:T),CENTER,COLOR(COLOR:Gray)
            STRING('Y'),AT(64,90,10,10),USE(?Letter:Y),CENTER,COLOR(COLOR:Gray)
            STRING('U'),AT(76,90,10,10),USE(?Letter:U),CENTER,COLOR(COLOR:Gray)
            STRING('I'),AT(88,90,10,10),USE(?Letter:I),CENTER,COLOR(COLOR:Gray)
            STRING('O'),AT(100,90,10,10),USE(?Letter:O),CENTER,COLOR(COLOR:Gray)
            STRING('P'),AT(112,90,10,10),USE(?Letter:P),CENTER,COLOR(COLOR:Gray)
            STRING('A'),AT(10,102,10,10),USE(?Letter:A),CENTER,COLOR(COLOR:Gray)
            STRING('S'),AT(22,102,10,10),USE(?Letter:S),CENTER,COLOR(COLOR:Gray)
            STRING('D'),AT(34,102,10,10),USE(?Letter:D),CENTER,COLOR(COLOR:Gray)
            STRING('F'),AT(46,102,10,10),USE(?Letter:F),CENTER,COLOR(COLOR:Gray)
            STRING('G'),AT(58,102,10,10),USE(?Letter:G),CENTER,COLOR(COLOR:Gray)
            STRING('H'),AT(70,102,10,10),USE(?Letter:H),CENTER,COLOR(COLOR:Gray)
            STRING('J'),AT(82,102,10,10),USE(?Letter:J),CENTER,COLOR(COLOR:Gray)
            STRING('K'),AT(94,102,10,10),USE(?Letter:K),CENTER,COLOR(COLOR:Gray)
            STRING('L'),AT(106,102,10,10),USE(?Letter:L),CENTER,COLOR(COLOR:Gray)
            STRING('Z'),AT(22,114,10,10),USE(?Letter:Z),CENTER,COLOR(COLOR:Gray)
            STRING('X'),AT(34,114,10,10),USE(?Letter:X),CENTER,COLOR(COLOR:Gray)
            STRING('C'),AT(46,114,10,10),USE(?Letter:C),CENTER,COLOR(COLOR:Gray)
            STRING('V'),AT(58,114,10,10),USE(?Letter:V),CENTER,COLOR(COLOR:Gray)
            STRING('B'),AT(70,114,10,10),USE(?Letter:B),CENTER,COLOR(COLOR:Gray)
            STRING('N'),AT(82,114,10,10),USE(?Letter:N),CENTER,COLOR(COLOR:Gray)
            STRING('M'),AT(94,114,10,10),USE(?Letter:M),CENTER,COLOR(COLOR:Gray)
        END
        BUTTON('&New Game'),AT(4,129,37,12),USE(?NewGameBtn),SKIP,FONT(,10),TIP('New Game'),TRN
        BUTTON('&Run Again'),AT(45,129,36,12),USE(?RunAgainBtn),SKIP,FONT(,10),TIP('Run another instance of Play Worlde'),TRN
        BUTTON('Run &Tool'),AT(86,129,36,12),USE(?RunSolverBtn),SKIP,FONT(,10),TIP('Open Worlde Solver Tool'),TRN
        LIST,AT(131,4,104,137),USE(?HistoryList),FLAT,NOBAR,VSCROLL,TIP('Guess History<13,10>Press Delete key to remove guess'), |
                FROM(HistoryQ),FORMAT('14C(0)|~#~C(0)@s2@10C|Y~1~@s1@10C|Y~2~@s1@10C|Y~3~@s1@10C|Y~4~@s1@10C|Y~5~@s1@10L' & |
                '(2)~Guess~@s5@'),ALRT(DeleteKey)
        LIST,AT(241,4,40,137),USE(?WordsList),FLAT,NOBAR,VSCROLL,TIP('Puzzle WordQ<13,10>2300 puzzle words end at ----' & |
                '<13,10>Double Click to selected Guess word'),FROM(WordQ),FORMAT('24L(2)|M@S5@')
    END
!WndPrvCls   CBWndPreviewClass
  CODE
  OPEN(Window)
!  WndPrvCls.Init()
  InitWindow()
  UnHideSecret = GETINI('Config','UnHideSecret',UnHideSecret,SettingsINI)
  IF UnHideSecret THEN
     SecretWord = 'THOSE'
     GuessWord  = 'SOARE' !'ARISE'
     TakeSecretUnHide()
  ELSE
    SecretWord = RandomWord()
    GuessWord = 'SOARE'  !'ARISE'
  END
  ACCEPT
    CASE SELECTED()
    OF ?SecretWord     ; SecretBefore = SecretWord
    END
    CASE ACCEPTED()
    !OF ?GuessWord      ; TakeGuess()
    OF ?GuessBtn       ; TakeGuess()
    OF ?PopupGuessBtn  ; PopupGuess()
    OF ?SecretWord     ; TakeSecret()
    OF ?UnHideSecret   ; TakeSecretUnHide()
                         PUTINI('Config','UnHideSecret',UnHideSecret,SettingsINI)
    OF ?NewGameBtn     ; NewGameInit()
    OF ?RunAgainBtn    ; RUN(COMMAND('0'))
    OF ?RunSolverBtn   ; RUN('WordleTool.exe') ; IF ERRORCODE() THEN Message('Run WordleTool.exe failed|'& ERROR(),'Solver').
    OF ?WordsList
       IF KEYCODE() = MouseLeft2 THEN
          GET(WordQ,CHOICE(?WordsList))
          GuessWord = WordQ.Word
          POST(EVENT:Accepted,?GuessWord)
       END
    END
    CASE EVENT()
    OF EVENT:OpenWindow
    OF EVENT:Accepted
      Compute()
!      Letters.DisplayLetters(Correct, MissedLetters, NoWhereLetters)
    OF EVENT:AlertKey
       CASE KEYCODE()
       OF EnterKey
          CASE FIELD()
          OF ?GuessWord     ; UPDATE ; POST(EVENT:Accepted,?GuessBtn)
          OF ?SecretWord    ; UPDATE ; POST(EVENT:Accepted,?SecretWord)
          END
       OF DeleteKey
          CASE FIELD()
          OF ?HistoryList   ; DeleteGuessHistory()
          END
       END
    END

  END
!-------------------------------------------
NewGameInit PROCEDURE()
    CODE
    CLEAR(Correct)
    CLEAR(MissedLetters)
    CLEAR(NoWhereLetters)
    FREE(HistoryQ)
    SecretWord = RandomWord()
    ?SecretWord{PROP:Password}=1-UnHideSecret
    RETURN
!-------------------------------------------
DeleteGuessHistory PROCEDURE()
GuessQ  QUEUE(HistoryQ),PRE(GueQ)
        END
X       USHORT,AUTO
SecretNow  LIKE(SecretWord)
GuessNow   LIKE(GuessWord)
    CODE
    GET(HistoryQ,CHOICE(?HistoryList))
    IF ERRORCODE() THEN RETURN.
    DELETE(HistoryQ)
    !Easy way seems like reprocess all the History
    QueueCopy(HistoryQ,GuessQ)
    SecretNow = SecretWord
    GuessNow  = GuessWord
    NewGameInit()
    SecretWord = SecretNow
    DISPLAY
    LOOP X=1 TO RECORDS(GuessQ)
        GET(GuessQ,X)
        GuessWord = GueQ:GuessWord
        TakeGuessOK()
    END
    GuessWord = GuessNow
    Compute()
    RETURN
!-------------------------------------------
PopupGuess PROCEDURE()
PopNo BYTE,AUTO
!Words from https://entertainment.howstuffworks.com/leisure/online-games/strategies-for-winning-wordle.htm
PopWords  STRING(           'ADIEU|ARIEL|ARISE|AROSE|IRATE|SOARE}' & |
                            'SLICE|TRIED|CRANE|CLOSE|TRAIN|SLATE|TRACE}' & |
                            'ADEPT|CLAMP|PLAID|SCALP|CLASP|DEPOT}') 
    CODE
    PopNo=POPUP('' & |      ! 1     2     3     4     5     6     7
        'Most Vowels{{'&    'ADIEU|ARIEL|ARISE|AROSE|IRATE|SOARE}' & |
        'Fewest Guesses{{'& 'SLICE|TRIED|CRANE|CLOSE|TRAIN|SLATE|TRACE}' & |
        'Always Solve{{'&   'ADEPT|CLAMP|PLAID|SCALP|CLASP|DEPOT}')
    IF PopNo THEN         
       GuessWord=SUB(PopWords,1+(PopNo-1)*6,5)
       DISPLAY
       SELECT(?GuessWord)
    END
    RETURN
!-------------------------------------------
TakeGuess PROCEDURE()
C BYTE
    CODE
    GuessWord=UPPER(GuessWord)
    IF LEN(CLIP(GuessWord)) <> WORD_LENGTH THEN
      SELECT(?GuessWord)
      Message('Please enter a '& WORD_LENGTH &' letter word.','Alert')
      RETURN
    END

    !--- GET HistoryQ and if found then reject a repeat guess
    HisQ:GuessWord = GuessWord
    GET(HistoryQ,HisQ:GuessWord)
    IF ~ERRORCODE() THEN
        IF GuessWord <> HisQ:GuessWord THEN  !Ignore last guess
           Message('"' & GuessWord &'" was guess #' & POINTER(HistoryQ),'Alert')
        END
        RETURN
    END
    !--- Check WordQ if not a listed word (done by game), then can skip checking letters
    WordQ.Word = GuessWord
    GET(WordQ,WordQ.Word)
    IF ERRORCODE() THEN
       CASE Message('"' & GuessWord &'" is not a valid word in the dictionary.',|
                     'Alert',ICON:Asterisk,'Change|Ignore')
       OF 1 ; SELECT(?GuessWord) ; RETURN
       END
    END
    !--- Check is all letters to be safe
    LOOP C=1 TO WORD_LENGTH
         CASE GuessWord[C]
         OF 'A' TO 'Z'
         ELSE
            SELECT(?GuessWord)
            Message('"'& GuessWord[C] &'" in the '& C & CHOOSE(C,'st','nd','rd','th') &' position is not a letter','Alert')
            RETURN
         END
    END
    TakeGuessOK()
    RETURN
!------------------------------------------
TakeGuessOK PROCEDURE()
!FYI: Double letters - Only show yellow if they exist
!       https://nerdschalk.com/wordle-same-letter-twice-rules-explained-how-does-it-work/
!       https://www.elitedaily.com/news/does-wordle-use-repeat-letters-same-letter-twice-plural-past-tense
MissedAlready BYTE
CLetter STRING(1)
C BYTE
LetterState BYTE
ScanGuess   LIKE(GuessWord),AUTO    !Only find letters once
ScanSecret  LIKE(SecretWord),AUTO   !Only find letters once
ScanCorrect LIKE(SecretWord),AUTO   !Found Correct this time
PosMissed   BYTE,AUTO
    CODE
    CLEAR(HistoryQ)
    HisQ:Number = RECORDS(HistoryQ) + 1
    HisQ:GuessWord = GuessWord
    ScanGuess = GuessWord
    ScanSecret = SecretWord
    ScanCorrect = ALL('-')
    LOOP C=1 TO WORD_LENGTH                         !1st find Correct Letters and Blank so not found again
        CLetter = ScanGuess[C]
        IF CLetter <> SecretWord[C] THEN CYCLE.     !Letter CORRECT Position ?
        ScanCorrect[C]  = CLetter                   !Remember [c] letter was correct
        ScanGuess[C]  = '-'                         !Do NOT find [c] letter again in Scan Guess
        ScanSecret[C] = '-'                         !Do NOT find [c] letter again in Scan Secret
        HisQ:Array.Letter[C] = CLetter
        Correct[C]=CLetter
        HisQ:Array.Style[C] = LetterState:Correct
    END
    !0{PROP:Text}='TakeGuessOK  ScanGuess='& ScanGuess &'  ScanSecret='& ScanSecret &'  ScanCorrect='& ScanCorrect
    LOOP C=1 TO WORD_LENGTH
        CLetter = ScanGuess[C]
        IF Cletter < 'A' THEN CYCLE.                 !Was found Correct so ='-' or ' '
        HisQ:Array.Letter[C] = CLetter
        LetterState = LetterState:NoWhere
        MissedAlready = INSTRING(CLetter,MissedLetters)
        PosMissed = INSTRING(CLetter,ScanSecret)    !Note will not have Previous Correct letters found
        IF PosMissed THEN
              IF ~MissedAlready THEN
                  MissedLetters=CLIP(MissedLetters) & CLetter
              END
              LetterState = LetterState:Missed
              ScanSecret[PosMissed] = ''            !Do NOT find letter missed again in Scan Secret so extras are NoWhere
        ELSIF INSTRING(CLetter,ScanCorrect) THEN    !Duplictes Correct Letter this time but not elsewhere
              LetterState = LetterState:NoWhere     !Show as NoWhere in History
        ELSIF INSTRING(CLetter,Correct) THEN        !Duplictes Correct Letter
              LetterState = LetterState:Missed      !Already in Correct show missed Missed
        ELSIF ~INSTRING(CLetter,NoWhereLetters) THEN
              NoWhereLetters=CLIP(NoWhereLetters) & CLetter
             ! message('NoWhereLetters=' & NoWhereLetters &'||Letter=' & CLetter )
        ELSE
             ! message('ELSE   NoWhereLetters=' & NoWhereLetters &'||Letter=' & CLetter )
        END
        HisQ:Array.Style[C] = LetterState
    END
    ADD(HistoryQ)
    IF GuessWord = SecretWord THEN ?SecretWord{PROP:Password}=0. !Solved it so Unhide Secret
    DISPLAY
    RETURN    
!------------------------------------------
TakeSecret                      PROCEDURE()
    CODE
    SecretWord=UPPER(SecretWord)
    WordQ.Word = SecretWord
    GET(WordQ,WordQ.Word)
    IF ERRORCODE() OR SecretWord[1]<'A' THEN
       Message('"' & SecretWord &'" is not a word in the dictionary.',|
                     'Alert',ICON:Asterisk)
       SecretWord = SecretBefore
       DISPLAY
       RETURN
    END
    SecretBefore=SecretWord
    NewGameInit()
    SecretWord=SecretBefore
    RETURN
!--------------------------------------
TakeSecretUnHide    PROCEDURE()
    CODE
    !     ENTRY(@s5),AT(35,4,38),USE(SecretWord),SKIP,FLAT,UPR,READONLY
    ?SecretWord{PROP:Password}=1-UnHideSecret
    ?SecretWord{PROP:Skip}=1-UnHideSecret
    ?SecretWord{PROP:READONLY}=1-UnHideSecret
    RETURN
!--------------------------------------
InitWindow                    PROCEDURE
StringFEQ   SIGNED,AUTO
StyleNo     LONG,AUTO
Light_Gray  EQUATE(00E0E0E0h)  !c11 COLOR:LightGray
  CODE
    LOOP StringFEQ = ?Letter:Q TO ?Letter:M
       Letters.InitControl(StringFEQ)
    END
    ?HistoryList{PROPLIST:DefHdrBackColor} = 3947066
    ?HistoryList{PROPLIST:DefHdrTextColor} = Light_Gray
    ?HistoryList{PropStyle:BackColor, LetterState:Nowhere} = 3947066        !Dark Gray
    ?HistoryList{PropStyle:TextColor, LetterState:Nowhere} = Light_Gray
    ?HistoryList{PropStyle:BackColor, LetterState:Missed}  = 3907508        !Yellow
    ?HistoryList{PropStyle:TextColor, LetterState:Missed}  = COLOR:White
    ?HistoryList{PropStyle:BackColor, LetterState:Correct} = 5147986        !Green
    ?HistoryList{PropStyle:TextColor, LetterState:Correct} = COLOR:White
    LOOP StyleNo = LetterState:Nowhere TO LetterState:Correct
       ?HistoryList{PropStyle:BackSelected, StyleNo} = ?HistoryList{PropStyle:BackColor, StyleNo}
       ?HistoryList{PropStyle:TextSelected, StyleNo} = ?HistoryList{PropStyle:FontColor, StyleNo}
    END
!--------------------------------------
Compute                       PROCEDURE
W                               LONG
  CODE
  Letters.DisplayLetters(Correct, MissedLetters, NoWhereLetters)
  DISPLAY()
  RETURN
!==============================================================================
LoadWords                     PROCEDURE
                              MAP
LoadWord                        PROCEDURE(STRING WordUpr)
                              END
WordsAlphaTxt                   EQUATE('PlayWordle_Words.TXT')  !Word file from Wordle JS Source
WordsFilename                   STRING(FILE:MaxFilePath),STATIC
WordsFile                       FILE,DRIVER('ASCII'),NAME(WordsFilename),PRE(WF)
                                  RECORD
Word                                STRING(30)
                                  END
                                END
  CODE
  FREE(WordQ)
  WordQ_RecordsForPuzzle=0
  WordsFilename = WordsAlphaTxt  !'words_alpha_len'& WORD_LENGTH &'.txt'    !Carl wrote smaller 5 letter word file FROM mh BIG FILE
  IF ~EXISTS(WordsFilename) THEN
      HALT(,'Did not find words file: '& WordsFilename)
  END

  OPEN(WordsFile, 40h)  !readOnly DenyNone
  IF ERRORCODE() THEN
     Message('Error on Open Words File: ' & WordsFilename &'||'& Error(),'Alert')
  END
  SET(WordsFile)
  LOOP
    NEXT(WordsFile)
    IF ERRORCODE() THEN BREAK.
    IF WF:Word[1:5]='-----' AND ~WordQ_RecordsForPuzzle THEN   !Put '-----' line in TXT file    !var Ta=["aahed" ...  Puzzle words end at "aahed"
       WordQ_RecordsForPuzzle = RECORDS(WordQ)
       SORT(WordQ,WordQ.Word)                   !Play 2300 Words are NOT sorted, probably to be random
       WordQ.Word='-----'
       ADD(WordQ)
       CYCLE
    END
    IF LEN(CLIP(WF:Word)) <> WORD_LENGTH THEN CYCLE.
    WF:Word=UPPER(WF:Word)
    CASE WF:Word[1]
    OF 'A' TO 'Z'       !File can have ; comments
        LoadWord(WF:Word)
    END
  END
  CLOSE(WordsFile)
  IF ~RECORDS(WordQ) THEN
     Message('Found Zero Words in file: ' & WordsFilename ,'Alert')
  ELSIF ~WordQ_RecordsForPuzzle THEN
     Message('Did not find end of puzzle works "-----" "AAHED"||Word file: ' & WordsFilename ,'Alert')
     WordQ_RecordsForPuzzle = RECORDS(WordQ)
  END
  RETURN
!--------------------------------------
LoadWord                      PROCEDURE(STRING WordUpr)
  CODE
  CLEAR(WordQ)
  WordQ.Word = WordUpr
  ADD(WordQ)
!==============================================================================
!##############################################################################
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
!  SELF.RegionFEQ = CREATE(0, CREATE:region)
!  GETPOSITION(SELF.StringFEQ, X, Y, W, H)
!  SETPOSITION(SELF.RegionFEQ, X, Y, W, H)
!  UNHIDE(SELF.RegionFEQ)
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

!  SELF.RegionFEQ{PROP:Cursor} = CHOOSE(~INLIST(SELF.State, LetterState:Missed, LetterState:Correct), |
!      CURSOR:Hand, CURSOR:None)

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
Letters.Construct             PROCEDURE
                              MAP
AddLetter                       PROCEDURE(STRING Letter)
                              END
C                               BYTE,AUTO
  CODE
  LOOP C = VAL('A') TO VAL('Z')
    AddLetter(CHR(C))
  END
  !ViewQueue_LetterQ()
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
Letters.InitControl           PROCEDURE(SIGNED StringFEQ)
  CODE
  SELF.FetchLetter(StringFEQ{PROP:Text})
  LetterQ.Handler.InitControl(StringFEQ)

!==============================================================================
Letters.SetState              PROCEDURE(STRING Letter,BYTE State)
  CODE
  SELF.FetchLetter(Letter)
  LetterQ.Handler.SetState(State)
  RETURN
!==============================================================================
Letters.DisplayLetters        PROCEDURE(STRING Correct,STRING Missed,STRING NoWhere)
L                               BYTE,AUTO
  CODE
  LOOP L = 1 TO RECORDS(LetterQ)
    GET(LetterQ, L)
    IF INSTRING(LetterQ.Letter, Correct)
      LetterQ.Handler.SetState(LetterState:Correct)
    ELSIF INSTRING(LetterQ.Letter, Missed)
      LetterQ.Handler.SetState(LetterState:Missed)
    ELSIF INSTRING(LetterQ.Letter, NoWhere)         !Carl need to track NoWhere
      LetterQ.Handler.SetState(LetterState:Nowhere)
    ELSE  !IF LetterQ.Handler.State <> LetterState:Nowhere
      LetterQ.Handler.SetState(LetterState:Unknown)
    END
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
!=====================================================
RandomWord PROCEDURE()!,STRING
WordCnt LONG,AUTO
R LONG,AUTO
    CODE
    WordCnt=WordQ_RecordsForPuzzle   !RECORDS(WordQ)
    LOOP CLOCK() % 100 + 1 TIMES
        R=RANDOM(1,WordCnt)
    END
    GET(WordQ,R)
    RETURN WordQ.Word
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
