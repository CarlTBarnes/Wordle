# Wordle

This repo contains Clarion source for a Wordle Game and Wordle Tool to help solve.

If you are not familiar with the
[online Wordle game](https://www.nytimes.com/games/wordle)
 you can read about [Wordle on Wikipedia](https://en.wikipedia.org/wiki/Wordle).

## Wordle Tool

Mike Hanson created a Wordle Tool to help solve the puzzle by suggesting words.
He presented it on [ClarionLive 565](https://www.youtube.com/watch?v=K4lThuFXeAw)
 and published his Wordle [CwProj on the BoxSoft GitHub](https://github.com/BoxSoft/Wordle).

In this Repo you will find my modified version of Mike's Wordle tool.

 * Added a list of words containing only new letters that have not previously been guessed
 * Changed the scoring to be simpler and display the word score in the list
 * Added Button New that clears all letters for playing a new game
 * Added Button Run starts a new EXE
 * Added Button Ltr that displays the letter scoring

To use this tool fill in the Correct Green letters in the Green entry boxes.
 Fill in the Yellow letters in the Yellow entry boxes then tick the check boxes for the positions 1-5 that missed.
 For any gray letters that are not in the word you must click them in the keyboard at the bottom of the window.

![tool 1](images/tool1.png)

In the Wordle game above the guess "AROSE" scores "R,O" as Yellow and "A,S,E" as Gray. In the Tool you enter the "R,O" into the Yellow entries and check boxes 2 and 3 to identify the position of the miss.
 For incorrect letters "A,S,E" you must click in the bottom keyboard to note those as Gray.
 The Tool then calculates the two word lists and scores.

The Tool suggests LIROT as the best word that contains R and O but not A,S,E.
By "best" the letters I,L,T are a bit more popular than I,N,T in INTRO or NITRO.
 I would use INTRO for my next guess because Wordle uses 2300 fairly common words.
  LIROT is ahhh "Turkish monetary unit" ?

On the far right of the Tool window the "New Letter Words" list has words that contain
 none of the "A,R,O,S,E" letters.
 I chose UNTIL for my next guess.
 The game scored "N" as Yellow, "I" as Green and "U,T,L" as Gray.
 In the Tool enter "N" in the Yellow entry and check box 2, enter "I" in the 4th Green entry and then click on "U,T,L" in the bottom keyboard.

 This narrows the possible list down to 3 with ROBIN being the word I guess and it is the correct answer.

![tool 2](images/tool2.png)

If the 3 words possible (PORIN ROBIN RONIN) were all good choices you could use the Run button to start a new instance of the Tool.
 In that you could enter in the Yellow boxes the letters not in common of those words: P,R,B.
 That would provide a list of words so that with one more guess PROBE you could find the last 2 letters in the secret word; otherwise, it may take 3 guesses. Many times you will have 5+ words like: STILL SHILL SPILL SKILL SWILL.

![tool 3](images/tool3.png)

---

## Wordle Game

The online Wordle game only allows a single puzzle per day.
 That made it hard to work on the tool code so I created my own version of the Wordle game written in Clarion.
 Mike's tool code provided many of the elements I needed.

![play 1](images/play1.png)

My game has a few differences from the online version:

 * You can Unhide the puzzle Secret Word at any time
 * Override the Secret Word (unhide first). Allows repeating games or testing certain words and letters.
 * Guess nonsense non-dictionary words by clicking Ignore on warning message e.g. BYANY
 * No limit of 6 guesses
 * Undo a guess by selecting it then pressing the Delete key
 * [...] button shows list of suggested first words like: SOARE AROSE ARISE SLICE
