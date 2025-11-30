DICE GAME - PROFESSOR'S STYLE (BEGINNER LEVEL)
===============================================

OVERVIEW:
---------
This program simulates a dice game on a 5×5 grid with 4 different
strategies. Each strategy uses a different approach to place dice
values (2-12) to maximize the final score.


FILES:
------
Main Program:
  wuerfel_game.f90              - Main program

Subroutines (each in separate file):
  sub_simulate.f90              - Simulate one strategy N times
  sub_play_game.f90             - Play one complete game (25 turns)
  sub_strat1_greedy.f90         - Strategy 1: Greedy
  sub_strat2_setbuilder.f90     - Strategy 2: Set Builder  
  sub_strat3_straightbuilder.f90 - Strategy 3: Straight Builder
  sub_strat4_random.f90         - Strategy 4: Random
  sub_calc_score.f90            - Calculate total grid score
  sub_score_line.f90            - Score one line (row/col/diagonal)

Compilation:
  compile.bat                   - Compile and run


HOW TO RUN:
-----------
1. Double-click compile.bat
2. Enter number of simulations (e.g., 1000)
3. Wait for results
4. Compare all 4 strategies


STRATEGIES EXPLAINED (SIMPLE):
-------------------------------

STRATEGY 1: GREEDY
  What it does: Tries every empty position, picks the one with
                highest total score right now
  
  Logic:
    1. For each dice roll
    2. Try placing it in every empty spot
    3. Calculate total score for each try
    4. Pick the spot with highest score
  
  Example: If dice = 7, tries all 25 positions (or however many empty),
           calculates score for each, picks best
  
  Best for: Overall highest scores


STRATEGY 2: SET BUILDER
  What it does: Counts how many matching values are already in
                each row and column, places near matches
  
  Logic:
    1. For each dice roll
    2. For each empty position:
       - Count matching values in that row
       - Count matching values in that column
       - Add them up
    3. Pick position with most matches
  
  Example: If dice = 7, and row 3 has two 7s already,
           this strategy wants to place in row 3
  
  Best for: Building pairs, triples, four-of-a-kind


STRATEGY 3: STRAIGHT BUILDER
  What it does: Calculates value ranges to find positions that
                could form consecutive sequences
  
  Logic:
    1. For each dice roll
    2. For each empty position:
       - Look at existing values in that row
       - Calculate range (max - min)
       - Do same for column
       - Add both ranges
    3. Pick position with smallest total range
  
  Example: Row has [5, 7, 8], dice = 6
           Range = 8 - 5 = 3 (good for straight!)
  
  Best for: Building straights like 5-6-7-8-9


STRATEGY 4: RANDOM
  What it does: Places dice randomly (no strategy)
  
  Logic:
    1. Find all empty positions
    2. Pick one randomly
    3. Place dice there
  
  Purpose: Baseline to show value of having a strategy
  
  Best for: Showing how bad random is!


SCORING SYSTEM:
---------------
Each row, column, and diagonal is scored separately:

Pattern                 Points
--------------------------------
Straight without 7        12    (5 consecutive, no 7 - HIGHEST!)
Five of a Kind            10    (all 5 same)
Straight with 7            8    (5 consecutive, has 7)
Full House                 8    (3 same + 2 same)
Four of a Kind             6    (4 same)
Three of a Kind            3    (3 same)
Two Pairs                  3    (2 pairs)
One Pair                   1    (1 pair)


IMPORTANT: Both diagonals score DOUBLE (multiply by 2)!

Examples with diagonal multiplier:
- Straight without 7 on diagonal: 12 × 2 = 24 points
- Five of a Kind on diagonal: 10 × 2 = 20 points
- Straight with 7 on diagonal: 8 × 2 = 16 points
- Full House on diagonal: 8 × 2 = 16 points

Total Score = All 5 rows + All 5 columns + 2 diagonals (doubled)
            = 12 lines total





WHY THESE RESULTS?
------------------
Greedy wins because:
  - Looks at actual score every time
  - Adapts to any situation
  - Always picks best immediate option

Set Builder is 2nd because:
  - Builds common patterns (pairs, triples)
  - But ignores other opportunities

Straight Builder is 3rd because:
  - Straights are less common than sets
  - Harder to complete than pairs/triples

Random is worst because:
  - No strategy at all
  - Pure luck
  - Shows ~100 point difference from best!


CODE STRUCTURE (PROFESSOR'S STYLE):
------------------------------------
Main Program (wuerfel_game.f90):
  - Very simple
  - Just loops through 4 strategies
  - Calls subroutines
  - Prints results

Each Subroutine:
  - One clear purpose
  - Simple logic
  - Uses basic do-loops
  - Clear variable names (i, j, k)
  - Lots of comments

No fancy features:
  - No modules
  - No complex data types
  - No allocatable arrays in main logic
  - Just real and integer arrays
  - Basic Fortran 90


COMPILATION:
------------
compile.bat contains:
  gfortran wuerfel_game.f90 sub_simulate.f90 sub_play_game.f90 sub_strat1_greedy.f90 sub_strat2_setbuilder.f90 sub_strat3_straightbuilder.f90 sub_strat4_random.f90 sub_calc_score.f90 sub_score_line.f90
  pause
  a.exe
  pause

This creates a.exe which runs the program.


TROUBLESHOOTING:
----------------
Problem: gfortran not found
Solution: Install MinGW or TDM-GCC

Problem: Compilation errors
Solution: Check all .f90 files are in same folder

Problem: Strange results
Solution: Try more simulations (10000 instead of 1000)





LEARNING FROM THIS CODE:
------------------------
This code teaches:
  ✓ Breaking large problem into small subroutines
  ✓ Each subroutine has one clear job
  ✓ Passing arrays between subroutines
  ✓ Simple algorithm implementation
  ✓ Comparing different strategies
  ✓ Using loops effectively
  ✓ Basic game simulation



