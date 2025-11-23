@echo off
echo ===== Compiling Fortran Files =====

:: Step 1: Compile .f90 files into .o object files
gfortran -c dice_game_common.f90
gfortran -c strategy_greedy.f90
gfortran -c strategy_set_builder.f90
gfortran -c strategy_straight_builder.f90
gfortran -c strategy_random.f90

echo ===== Linking Object Files =====

:: Step 2: Link .o files into executables
gfortran dice_game_common.o strategy_greedy.o -o strategy_greedy.exe
gfortran dice_game_common.o strategy_set_builder.o -o strategy_set_builder.exe
gfortran dice_game_common.o strategy_straight_builder.o -o strategy_straight_builder.exe
gfortran dice_game_common.o strategy_random.o -o strategy_random.exe

echo ===== Running Programs =====

:: Step 3: Run the executables one by one
strategy_greedy.exe
pause
strategy_set_builder.exe
pause
strategy_straight_builder.exe
pause
strategy_random.exe
pause

echo ===== Finished =====
pause

