! Program for dice game simulation with 4 different strategies
!**************************************************************************!
!**************************************************************************!
program wuerfel_game
implicit none

integer grid(5,5)           ! 5x5 game grid
integer bestscore           ! best score for a strategy
integer avgscores(4)        ! average scores for all strategies
integer bestscores(4)       ! best scores for all strategies
integer bestgrids(4,5,5)    ! best grids for all strategies
integer nsim                ! number of simulations
integer strat               ! strategy number (1-4)
integer i,j                 ! loop indexes
integer seed_size           ! size of random seed array
integer, allocatable :: seed(:)  ! random seed array
integer clock_time          ! system clock for random seed

! Initialize random number generator with system time
!**************************************************************************!
call random_seed(size=seed_size)
allocate(seed(seed_size))
call system_clock(count=clock_time)
seed = clock_time + 37 * (/ (i, i=1,seed_size) /)
call random_seed(put=seed)
deallocate(seed)

! Input number of simulations
!**************************************************************************!
write(*,*) '=========================================='
write(*,*) 'DICE GAME - 4 STRATEGIES COMPARISON'
write(*,*) '=========================================='
write(*,*)
write(*,*) 'Enter number of simulations:'
read(*,*) nsim

! Test all 4 strategies
!**************************************************************************!
do strat=1,4
    call simulate_strategy(strat,nsim,grid,bestscore,avgscores(strat))
    bestscores(strat)=bestscore
    bestgrids(strat,:,:)=grid
enddo

! Output comparison results
!**************************************************************************!
write(*,*)
write(*,*) '=========================================='
write(*,*) '          FINAL RESULTS'
write(*,*) '=========================================='
write(*,*)
write(*,*) 'Strategy             Average    Best'
write(*,*) '------------------------------------------'
write(*,'(A,I8,I8)') ' 1. Greedy           ', avgscores(1), bestscores(1)
write(*,'(A,I8,I8)') ' 2. Set Builder      ', avgscores(2), bestscores(2)
write(*,'(A,I8,I8)') ' 3. Straight Builder ', avgscores(3), bestscores(3)
write(*,'(A,I8,I8)') ' 4. Random           ', avgscores(4), bestscores(4)
write(*,*) '=========================================='
write(*,*)

! Output best grids for each strategy
!**************************************************************************!
do strat=1,4
    write(*,*)
    if (strat.eq.1) write(*,*) 'Best grid for Strategy 1 (Greedy):'
    if (strat.eq.2) write(*,*) 'Best grid for Strategy 2 (Set Builder):'
    if (strat.eq.3) write(*,*) 'Best grid for Strategy 3 (Straight Builder):'
    if (strat.eq.4) write(*,*) 'Best grid for Strategy 4 (Random):'
    
    do i=1,5
        write(*,'(5I4)') bestgrids(strat,i,:)
    enddo
enddo

write(*,*)
write(*,*) 'Press Enter to exit'
read(*,*)

end program wuerfel_game
