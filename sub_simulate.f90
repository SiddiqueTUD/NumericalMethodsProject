! Subroutine for simulating a strategy multiple times
!**************************************************************************!
!**************************************************************************!
subroutine simulate_strategy(strat,nsim,bestgrid,bestscore,avgscore)
implicit none

integer strat              ! strategy number (1-4)
integer nsim               ! number of simulations
integer bestgrid(5,5)      ! best grid found
integer bestscore          ! best score found
integer avgscore           ! average score
integer grid(5,5)          ! current grid
integer score              ! current score
integer totalscore         ! total score for averaging
integer i                  ! loop index

! Initialize
!**************************************************************************!
bestscore=0
totalscore=0

! Run multiple simulations
!**************************************************************************!
do i=1,nsim
    call play_one_game(strat,grid,score)
    
    totalscore=totalscore+score
    
    if (score.gt.bestscore) then
        bestscore=score
        bestgrid=grid
    endif
enddo

! Calculate average
!**************************************************************************!
avgscore=totalscore/nsim

end subroutine simulate_strategy
