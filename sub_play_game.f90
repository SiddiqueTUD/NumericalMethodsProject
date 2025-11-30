! Subroutine to play one complete game (25 dice rolls)
!**************************************************************************!
!**************************************************************************!
subroutine play_one_game(strat,grid,score)
implicit none

integer strat              ! strategy number
integer grid(5,5)          ! game grid
integer score              ! final score
integer dice               ! dice value (2-12)
integer row,col            ! position to place dice
integer turn               ! turn counter
real    r1,r2              ! random numbers for dice
integer d1,d2              ! individual dice

! Initialize grid to zero
!**************************************************************************!
grid=0

! Play 25 turns (fill entire 5x5 grid)
!**************************************************************************!
do turn=1,25
    ! Roll two dice
    call random_number(r1)
    call random_number(r2)
    d1=int(r1*6)+1
    d2=int(r2*6)+1
    dice=d1+d2
    
    ! Choose position based on strategy
    if (strat.eq.1) then
        call strategy_greedy(grid,dice,row,col)
    else if (strat.eq.2) then
        call strategy_set_builder(grid,dice,row,col)
    else if (strat.eq.3) then
        call strategy_straight_builder(grid,dice,row,col)
    else
        call strategy_random(grid,dice,row,col)
    endif
    
    ! Place dice on grid
    grid(row,col)=dice
enddo

! Calculate final score
!**************************************************************************!
call calculate_total_score(grid,score)

end subroutine play_one_game
