! Subroutine for Strategy 1: Greedy - try all positions, pick best
!**************************************************************************!
!**************************************************************************!
subroutine strategy_greedy(grid,dice,bestrow,bestcol)
implicit none

integer grid(5,5)          ! current grid
integer dice               ! dice value to place
integer bestrow,bestcol    ! best position found
integer testgrid(5,5)      ! temporary grid for testing
integer row,col            ! loop indexes
integer score              ! score for testing
integer bestscore          ! best score found

! Initialize
!**************************************************************************!
bestscore=-999

! Try all empty positions
!**************************************************************************!
do row=1,5
    do col=1,5
        if (grid(row,col).eq.0) then
            ! Test this position
            testgrid=grid
            testgrid(row,col)=dice
            call calculate_total_score(testgrid,score)
            
            ! Keep if best so far
            if (score.gt.bestscore) then
                bestscore=score
                bestrow=row
                bestcol=col
            endif
        endif
    enddo
enddo

end subroutine strategy_greedy
