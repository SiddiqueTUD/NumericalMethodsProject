! Subroutine for Strategy 2: Set Builder - place near matching values
!**************************************************************************!
!**************************************************************************!
subroutine strategy_set_builder(grid,dice,bestrow,bestcol)
implicit none

integer grid(5,5)          ! current grid
integer dice               ! dice value to place
integer bestrow,bestcol    ! best position found
integer row,col            ! loop indexes
integer i                  ! loop index
integer matches            ! number of matches found
integer bestmatches        ! best matches found

! Initialize
!**************************************************************************!
bestmatches=-1
bestrow=1
bestcol=1

! Find position with most matching values in row+column
!**************************************************************************!
do row=1,5
    do col=1,5
        if (grid(row,col).eq.0) then
            matches=0
            
            ! Count matches in same row
            do i=1,5
                if (grid(row,i).eq.dice) then
                    matches=matches+1
                endif
            enddo
            
            ! Count matches in same column
            do i=1,5
                if (grid(i,col).eq.dice) then
                    matches=matches+1
                endif
            enddo
            
            ! Keep if best match OR if this is first empty position found
            if (matches.gt.bestmatches.or.bestmatches.eq.-1) then
                bestmatches=matches
                bestrow=row
                bestcol=col
            endif
        endif
    enddo
enddo

end subroutine strategy_set_builder
