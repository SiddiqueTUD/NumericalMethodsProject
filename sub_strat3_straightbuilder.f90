! Subroutine for Strategy 3: Straight Builder - build consecutive sequences
!**************************************************************************!
!**************************************************************************!
subroutine strategy_straight_builder(grid,dice,bestrow,bestcol)
implicit none

integer grid(5,5)          ! current grid
integer dice               ! dice value to place
integer bestrow,bestcol    ! best position found
integer row,col            ! loop indexes
integer i                  ! loop index
integer linevals(5)        ! values in a line
integer nfilled            ! number of filled positions
integer minval,maxval      ! min and max values
integer range              ! range of values
integer potential          ! straight potential
integer bestpotential      ! best potential found

! Initialize
!**************************************************************************!
bestpotential=9999
bestrow=1
bestcol=1

! Find position with best straight potential (smallest range)
!**************************************************************************!
do row=1,5
    do col=1,5
        if (grid(row,col).eq.0) then
            potential=0
            
            ! Check row potential
            nfilled=0
            do i=1,5
                if (grid(row,i).ne.0) then
                    nfilled=nfilled+1
                    linevals(nfilled)=grid(row,i)
                endif
            enddo
            
            if (nfilled.gt.0) then
                linevals(nfilled+1)=dice
                minval=linevals(1)
                maxval=linevals(1)
                do i=1,nfilled+1
                    if (linevals(i).lt.minval) minval=linevals(i)
                    if (linevals(i).gt.maxval) maxval=linevals(i)
                enddo
                range=maxval-minval
                if (range.le.4) then
                    potential=potential+range
                else
                    potential=potential+10
                endif
            endif
            
            ! Check column potential
            nfilled=0
            do i=1,5
                if (grid(i,col).ne.0) then
                    nfilled=nfilled+1
                    linevals(nfilled)=grid(i,col)
                endif
            enddo
            
            if (nfilled.gt.0) then
                linevals(nfilled+1)=dice
                minval=linevals(1)
                maxval=linevals(1)
                do i=1,nfilled+1
                    if (linevals(i).lt.minval) minval=linevals(i)
                    if (linevals(i).gt.maxval) maxval=linevals(i)
                enddo
                range=maxval-minval
                if (range.le.4) then
                    potential=potential+range
                else
                    potential=potential+10
                endif
            endif
            
            ! Keep if best potential (smallest range)
            ! OR if this is first empty position (bestpotential still 9999)
            if (potential.lt.bestpotential.or.bestpotential.eq.9999) then
                bestpotential=potential
                bestrow=row
                bestcol=col
            endif
        endif
    enddo
enddo

end subroutine strategy_straight_builder
