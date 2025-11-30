! Subroutine for Strategy 4: Random - random placement
!**************************************************************************!
!**************************************************************************!
subroutine strategy_random(grid,dice,row,col)
implicit none

integer grid(5,5)          ! current grid
integer dice               ! dice value to place
integer row,col            ! position to place
integer emptylist(25,2)    ! list of empty positions
integer nempty             ! number of empty positions
integer i,j                ! loop indexes
integer pick               ! random pick
real    r                  ! random number

! Find all empty positions
!**************************************************************************!
nempty=0
do i=1,5
    do j=1,5
        if (grid(i,j).eq.0) then
            nempty=nempty+1
            emptylist(nempty,1)=i
            emptylist(nempty,2)=j
        endif
    enddo
enddo

! Pick random empty position
!**************************************************************************!
call random_number(r)
pick=int(r*nempty)+1
if (pick.gt.nempty) pick=nempty

row=emptylist(pick,1)
col=emptylist(pick,2)

end subroutine strategy_random
