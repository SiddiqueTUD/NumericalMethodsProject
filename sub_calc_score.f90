! Subroutine to calculate total score for entire grid
!**************************************************************************!
!**************************************************************************!
subroutine calculate_total_score(grid,total)
implicit none

integer grid(5,5)          ! game grid
integer total              ! total score
integer line(5)            ! one line (row/col/diagonal)
integer linescore          ! score for one line
integer i,j                ! loop indexes

total=0

! Score all 5 rows
!**************************************************************************!
do i=1,5
    do j=1,5
        line(j)=grid(i,j)
    enddo
    call score_one_line(line,linescore)
    total=total+linescore
enddo

! Score all 5 columns
!**************************************************************************!
do j=1,5
    do i=1,5
        line(i)=grid(i,j)
    enddo
    call score_one_line(line,linescore)
    total=total+linescore
enddo

! Score main diagonal (doubled)
!**************************************************************************!
do i=1,5
    line(i)=grid(i,i)
enddo
call score_one_line(line,linescore)
total=total+2*linescore

! Score anti-diagonal (doubled)
!**************************************************************************!
do i=1,5
    line(i)=grid(i,6-i)
enddo
call score_one_line(line,linescore)
total=total+2*linescore

end subroutine calculate_total_score
