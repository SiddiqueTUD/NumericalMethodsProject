! Subroutine to score one line (row/column/diagonal)
!**************************************************************************!
!**************************************************************************!
subroutine score_one_line(line,points)
implicit none

integer line(5)            ! line to score
integer points             ! points earned
integer sorted(5)          ! sorted line
integer counts(12)         ! count of each value (2-12)
integer maxcount           ! maximum count
integer numpairs           ! number of pairs
integer i,j                ! loop indexes
integer temp               ! temporary variable

! Initialize
!**************************************************************************!
points=0
counts=0

! Sort the line (simple bubble sort)
!**************************************************************************!
sorted=line
do i=1,4
    do j=i+1,5
        if (sorted(j).lt.sorted(i)) then
            temp=sorted(i)
            sorted(i)=sorted(j)
            sorted(j)=temp
        endif
    enddo
enddo

! Count occurrences of each value
!**************************************************************************!
do i=1,5
    if (line(i).ge.2.and.line(i).le.12) then
        counts(line(i))=counts(line(i))+1
    endif
enddo

! Find maximum count and number of pairs
!**************************************************************************!
maxcount=0
numpairs=0
do i=2,12
    if (counts(i).gt.maxcount) then
        maxcount=counts(i)
    endif
    if (counts(i).eq.2) then
        numpairs=numpairs+1
    endif
enddo

! Check patterns in priority order (CORRECT SCORING SYSTEM)
!**************************************************************************!
! Check for Straight WITHOUT 7: 12 points (HIGHEST)
if (sorted(2).eq.sorted(1)+1.and. &
    sorted(3).eq.sorted(2)+1.and. &
    sorted(4).eq.sorted(3)+1.and. &
    sorted(5).eq.sorted(4)+1) then
    ! Check if 7 is NOT in the straight
    if (sorted(1).ne.7.and.sorted(2).ne.7.and.sorted(3).ne.7.and. &
        sorted(4).ne.7.and.sorted(5).ne.7) then
        points=12
        return
    endif
endif

! Five of a Kind: 10 points
if (maxcount.eq.5) then
    points=10
    return
endif

! Check for Straight WITH 7: 8 points
if (sorted(2).eq.sorted(1)+1.and. &
    sorted(3).eq.sorted(2)+1.and. &
    sorted(4).eq.sorted(3)+1.and. &
    sorted(5).eq.sorted(4)+1) then
    ! Check if 7 IS in the straight
    if (sorted(1).eq.7.or.sorted(2).eq.7.or.sorted(3).eq.7.or. &
        sorted(4).eq.7.or.sorted(5).eq.7) then
        points=8
        return
    endif
endif

! Full House (3 of one + 2 of another): 8 points
if (maxcount.eq.3.and.numpairs.eq.1) then
    points=8
    return
endif

! Four of a Kind: 6 points
if (maxcount.eq.4) then
    points=6
    return
endif

! Three of a Kind: 3 points
if (maxcount.eq.3) then
    points=3
    return
endif

! Two Pairs: 3 points
if (numpairs.eq.2) then
    points=3
    return
endif

! One Pair: 1 point
if (numpairs.eq.1) then
    points=1
    return
endif

! Nothing: 0 points
points=0

end subroutine score_one_line
