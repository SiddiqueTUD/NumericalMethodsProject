! dice_game_common.f90
! Common utilities for the dice game

module dice_game_common
    implicit none
    
    integer, parameter :: GRID_SIZE = 5
    integer, parameter :: NUM_ROLLS = 25
    integer, parameter :: NUM_LINES = 12
    
    type :: GameGrid
        integer :: cells(GRID_SIZE, GRID_SIZE)
        logical :: filled(GRID_SIZE, GRID_SIZE)
        integer :: fill_count
    end type GameGrid
    
contains

    subroutine init_board(gg)
        type(GameGrid), intent(out) :: gg
        gg%cells = 0
        gg%filled = .false.
        gg%fill_count = 0
    end subroutine init_board
    
    subroutine place_value(gg, row, col, value)
        type(GameGrid), intent(inout) :: gg
        integer, intent(in) :: row, col, value
        
        if (.not. gg%filled(row, col)) then
            gg%cells(row, col) = value
            gg%filled(row, col) = .true.
            gg%fill_count = gg%fill_count + 1
        end if
    end subroutine place_value
    
    function is_empty(gg, row, col) result(empty)
        type(GameGrid), intent(in) :: gg
        integer, intent(in) :: row, col
        logical :: empty
        empty = .not. gg%filled(row, col)
    end function is_empty
    
    function roll_dice() result(sum_val)
        integer :: sum_val
        real :: r1, r2
        integer :: d1, d2
        
        call random_number(r1)
        call random_number(r2)
        d1 = int(r1 * 6) + 1
        d2 = int(r2 * 6) + 1
        sum_val = d1 + d2
    end function roll_dice
    
    subroutine get_line(gg, line_idx, line_values, is_diagonal)
        type(GameGrid), intent(in) :: gg
        integer, intent(in) :: line_idx
        integer, intent(out) :: line_values(GRID_SIZE)
        logical, intent(out) :: is_diagonal
        integer :: i
        
        is_diagonal = .false.
        
        if (line_idx <= GRID_SIZE) then
            line_values = gg%cells(line_idx, :)
        else if (line_idx <= 2 * GRID_SIZE) then
            line_values = gg%cells(:, line_idx - GRID_SIZE)
        else if (line_idx == 2 * GRID_SIZE + 1) then
            is_diagonal = .true.
            do i = 1, GRID_SIZE
                line_values(i) = gg%cells(i, i)
            end do
        else
            is_diagonal = .true.
            do i = 1, GRID_SIZE
                line_values(i) = gg%cells(i, GRID_SIZE - i + 1)
            end do
        end if
    end subroutine get_line
    
    subroutine evaluate_line(line_values, is_diagonal, category_name, score)
        integer, intent(in) :: line_values(GRID_SIZE)
        logical, intent(in) :: is_diagonal
        character(len=20), intent(out) :: category_name
        integer, intent(out) :: score
        
        integer :: sorted(GRID_SIZE)
        integer :: counts(2:12)
        integer :: i, max_count, num_pairs, base_score
        logical :: is_straight, has_seven
        
        sorted = line_values
        call sort_array(sorted, GRID_SIZE)
        
        counts = 0
        do i = 1, GRID_SIZE
            if (sorted(i) >= 2 .and. sorted(i) <= 12) then
                counts(sorted(i)) = counts(sorted(i)) + 1
            end if
        end do
        
        max_count = maxval(counts)
        num_pairs = count(counts == 2)
        
        is_straight = .false.
        has_seven = .false.
        if (sorted(1) + 4 == sorted(5)) then
            is_straight = .true.
            do i = 1, GRID_SIZE - 1
                if (sorted(i+1) /= sorted(i) + 1) then
                    is_straight = .false.
                    exit
                end if
            end do
            if (is_straight) then
                do i = 1, GRID_SIZE
                    if (sorted(i) == 7) then
                        has_seven = .true.
                        exit
                    end if
                end do
            end if
        end if
        
        base_score = 0
        category_name = "None"
        
        if (is_straight .and. .not. has_seven) then
            category_name = "Straight without 7"
            base_score = 12
        else if (max_count == 5) then
            category_name = "5 of a Kind"
            base_score = 10
        else if (is_straight .and. has_seven) then
            category_name = "Straight with 7"
            base_score = 8
        else if ((maxval(counts) == 3 .and. num_pairs >= 1) .or. &
                 (count(counts == 3) == 1 .and. count(counts == 2) >= 1)) then
            category_name = "Full House"
            base_score = 8
        else if (max_count == 4) then
            category_name = "4 of a Kind"
            base_score = 6
        else if (max_count == 3) then
            category_name = "3 of a Kind"
            base_score = 3
        else if (num_pairs >= 2) then
            category_name = "Two Pair"
            base_score = 3
        else if (num_pairs == 1) then
            category_name = "Pair"
            base_score = 1
        end if
        
        if (is_diagonal) then
            score = base_score * 2
        else
            score = base_score
        end if
    end subroutine evaluate_line
    
    function calculate_total_score(gg) result(total)
        type(GameGrid), intent(in) :: gg
        integer :: total
        integer :: line_values(GRID_SIZE)
        logical :: is_diagonal
        character(len=20) :: category
        integer :: score, i
        
        total = 0
        do i = 1, NUM_LINES
            call get_line(gg, i, line_values, is_diagonal)
            call evaluate_line(line_values, is_diagonal, category, score)
            total = total + score
        end do
    end function calculate_total_score
    
    subroutine print_board(gg)
        type(GameGrid), intent(in) :: gg
        integer :: i, j
        
        print *, ""
        print *, "Final 5x5 Grid:"
        print *, "------------------------"
        do i = 1, GRID_SIZE
            write(*, '(5(I3, 1X))') (gg%cells(i, j), j=1, GRID_SIZE)
        end do
        print *, "------------------------"
    end subroutine print_board
    
    subroutine print_score_breakdown(gg)
        type(GameGrid), intent(in) :: gg
        integer :: line_values(GRID_SIZE)
        logical :: is_diagonal
        character(len=20) :: category
        integer :: score, i, total
        character(len=10) :: line_type
        
        print *, ""
        print *, "Score Breakdown:"
        print *, "================================================================"
        
        total = 0
        do i = 1, NUM_LINES
            call get_line(gg, i, line_values, is_diagonal)
            call evaluate_line(line_values, is_diagonal, category, score)
            
            if (i <= GRID_SIZE) then
                write(line_type, '(A, I1)') "Row ", i
            else if (i <= 2 * GRID_SIZE) then
                write(line_type, '(A, I1)') "Col ", i - GRID_SIZE
            else if (i == 2 * GRID_SIZE + 1) then
                line_type = "Diag \   "
            else
                line_type = "Diag /   "
            end if
            
            write(*, '(A, A, 5(I3), A, A, A, I3)') &
                line_type, ": [", line_values, "] -> ", &
                trim(category), " = ", score
            
            total = total + score
        end do
        
        print *, "================================================================"
        write(*, '(A, I5)') "TOTAL SCORE: ", total
        print *, ""
    end subroutine print_score_breakdown
    
    subroutine sort_array(arr, n)
        integer, intent(inout) :: arr(n)
        integer, intent(in) :: n
        integer :: i, j, temp
        
        do i = 1, n-1
            do j = 1, n-i
                if (arr(j) > arr(j+1)) then
                    temp = arr(j)
                    arr(j) = arr(j+1)
                    arr(j+1) = temp
                end if
            end do
        end do
    end subroutine sort_array

end module dice_game_common