program strategy_straight_builder
    use dice_game_common
    implicit none
    
    type(GameGrid) :: gg, best_grid
    integer :: rolls(NUM_ROLLS)
    integer :: i, num_runs, run
    integer :: best_score, total_score, current_score
    real :: avg_score
    integer :: seed_size
    integer, allocatable :: seed(:)
    
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    call system_clock(count=seed(1))
    do i = 2, seed_size
        seed(i) = seed(1) + i * 456789123
    end do
    call random_seed(put=seed)
    deallocate(seed)
    
    print *, "=========================================="
    print *, "  STRATEGY 3: STRAIGHT BUILDER           "
    print *, "=========================================="
    print *, ""
    
    print *, "Enter number of simulation runs (100-1000 recommended):"
    read *, num_runs
    
    if (num_runs < 1) num_runs = 100
    if (num_runs > 10000) num_runs = 10000
    
    best_score = 0
    total_score = 0
    call init_board(best_grid)
    
    do run = 1, num_runs
        call init_board(gg)
        
        do i = 1, NUM_ROLLS
            rolls(i) = roll_dice()
        end do
        
        call apply_straight_builder_strategy(gg, rolls)
        
        current_score = calculate_total_score(gg)
        total_score = total_score + current_score
        
        if (current_score > best_score) then
            best_score = current_score
            best_grid = gg
        end if
        
        if (mod(run, 100) == 0) then
            write(*, '(A, I5, A, I5)') "Completed ", run, " runs. Best so far: ", best_score
        end if
    end do
    
    avg_score = real(total_score) / real(num_runs)
    
    print *, ""
    print *, "=========================================="
    print *, "          SIMULATION RESULTS              "
    print *, "=========================================="
    write(*, '(A, I5)') "Number of runs:    ", num_runs
    write(*, '(A, F8.2)') "Average score:     ", avg_score
    write(*, '(A, I5)') "Best score:        ", best_score
    print *, "=========================================="
    print *, ""
    
    print *, "Showing the best score example..."
    print *, ""
    
    call print_board(best_grid)
    call print_score_breakdown(best_grid)
    
contains

    subroutine apply_straight_builder_strategy(gg, roll_values)
        type(GameGrid), intent(inout) :: gg
        integer, intent(in) :: roll_values(NUM_ROLLS)
        integer :: roll_idx, best_row, best_col
        integer :: row, col
        real :: best_score, current_score
        
        do roll_idx = 1, NUM_ROLLS
            best_score = -1000.0
            best_row = 1
            best_col = 1
            
            do row = 1, GRID_SIZE
                do col = 1, GRID_SIZE
                    if (is_empty(gg, row, col)) then
                        current_score = evaluate_straight_placement(gg, row, col, roll_values(roll_idx))
                        
                        if (current_score > best_score) then
                            best_score = current_score
                            best_row = row
                            best_col = col
                        end if
                    end if
                end do
            end do
            
            call place_value(gg, best_row, best_col, roll_values(roll_idx))
        end do
    end subroutine apply_straight_builder_strategy
    
    function evaluate_straight_placement(gg, row, col, value) result(score)
        type(GameGrid), intent(in) :: gg
        integer, intent(in) :: row, col, value
        real :: score
        integer :: line_indices(3)
        integer :: num_lines, i, line_idx
        integer :: line_values(GRID_SIZE)
        logical :: is_diagonal
        real :: line_score
        
        score = 0.0
        
        num_lines = 2
        line_indices(1) = row
        line_indices(2) = GRID_SIZE + col
        
        if (row == col) then
            num_lines = num_lines + 1
            line_indices(num_lines) = 2 * GRID_SIZE + 1
        end if
        
        if (row + col == GRID_SIZE + 1) then
            num_lines = num_lines + 1
            line_indices(num_lines) = 2 * GRID_SIZE + 2
        end if
        
        do i = 1, num_lines
            line_idx = line_indices(i)
            call get_line(gg, line_idx, line_values, is_diagonal)
            
            line_score = calculate_straight_potential(gg, line_idx, value)
            
            if (is_diagonal) then
                line_score = line_score * 1.8
            end if
            
            score = score + line_score
        end do
        
    end function evaluate_straight_placement
    
    function calculate_straight_potential(gg, line_idx, new_value) result(potential)
        type(GameGrid), intent(in) :: gg
        integer, intent(in) :: line_idx, new_value
        real :: potential
        integer :: existing_values(GRID_SIZE)
        integer :: num_filled, i, j
        integer :: min_val, max_val, range_val
        logical :: has_seven
        integer :: unique_vals(GRID_SIZE)
        integer :: num_unique
        logical :: is_consecutive
        
        potential = 0.0
        num_filled = 0
        has_seven = .false.
        
        do i = 1, GRID_SIZE
            if (line_idx <= GRID_SIZE) then
                if (gg%filled(line_idx, i)) then
                    num_filled = num_filled + 1
                    existing_values(num_filled) = gg%cells(line_idx, i)
                end if
            else if (line_idx <= 2 * GRID_SIZE) then
                if (gg%filled(i, line_idx - GRID_SIZE)) then
                    num_filled = num_filled + 1
                    existing_values(num_filled) = gg%cells(i, line_idx - GRID_SIZE)
                end if
            else if (line_idx == 2 * GRID_SIZE + 1) then
                if (gg%filled(i, i)) then
                    num_filled = num_filled + 1
                    existing_values(num_filled) = gg%cells(i, i)
                end if
            else
                if (gg%filled(i, GRID_SIZE - i + 1)) then
                    num_filled = num_filled + 1
                    existing_values(num_filled) = gg%cells(i, GRID_SIZE - i + 1)
                end if
            end if
        end do
        
        if (num_filled == 0) then
            if (new_value >= 5 .and. new_value <= 9) then
                potential = 5.0
            else
                potential = 3.0
            end if
            return
        end if
        
        num_filled = num_filled + 1
        existing_values(num_filled) = new_value
        
        call get_unique_sorted(existing_values, num_filled, unique_vals, num_unique)
        
        min_val = unique_vals(1)
        max_val = unique_vals(num_unique)
        range_val = max_val - min_val
        
        if (range_val <= 4) then
            is_consecutive = .true.
            do i = 1, num_unique - 1
                if (unique_vals(i+1) - unique_vals(i) > 1) then
                    is_consecutive = .false.
                end if
            end do
            
            if (is_consecutive) then
                potential = potential + real(num_unique) * 8.0
                
                do i = 1, num_unique
                    if (unique_vals(i) == 7) then
                        has_seven = .true.
                        exit
                    end if
                end do
                
                if (.not. has_seven .and. num_unique >= 3) then
                    potential = potential + 10.0
                else if (num_unique >= 3) then
                    potential = potential + 5.0
                end if
                
                if (num_unique == 4) then
                    potential = potential + 15.0
                end if
            else
                if (range_val == num_unique) then
                    potential = potential + real(num_unique) * 2.0
                else
                    potential = potential + 1.0
                end if
            end if
        else
            potential = potential - 5.0
        end if
        
    end function calculate_straight_potential
    
    subroutine get_unique_sorted(arr, n, unique, n_unique)
        integer, intent(in) :: arr(n), n
        integer, intent(out) :: unique(n), n_unique
        integer :: i, j
        logical :: is_duplicate
        integer :: temp
        
        n_unique = 0
        
        do i = 1, n
            is_duplicate = .false.
            do j = 1, n_unique
                if (arr(i) == unique(j)) then
                    is_duplicate = .true.
                    exit
                end if
            end do
            if (.not. is_duplicate) then
                n_unique = n_unique + 1
                unique(n_unique) = arr(i)
            end if
        end do
        
        do i = 1, n_unique - 1
            do j = 1, n_unique - i
                if (unique(j) > unique(j+1)) then
                    temp = unique(j)
                    unique(j) = unique(j+1)
                    unique(j+1) = temp
                end if
            end do
        end do
    end subroutine get_unique_sorted

end program strategy_straight_builder