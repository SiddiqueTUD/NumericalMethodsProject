program strategy_greedy
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
        seed(i) = seed(1) + i * 123456789
    end do
    call random_seed(put=seed)
    deallocate(seed)
    
    print *, "=========================================="
    print *, "   STRATEGY 1: GREEDY (Immediate Gain)   "
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
        
        call apply_greedy_strategy(gg, rolls)
        
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

    subroutine apply_greedy_strategy(gg, roll_values)
        type(GameGrid), intent(inout) :: gg
        integer, intent(in) :: roll_values(NUM_ROLLS)
        integer :: roll_idx, best_row, best_col
        integer :: row, col
        real :: best_heuristic, current_heuristic
        
        do roll_idx = 1, NUM_ROLLS
            best_heuristic = -1000.0
            best_row = 1
            best_col = 1
            
            do row = 1, GRID_SIZE
                do col = 1, GRID_SIZE
                    if (is_empty(gg, row, col)) then
                        current_heuristic = evaluate_placement(gg, row, col, roll_values(roll_idx))
                        
                        if (current_heuristic > best_heuristic) then
                            best_heuristic = current_heuristic
                            best_row = row
                            best_col = col
                        end if
                    end if
                end do
            end do
            
            call place_value(gg, best_row, best_col, roll_values(roll_idx))
        end do
    end subroutine apply_greedy_strategy
    
    function evaluate_placement(gg, row, col, value) result(heuristic)
        type(GameGrid), intent(in) :: gg
        integer, intent(in) :: row, col, value
        real :: heuristic
        type(GameGrid) :: temp_grid
        integer :: score_before, score_after
        integer :: line_indices(3)
        integer :: num_lines, i, line_idx
        integer :: line_values(GRID_SIZE)
        logical :: is_diagonal
        character(len=20) :: category
        integer :: line_score_before, line_score_after
        
        temp_grid = gg
        
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
        
        heuristic = 0.0
        do i = 1, num_lines
            line_idx = line_indices(i)
            
            call get_line(temp_grid, line_idx, line_values, is_diagonal)
            call evaluate_line(line_values, is_diagonal, category, line_score_before)
            
            call place_value(temp_grid, row, col, value)
            
            call get_line(temp_grid, line_idx, line_values, is_diagonal)
            call evaluate_line(line_values, is_diagonal, category, line_score_after)
            
            heuristic = heuristic + real(line_score_after - line_score_before)
            
            if (line_score_after > line_score_before) then
                heuristic = heuristic + 0.5
            end if
            
            temp_grid%cells(row, col) = 0
            temp_grid%filled(row, col) = .false.
            temp_grid%fill_count = temp_grid%fill_count - 1
        end do
        
        if (row == col .or. row + col == GRID_SIZE + 1) then
            heuristic = heuristic * 1.1
        end if
        
    end function evaluate_placement

end program strategy_greedy