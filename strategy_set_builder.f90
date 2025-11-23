program strategy_set_builder
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
        seed(i) = seed(1) + i * 987654321
    end do
    call random_seed(put=seed)
    deallocate(seed)
    
    print *, "=========================================="
    print *, "  STRATEGY 2: SET BUILDER (Multiples)    "
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
        
        call apply_set_builder_strategy(gg, rolls)
        
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

    subroutine apply_set_builder_strategy(gg, roll_values)
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
                        current_score = evaluate_set_placement(gg, row, col, roll_values(roll_idx))
                        
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
    end subroutine apply_set_builder_strategy
    
    function evaluate_set_placement(gg, row, col, value) result(score)
        type(GameGrid), intent(in) :: gg
        integer, intent(in) :: row, col, value
        real :: score
        integer :: line_indices(3)
        integer :: num_lines, i, line_idx
        integer :: line_values(GRID_SIZE)
        logical :: is_diagonal
        integer :: counts(2:12), max_count, j
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
            
            counts = 0
            do j = 1, GRID_SIZE
                if (line_idx <= GRID_SIZE) then
                    if (gg%filled(line_idx, j)) then
                        counts(gg%cells(line_idx, j)) = counts(gg%cells(line_idx, j)) + 1
                    end if
                else if (line_idx <= 2 * GRID_SIZE) then
                    if (gg%filled(j, line_idx - GRID_SIZE)) then
                        counts(gg%cells(j, line_idx - GRID_SIZE)) = counts(gg%cells(j, line_idx - GRID_SIZE)) + 1
                    end if
                else if (line_idx == 2 * GRID_SIZE + 1) then
                    if (gg%filled(j, j)) then
                        counts(gg%cells(j, j)) = counts(gg%cells(j, j)) + 1
                    end if
                else
                    if (gg%filled(j, GRID_SIZE - j + 1)) then
                        counts(gg%cells(j, GRID_SIZE - j + 1)) = counts(gg%cells(j, GRID_SIZE - j + 1)) + 1
                    end if
                end if
            end do
            
            max_count = maxval(counts)
            line_score = 0.0
            
            if (counts(value) >= 1) then
                line_score = line_score + real(counts(value)) * 5.0
                
                if (counts(value) == 1) then
                    line_score = line_score + 3.0
                else if (counts(value) == 2) then
                    line_score = line_score + 8.0
                else if (counts(value) == 3) then
                    line_score = line_score + 15.0
                else if (counts(value) >= 4) then
                    line_score = line_score + 25.0
                end if
            else
                if (max_count == 2 .or. max_count == 3) then
                    line_score = line_score + 2.0
                else
                    line_score = line_score - 1.0
                end if
            end if
            
            if (is_diagonal) then
                line_score = line_score * 1.5
            end if
            
            score = score + line_score
        end do
        
    end function evaluate_set_placement

end program strategy_set_builder