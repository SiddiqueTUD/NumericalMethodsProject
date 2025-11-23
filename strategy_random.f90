program strategy_random
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
        seed(i) = seed(1) + i * 111222333
    end do
    call random_seed(put=seed)
    deallocate(seed)
    
    print *, "=========================================="
    print *, "  STRATEGY 4: RANDOM (Baseline)          "
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
        
        call apply_random_strategy(gg, rolls)
        
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

    subroutine apply_random_strategy(gg, roll_values)
        type(GameGrid), intent(inout) :: gg
        integer, intent(in) :: roll_values(NUM_ROLLS)
        integer :: roll_idx, row, col
        integer :: empty_cells(NUM_ROLLS, 2)
        integer :: num_empty, choice
        real :: rand_val
        
        do roll_idx = 1, NUM_ROLLS
            num_empty = 0
            do row = 1, GRID_SIZE
                do col = 1, GRID_SIZE
                    if (is_empty(gg, row, col)) then
                        num_empty = num_empty + 1
                        empty_cells(num_empty, 1) = row
                        empty_cells(num_empty, 2) = col
                    end if
                end do
            end do
            
            call random_number(rand_val)
            choice = int(rand_val * num_empty) + 1
            if (choice > num_empty) choice = num_empty
            
            call place_value(gg, empty_cells(choice, 1), empty_cells(choice, 2), roll_values(roll_idx))
        end do
    end subroutine apply_random_strategy

end program strategy_random