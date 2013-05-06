module TimeIntegrate

    use FindNeighbors
    use Parameters

    implicit none
    private
    
    public update

    real(8) :: rho(N), m(N), W(N, N)

contains

    subroutine update(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        call update_positions(positions, velocities, accelerations)
        call update_velocities(positions, velocities, accelerations)
        call update_accelerations(positions, velocities, accelerations)
        call update_velocities(positions, velocities, accelerations)

    end subroutine

    subroutine update_positions(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

    end subroutine

    subroutine update_velocities(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
    
    end subroutine

    subroutine update_accelerations(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
    
        call calc_density(positions(:, :))
        

    end subroutine

    subroutine calc_weights(positions)

        real(8), intent(in) :: positions(:, :)
        real(8), parameter :: pi = 4 * atan(1d0)
        integer :: i, j

        do i = 1, N
            do j = 1, N
                W(i, j) = exp(sum((positions(:, i) - positions(:, j))**2) / 2) / (sqrt(2 * pi))
            end do
        end do

    end subroutine

    subroutine calc_density(positions)

        real(8), intent(in) :: positions(:, :)
        integer :: i

        do i = 1, N
            rho(i) = sum(W(i, :))
        end do
        print *, rho

    end subroutine

end module
