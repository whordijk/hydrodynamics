module TimeIntegrate

    use FindNeighbors
    use Parameters

    implicit none
    private
    
    public update

    real(8) :: rho(N), rho_0, pressure(N), W(N, N), delW(3, N, N), P(N, N)
    real(8) :: a_pressure(3, N), a_viscosity(3, N), a_internal(3, N), a_gravity(3, N)

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

        positions = positions + 1d0/2*.1*a_pressure
        print*, a_pressure

    end subroutine

    subroutine update_velocities(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
    
    end subroutine

    subroutine update_accelerations(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
    
        call calc_weights(positions(:, :))
        call calc_density()
        call calc_pressure()
        
    end subroutine

    subroutine calc_weights(positions)

        real(8), intent(in) :: positions(:, :)
        real(8), parameter :: pi = 4 * atan(1d0)
        real(8) :: q 
        integer :: i, j

        do i = 1, N
            do j = 1, N
                q = sqrt(sum((positions(:, i) - positions(:, j))**2))
                if (0 <= q .and. q <= 1) then
                    W(i, j) = ((2 - q)**3 - 4 * (1 - q)**3) / (4 * pi)
                    delW(:, i, j) = 6 * ((2 - q)**2 - 4 * (1 - q)**2) / (4 * pi) * (positions(:, i) - positions(:, j))
                else if (1 < q .and. q <= 2) then
                    W(i, j) = (2 - q)**3 /  (4 * pi)
                    delW(:, i, j) = 6 * (2 - q)**2 / (4 * pi) * (positions(:, i) - positions(:, j))
                else
                    W(i, j) = 0
                    delW(:, i, j) = 0
                end if
            end do

        end do

    end subroutine

    subroutine calc_density()

        integer :: i

        do i = 1, N
            rho(i) = sum(W(i, :))
        end do
        rho_0 = sum(rho) / size(rho)

    end subroutine
    
    subroutine calc_pressure()

        integer :: i, j

        pressure = c_s**2 * (rho - rho_0)
        do i = 1, N
            do j = 1, N
                P(i, j) = -(pressure(i) / rho(i)**2 + pressure(j) / rho(j)**2)
            end do
        end do
        do i = 1, N
            a_pressure(:, i) = sum(P(i, j) * delW(:, i, j))
            ! sum over j above
        end do

    end subroutine

end module
