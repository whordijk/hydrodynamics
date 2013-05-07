module TimeIntegrate

    use FindNeighbors
    use Parameters

    implicit none
    private
    
    public update

    real(8) :: rho(N), rho_0, pressure(N), W(N, N), delW(3, N, N), del2W(N, N), P(N, N), V(3, N, N)
    real(8) :: a_pressure(3, N), a_viscosity(3, N), a_internal(3, N)

contains

    subroutine update(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        positions = positions + velocities * dt + accelerations**2 * dt**2 / 2
        velocities = velocities + accelerations * dt / 2
        call update_accelerations(positions, velocities, accelerations)
        velocities = velocities + accelerations * dt / 2

    end subroutine

    subroutine update_positions(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

    end subroutine

    subroutine update_velocities(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
    
    end subroutine

    subroutine update_accelerations(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
    
        call calc_weights(positions(:, :))
        call calc_density()
        call calc_pressure()
        !call calc_viscosity(velocities)
        !call calc_internal()

        accelerations = a_pressure
        accelerations(3, :) = accelerations(3, :) - 9.81d0
        
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
                    del2W(i, j) = 6 * ((2 - q) - 4 * (1 - q)) / (4 * pi) * sum((positions(:, i) - positions(:, j)**2)) &
                        + 36 * ((2 - q)**2 - 4 * (1 - q)**2) / (4 * pi)
                else if (1 < q .and. q <= 2) then
                    W(i, j) = (2 - q)**3 /  (4 * pi)
                    delW(:, i, j) = 6 * (2 - q)**2 / (4 * pi) * (positions(:, i) - positions(:, j))
                    del2W(i, j) = 1
                else
                    W(i, j) = 0
                    delW(:, i, j) = 0
                    del2W(i, j) = 0
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
        a_pressure = 0
        do i = 1, N
            do j = 1, N
                a_pressure(:, i) = a_pressure(:, i) + P(i, j) * delW(:, i, j)
            end do
        end do

    end subroutine

    subroutine calc_viscosity(velocities)

        real(8), intent(in) :: velocities(:, :)
        integer :: i, j

        do  i = 1, N
            do j = 1, N
                V(:, i, j) = mu * (velocities(:, j) - velocities(:, i)) / (rho(i) * rho(j))
            end do
        end do
        a_viscosity = 0
        do i = 1, N
            do j = 1, N
                a_viscosity(:, i) = a_viscosity(:, i) + V(:, i, j) * del2W(i, j)
            end do
        end do

    end subroutine

    subroutine calc_internal()

    end subroutine
    
end module
