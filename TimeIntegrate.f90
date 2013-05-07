module TimeIntegrate

    use FindNeighbors
    use Parameters

    implicit none
    private
    
    public update

    real(8) :: rho(N), rho_0, pressure(N), Wd(N, N), delWp(3, N, N), del2Wv(N, N), P(N, N), V(3, N, N)
    real(8) :: a_pressure(3, N), a_viscosity(3, N), a_internal(3, N)

contains

    subroutine update(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        positions = positions + velocities * dt + accelerations**2 * dt**2 / 2
        velocities = velocities + accelerations * dt / 2
        call update_accelerations(positions, velocities, accelerations)
        velocities = velocities + accelerations * dt / 2

    end subroutine

    subroutine update_accelerations(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
    
        call calc_weights(positions(:, :))
        call calc_density()
        call calc_pressure()
        call calc_viscosity(velocities)
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
            do j = i, N
                q = sqrt(sum((positions(:, i) - positions(:, j))**2))
                if (0 <= q .and. q <= 1) then
                    Wd(i, j) = 315 * (1 - q**2)**3 / (64 * pi)
                    Wd(j, i) = Wd(i, j)
                    delWp(:, i, j) = 45 * (1 - q)**3 / pi * (positions(:, i) - positions(:, j)) / q
                    delWp(:, j, i) = -delWp(:, i, j)
                    del2Wv(i, j) = 45 * (1 - q) / pi
                    del2Wv(j, i) = del2Wv(i, j)
                else
                    Wd(i, j) = 0
                    Wd(j, i) = 0
                    delWp(:, i, j) = 0
                    delWp(:, j, i) = 0
                    del2Wv(i, j) = 0
                    del2Wv(j, i) = 0
                end if
            end do
            delWp(:, i, i) = 0
        end do

    end subroutine

    subroutine calc_density()

        integer :: i

        do i = 1, N
            rho(i) = sum(Wd(i, :))
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
                a_pressure(:, i) = a_pressure(:, i) + P(i, j) * delWp(:, i, j)
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
                a_viscosity(:, i) = a_viscosity(:, i) + V(:, i, j) * del2Wv(i, j)
            end do
        end do

    end subroutine

    subroutine calc_internal()

    end subroutine
    
end module
