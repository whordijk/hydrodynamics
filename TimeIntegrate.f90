module TimeIntegrate

    use FindNeighbors
    use Parameters

    implicit none
    private
    
    public update

    real(8) :: rho(N), rho_0, pressure(N), Wd(N, N), delWp(3, N, N), del2Wv(N, N), P(N, N), V(3, N, N)
    integer :: calc(2,N**2),pairs

contains

    subroutine update(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        velocities = velocities + accelerations * dt / 2
        positions = positions + velocities * dt !+ accelerations**2 * dt**2 / 2
        call update_accelerations(positions, velocities, accelerations)
        velocities = velocities + accelerations * dt / 2

    end subroutine

    subroutine update_accelerations(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        accelerations = 0
    
        call calc_weights(positions(:, :))
        call calc_density()
        call calc_viscosity(velocities,accelerations)
        call calc_pressure(accelerations)
        call calc_boundaries(positions,accelerations)
        !call calc_internal()

        accelerations(3, :) = accelerations(3, :) - 9.81d0
        
    end subroutine

    subroutine calc_weights(positions)

        real(8), intent(in) :: positions(:, :)
        real(8), parameter :: pi = 4 * atan(1d0)
        real(8) :: q 
        integer :: i, j

        pairs=1
        calc = 0

        do i = 1, N
            do j = i, N
                q = sqrt(sum((positions(:, i) - positions(:, j))**2))
                if (0 <= q .and. q <= h) then
                    Wd(i, j) = 315 * (h**2 - q**2)**3 / (64 * pi * h**9)
                    Wd(j, i) = Wd(i, j)
                    delWp(:, i, j) = -45 * (h - q)**3 / (pi * h**6) * (positions(:, i) - positions(:, j)) / q
                    delWp(:, j, i) = -delWp(:, i, j)
                    del2Wv(i, j) = 45 * (h - q) / (pi * h**6)
                    del2Wv(j, i) = del2Wv(i, j)

                    calc(1,pairs) = i
                    calc(2,pairs) = j
                    pairs = pairs+1
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
        rho = sum(Wd, dim=2)
    end subroutine
    
    subroutine calc_pressure(a)
        real(8) :: a(:,:)
        integer :: i, j, pair

        pressure = c_s**2 * (rho - set_density)
        do pair = 1, pairs
            i = calc(1,pair)
            j = calc(2,pair)
            P(i, j) = -(pressure(i) / rho(i)**2 + pressure(j) / rho(j)**2)
            P(j, i) = -(pressure(j) / rho(j)**2 + pressure(i) / rho(i)**2)
            a(:, i) = a(:, i) + P(i, j) * delWp(:, i, j)
            a(:, j) = a(:, j) + P(j, i) * delWp(:, j, i)
        end do

    end subroutine

    subroutine calc_viscosity(velocities,a)
        real(8) :: a(:,:)
        real(8), intent(in) :: velocities(:, :)
        integer :: i, j, pair

        do  pair = 1, pairs
            i = calc(1,pair)
            j = calc(2,pair)
            V(:, i, j) = mu * (velocities(:, j) - velocities(:, i)) / (rho(i) * rho(j))
            V(:, j, i) = mu * (velocities(:, i) - velocities(:, j)) / (rho(j) * rho(i))
        end do
        do i = 1, N
            do j = 1, N
                a(:, i) = a(:, i) + V(:, i, j) * del2Wv(i, j)
            end do
        end do

    end subroutine

    subroutine calc_internal()

    end subroutine

    subroutine calc_boundaries(positions,a)
        real(8) :: a(:,:)
        real(8) :: normal(3), d, test
        integer :: i
        real(8), intent(in):: positions(:,:)


       normal = [0,0,1]
       d=0

       do i=1,N
           test = sum(positions(:,i)*normal(:))-d
           if (test<0) then
               a(3,i) = a(3,i)+ exp(-test)
           end if
       end do

    end subroutine
    
end module
