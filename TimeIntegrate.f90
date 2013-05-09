module TimeIntegrate

    use Parameters

    implicit none
    private
    
    public update

    real(8) :: rho(N), rho_0, pressure(N), Wd(N, N), delWd(3, N, N), del2Wd(N, N)
    real(8) :: delWp(3, N, N), del2Wv(N, N), P(N, N), V(3, N, N)
    integer :: calc(2, N**2), pairs

contains

    subroutine update(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        velocities = velocities + accelerations * dt / 2
        positions = positions + velocities * dt
        call update_accelerations(positions, velocities, accelerations)
        velocities = velocities + accelerations * dt / 2

    end subroutine

    subroutine update_accelerations(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        accelerations = 0
    
        call calc_weights(positions(:, :))
        rho = sum(Wd, dim=2)
        call calc_pressure(accelerations)
        call calc_viscosity(velocities,accelerations)
        call calc_surface(accelerations)
        call calc_boundaries(positions,velocities, accelerations)

        accelerations(3, :) = accelerations(3, :) - 9.81d0
        
    end subroutine

    subroutine calc_weights(positions)

        real(8), intent(in) :: positions(:, :)
        real(8), parameter :: pi = 4 * atan(1d0)
        real(8) :: q 
        integer :: i, j

        pairs= 0 
        calc = 0

        do i = 1, N
            do j = i, N
                q = sqrt(sum((positions(:, i) - positions(:, j))**2))
                if (0 <= q .and. q <= h) then
                    Wd(i, j) = 315 * (h**2 - q**2)**3 / (64 * pi * h**9)
                    delWd(:, i, j) = -945 * (h**2 - q**2)**2 * (positions(:, j) - positions(:, i)) / (32 * pi * h**9)
                    del2Wd(i, j) = -945 / (32 * pi * h**9) * (h**2 - q**2) &
                        * (3 * h**2 - 7 * q**2)
                    delWp(:, i, j) = -45 * (h - q)**2 / (pi * h**6) * (positions(:, j) - positions(:, i)) / q
                    delWp(:, j, i) = -delWp(:, i, j)
                    del2Wv(i, j) = 45 * (h - q) / (pi * h**6)

                    pairs = pairs + 1
                    calc(1, pairs) = i
                    calc(2, pairs) = j
                else
                    Wd(i, j) = 0
                    delWd(:, i, j) = 0
                    del2Wd(i, j) = 0
                    delWp(:, i, j) = 0
                    del2Wv(i, j) = 0
                end if
                Wd(j, i) = Wd(i, j)
                delWd(:, j, i) = -delWd(:, i, j)
                del2Wd(j, i) = del2Wd(i, j)
                delWp(:, j, i) = -delWp(:, i, j)
                del2Wv(j, i) = del2Wv(i, j)
            end do
            delWp(:, i, i) = 0
        end do

    end subroutine

    subroutine calc_density()
    end subroutine
    
    subroutine calc_pressure(a)
        real(8) :: a(:,:)
        integer :: i, j, pair

        pressure = c_s**2 * (rho - set_density)
        do pair = 1, pairs
            i = calc(1,pair)
            j = calc(2,pair)
            if (i==j) then 
                cycle
            end if
            P(i, j) = -mass * (pressure(i) / rho(i)**2 + pressure(j) / rho(j)**2)
            P(j, i) = -mass * (pressure(j) / rho(j)**2 + pressure(i) / rho(i)**2)
            a(:, i) = a(:, i) + P(i, j) * delWp(:, i, j)
            a(:, j) = a(:, j) + P(j, i) * delWp(:, j, i)
        end do

    end subroutine

    subroutine calc_viscosity(velocities,a)
        real(8) :: a(:,:)
        real(8), intent(in) :: velocities(:, :)
        integer :: i, j, pair
        real(8) :: V(3)

        V=0
        do  pair = 1, pairs
            i = calc(1,pair)
            j = calc(2,pair)
            V= mu * mass*(velocities(:, j) - velocities(:, i)) / (rho(i) * rho(j))
            a(:, i) = a(:,i) + V*del2WV(i,j)
            a(:, j) = a(:,j) - V*del2WV(j,i)
        end do

    end subroutine

    subroutine calc_surface(a)

        real(8) :: normal(3, N), absnorm(N), K(N)
        real(8), intent(inout) :: a(:, :)
        integer :: i, j

        do i = 1, N

            do j = 1, 3
                normal(j, i) = sum(mass * delWd(j, i, :) / rho)
            end do            
            absnorm(i) = sqrt(sum(normal(:, i)**2))
            K(i) = -sum(del2Wd(i, :) / rho) / absnorm(i)
            a(:, i) = a(:, i) + sigma * K(i) * normal(:, i) / absnorm(i)

        end do

    end subroutine

    subroutine calc_boundaries(positions, velocities, a)
        
        real(8) :: a(:,:)
        real(8) :: normal(3,5), normal_k(3), d(5), d_k
        integer :: i,k
        real(8), intent(in):: positions(:,:), velocities(:, :)
        real(8) :: proj(3,N), test(N)

        normal(:,1) = [1, 0, 0]
        normal(:,2) = [-1, 0, 0]
        normal(:,3) = [0, 1, 0]
        normal(:,4) = [0, -1, 0]
        normal(:,5) = [0, 0, 1]
        !d = [wall_body, wall_body, wall_body, wall_body, 0d0]
        d = [init_size / 2, init_size / 2, init_size / 2, init_size / 2, 0d0]

        
        do k =1,5
            normal_k = normal(:,k)
            d_k = d(k)
            proj = 0
            do i =1, 3
                proj(i,:) = proj(i,:) + positions(i,:) * normal_k(i)
            end do
            test = sum(proj, dim=1) + d_k
            test = (test - abs(test)) / 2
            do i=1,3
                !a(i,:) = a(i,:) - f * exp(-test) * velocities(i, :) / sqrt(sum(velocities(i, :)**2))
                a(i, :) = a(i, :) + exp(-test) * normal(i, k)
            end do
        end do

    end subroutine
    
end module
