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
        call calc_pressure(acceleratioins)
        call calc_viscosity(velocities,accelerations)
        call calc_surface()
        call calc_boundaries(positions,velocities, accelerations)
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
                    delWp(:, i, j) = -135 * (h - q)**2 / (pi * h**6) * (positions(:, i) - positions(:, j)) / q
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
            if (i==j) then 
                cycle
            end if
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

    subroutine calc_surface()

        

    end subroutine

    subroutine calc_boundaries(positions, velocities, a)
        
        real(8) :: a(:,:)
        real(8) :: normal(3,5), normal_k(3), d(5), d_k
        integer :: i,k
        real(8), intent(in):: positions(:,:), velocities(:, :)
        real(8) :: proj(3,N), test(N)

        normal(:,1) = [1,0,0]
        normal(:,2) = [-1,0,0]
        normal(:,3) = [0,1,0]
        normal(:,4) = [0,-1,0]
        normal(:,5) = [0,0,1]
        d=[init_size/2,init_size/2,init_size/2,init_size/2,0d0]

        
        do k =1,5
            normal_k = normal(:,k)
            d_k = d(k)
            proj = 0
            do i =1,3
                proj(i,:) = proj(i,:) + positions(i,:)*normal_k(i)
            end do
            test = sum(proj,dim=1)+d_k
            test = (test - abs(test)) / 2
            do i=1,3
                a(i,:) = a(i,:) - 0.05 * exp(-test)*velocities(i, :)
            end do
        end do

    end subroutine
    
end module
