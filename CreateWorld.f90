module CreateWorld

    use Parameters

    implicit none
    private

    public initiate_system

contains

    subroutine initiate_system(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        call initiate_positions(positions)
        !call initiate_velocities(velocities)
        velocities = 0
        accelerations = 0
    
    end subroutine

    subroutine initiate_positions(x)
        real(8), dimension(3) :: r1, r2, r3, r4
        real(8), dimension(3, N) :: x
        integer :: m, i, j, k, z, q(3)
        real(8) :: a = (4/set_density)**(1d0/3)

        m = nint(init_size/a)

        r1 = [ 0d0, 0d0, 0d0 ]
        r2 = [ 0d0, 0.5d0, 0.5d0 ]
        r3 = [ 0.5d0, 0d0, 0.5d0 ]
        r4 = [ 0.5d0, 0.5d0, 0d0 ]
        
        z = 1
        do i = 1, m
            do j = 1, m
                do k = 1, m
                    q = (/ i - 1, j - 1, k - 1 /)
                    x(:, z) = q * a + r1 * a
                    x(:, z + 1) = q * a + r2 * a
                    x(:, z + 2) = q * a + r3 * a
                    x(:, z + 3) = q * a + r4 * a
                    z = z + 4
                end do
            end do
        end do

        x(3,:) = x(3,:) + 15
        x(2,:) = x(2,:) - init_size/2
        x(1,:) = x(1,:) - init_size/2


    end subroutine

    subroutine initiate_velocities(p)
        real(8), intent(out) :: p(:,:) 
        real(8), parameter :: pi = 4 * atan(1d0)
        real(8), dimension(3) :: psum
        real(8), dimension(3, N) :: u1, u2
        real(8) :: beta
        
        p = 0d0
        
        call init_random_seed()
        call random_number(u1)
        call random_number(u2)

        p = sqrt(-2d0 * log(u1)) * cos(2*pi*u2)
        beta = sqrt((3 * (N - 1) * Temperature) / (sum(p**2)))
        psum = sum(p, dim = 2)
        p(1,:) = p(1,:) - 1d0 / N * psum(1)
        p(2,:) = p(2,:) - 1d0 / N * psum(2)
        p(3,:) = p(3,:) - 1d0 / N * psum(3)
        p = beta * p 

    end subroutine

    subroutine init_random_seed()

        integer :: i, n, clock
        integer, dimension(:), allocatable :: seed

        call random_seed(size = n)
        allocate(seed(n))

        call system_clock(count=clock)

        seed = clock + 37 * (/ (i-1, i = 1, n) /)
        call random_seed(put = seed)

        deallocate(seed)

    end subroutine

end module
