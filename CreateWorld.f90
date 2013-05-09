module CreateWorld

    use Parameters

    implicit none
    private

    public initiate_system

contains

    subroutine initiate_system(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        positions = 20
        call read_positions(positions,1,4*N3_body**3)
        !call initiate_positions(positions,1,N3_body,-1d0)
        wall_body = positions(3,4*N3_body**3)/2
        call initiate_positions(positions, 4*N3_body**3+1, N3_drop, 40d0)
        call initiate_velocities(velocities)
        accelerations = 0
    
    end subroutine

    subroutine initiate_positions(x,start_index,m,z_offset)
        integer :: start_index
        real(8) :: z_offset, xy_offset
        real(8), dimension(3) :: r1, r2, r3, r4
        real(8), dimension(:,:) :: x
        integer :: m, i, j, k, z, q(3)
        real(8) :: a = (4/set_density)**(1d0/3)


        r1 = [ 0d0, 0d0, 0d0 ]
        r2 = [ 0d0, 0.5d0, 0.5d0 ]
        r3 = [ 0.5d0, 0d0, 0.5d0 ]
        r4 = [ 0.5d0, 0.5d0, 0d0 ]
        
        z = start_index
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

        xy_offset=x(3,z-1)/2

        x(3,start_index:z-1) = x(3,start_index:z-1) + z_offset
        x(2,start_index:z-1) = x(2,start_index:z-1) - xy_offset
        x(1,start_index:z-1) = x(1,start_index:z-1) - xy_offset


    end subroutine

    subroutine read_positions(x,start_index,N_in)
        real(8) :: x(:,:)
        integer :: start_index, N_in, i, j

        open(unit = 25, file=in_file,action='read')
        
        do i=1,N_in
            read (25, *) x(:,i)
            print*, i
        end do

        close(25)

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
