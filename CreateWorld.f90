module CreateWorld

    use Parameters

    implicit none
    private

    public initiate_system

contains

    subroutine initiate_system(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        positions = 20
        !call read_positions(positions,1,4*N3_body**3)
        call initiate_positions(positions,1,N3_body,0d0)
        wall_body = positions(3,4*N3_body**3)/2
        call initiate_positions(positions, 4*N3_body**3+1, N3_drop, 40d0)
        !call initiate_velocities(velocities)
        velocities = 0
        accelerations = 0
    
    end subroutine

    subroutine initiate_positions(x,start_index,m,z_offset)
        integer :: start_index
        real(8) :: z_offset, xy_offset
        real(8), dimension(3) :: r1, r2, r3, r4
        real(8), dimension(:,:) :: x
        integer :: m, i, j, k, z, q(3)
        real(8) :: a = (4 * mass / rho_0)**(1d0/3)


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

end module
