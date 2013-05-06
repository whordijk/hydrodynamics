module CreateWorld

    use Parameters

    implicit none
    private

    public initiate_system

contains

    subroutine initiate_system(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
        integer :: i, j, k, m, z
        real(8) :: a

        m = 10
        a = 1d0
        z = 1
        do i = 1, m
            do j = 1, m
                do k = 1, m
                    positions(1, z) = (i - 1) * 1d0
                    positions(2, z) = (j - 1) * 1d0
                    positions(3, z) = (k - 1) * 1d0
                    z = z + 1
                end do
            end do
        end do
        velocities = 0
        accelerations = 0
    
    end subroutine

end module
