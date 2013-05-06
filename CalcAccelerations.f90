module CalcAccelerations
    implicit none
    private

    public calc_accelerations

    contains

    subroutine calc_accelerations(positions, distances, velocities)
        real(8), intent(in) :: positions(:,:), distances(:,:)
        real(8), intent(inout) :: velocities(:,:)


    end subroutine

end module
