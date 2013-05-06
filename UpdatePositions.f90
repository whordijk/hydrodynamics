module UpdatePositions

    implicit none
    private

    public update_positions

contains

    subroutine update_positions(positions, velocities)

        real(8), intent(inout) :: positions(:, :), velocities(:, :)

    end subroutine

end module
