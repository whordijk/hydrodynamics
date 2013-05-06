module TimeIntegrate

    implicit none
    private

    public update

contains

    subroutine update(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

        call update_positions(positions, velocities, accelerations)
        call update_velocities(positions, velocities, accelerations)
        call update_accelerations(positions, velocities, accelerations)
        call update_velocities(positions, velocities, accelerations)

    end subroutine

    subroutine update_positions(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)

    end subroutine

    subroutine update_velocities(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
    
    end subroutine

    subroutine update_accelerations(positions, velocities, accelerations)

        real(8), intent(inout) :: positions(:, :), velocities(:, :), accelerations(:, :)
    
    end subroutine

end module
