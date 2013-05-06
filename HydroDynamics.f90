program HydroDynamics

    use CreateWorld

    implicit none

    integer, parameter  :: T = 10
    integer, parameter :: N = 8
    real(8) :: positions(3, N), velocities(3, N), distances(3, N**2)
    integer :: i

    !allocate(positions(3, N), velocities(3, N), distances(3, N**2 - N))

    call initiate_system(positions, velocities)
    do i = 1, T
        !call find_neighbors(positions, distances)
        !call calc_accelerations(positions, distances, velocities)
        !call update_positions(positions, velocities)
        !call output_data(positions)
    end do

end program
