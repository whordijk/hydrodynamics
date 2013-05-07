program HydroDynamics

    use Parameters
    use CreateWorld
    use TimeIntegrate
    use OutputData

    implicit none

    real(8) :: positions(3, N), velocities(3, N), accelerations(3, N)
    integer :: i

    call initiate_system(positions, velocities, accelerations)
    call set_output()
    do i = 1, T
        call update(positions, velocities, accelerations)
        call output_data(positions)
    end do

end program
