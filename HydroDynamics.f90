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
        if (mod(i,20)==1) then
            call output_continuous(positions)
        end if
    end do

    !call output_end(positions)
    close(24)

end program
