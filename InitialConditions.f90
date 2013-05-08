module InitialConditions
    implicit none

    integer, parameter :: N_bodies=2
    integer, dimension(N_bodies), parameter :: N3_bodies(/ 10, 4 /)
    real(8), dimension(N_bodies) :: z_offset_bodies(/100d0,-1d0/)


end module
