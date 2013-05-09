module Parameters

    implicit none
    
    integer, parameter :: N3_body = 4
    integer, parameter :: N3_drop = 0
    integer, parameter :: N = 4 * (N3_body**3 + N3_drop**3)
    integer, parameter :: T = 3000
    real(8), parameter :: total_mass = 1
    real(8), parameter :: set_density = 999.29
    real(8), parameter :: total_volume = total_mass / set_density
    real(8), parameter :: mass = set_density * total_volume / N
    real(8), parameter :: dt = 1d-2
    real(8), parameter :: c_s = 100, mu = 1, sigma = 1, f = 1
    real(8), parameter :: init_size = (total_mass / (mass * set_density))**(1d0 / 3)
    real(8), parameter :: h = 2 * (4 / set_density)**(1d0 / 3)
    real(8) :: wall_body

    character(len=*), parameter :: out_file="out_positions.txt"
    character(len=*), parameter :: in_file="in_positions.txt"

end module
