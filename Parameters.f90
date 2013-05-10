module Parameters

    implicit none
    
    integer, parameter :: N3_body = 6
    integer, parameter :: N3_drop = 0
    integer, parameter :: N = 4 * (N3_body**3 + N3_drop**3)
    integer, parameter :: T = 5000
    real(8), parameter :: total_mass = 0.01
    real(8), parameter :: rho_0 = 999.29
    real(8), parameter :: total_volume = total_mass / rho_0
    real(8), parameter :: mass = total_mass / N
    real(8), parameter :: dt = 1d-4
    real(8), parameter :: c_s = 2, mu = 1, sigma = 1, f = 0.1
    real(8), parameter :: init_size = total_volume**(1d0 / 3)
    real(8), parameter :: h = 1.5 * (4 * total_volume / N)**(1d0 / 3)
    real(8), parameter :: pi = 4 * atan(1d0)
    real(8) :: wall_body

    character(len=*), parameter :: out_file="out_positions.txt"
    character(len=*), parameter :: in_file="in_positions.txt"

end module
