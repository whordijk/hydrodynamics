module Parameters

    implicit none

    integer, parameter :: N = 4*4**3
    integer, parameter :: T = 8000
    real(8), parameter :: mass = 1
    real(8), parameter :: total_mass = N * mass
    real(8), parameter :: set_density = 1d0
    real(8), parameter :: Temperature = .1
    real(8), parameter :: dt = 1d-3
    real(8), parameter :: c_s = 10, mu = 1, sigma = 0.1, f = 0.01
    real(8) :: init_size = (total_mass/(mass * set_density))**(1d0/3)
    real(8), parameter :: h = (4/set_density)**(1d0/3)
    character(len=*), parameter :: filename="out_positions.txt"

end module
