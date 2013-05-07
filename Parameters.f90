module Parameters

    implicit none

    integer, parameter :: N = 125
    integer, parameter :: T = 8000
    real(8), parameter :: total_mass = float(N)
    real(8), parameter :: set_density = 1
    real(8), parameter :: Temperature = .1
    real(8), parameter :: dt = 1d-3
    real(8), parameter :: c_s = 10, mu = 10
    real(8), parameter :: h = 1*(4/set_density)**(1d0/3)
    character(len=*), parameter :: filename="out_positions.txt"

end module
