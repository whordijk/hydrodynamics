FC = gfortran
FFLAGS = -Wall -Wextra -O3 
LDLFLAGS =
LIBS = -llapack

COMPILE = $(FC) $(FFLAGS)
LINK = $(FC) $(LDFLAGS)

OBJS =
OBJS += Parameters.o
OBJS += CreateWorld.o
OBJS += TimeIntegrate.o
OBJS += OutputData.o
OBJS += HydroDynamics.o

all: HydroDynamics

HydroDynamics: $(OBJS)
	$(LINK) -o $@ $^ $(LIBS)
%.o: %.f90
	$(COMPILE) -o $@ -c $<
clean:
	$(RM) $(OBJS) *.mod
