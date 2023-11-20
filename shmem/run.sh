set -xe

for MODE in gather shmem; do
    FN="mpi${MODE}"
    mpif90 ${FN}.f90 -o ${FN}
    nsys profile -t mpi -o ${FN} mpiexec -np 8 ${FN}
done
