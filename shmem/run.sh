NUM_PROC="8"

if [ "$1" = "gather" -o "$1" = "shmem" ]; then
    MODE=$1
else
    echo "Usage: bash run.sh [gather|shmem]"
    exit
fi

FN="mpi${MODE}"
mpif90 ${FN}.f90 -o ${FN}
mpiexec -np ${NUM_PROC} ${FN}
