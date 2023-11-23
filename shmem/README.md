### Introduction ###

MPI 3.0 introduced a new API shared memory (SHM) model to accelerate the communication between processes on the shared-memory nodes.

MPI SHM takes advantage for

- Sharing replicated data to several processes by shared memory could save memory usage
- Communication between processes by shared memory costs lower on shared memory nodes.

Also, there are some issues to address

- Reduction in available physical ram to the demand-paging system
- Programmers have to address data race condition

### Steps ###

MPI programs are able to be run on a multi-node system, but only the processes on a same node can communicate by shared memory.

Hence, in addition to the default communication, [`MPI_Comm_split_type`](mpishmem.f90#L22) provides a smaller communication among the processes in the same node. (This is unnecessary if your MPI is run on a single node).

Then, the process with rank 0 (new communication for shared memory) calls [`MPI_Win_allocate_shared`](mpishmem.f90#L37) for shared memory allocation.

Other processes call [`MPI_Win_shared_query`](mpishmem.f90#L39) to retrieve the pointer of shared memory.

The shared memory buffer is ready for all processes to read and modify.

In order to access the shared memory in a safe way, call [`MPI_Win_fence`](mpishmem.f90#L57) for synchronization.

[mpishmem.f90](mpishmem.f90) is a simple example for MPI shared memory use. 

### Benchmark ###

In [mpishmem.f90](mpishmem.f90), each process writes `bufcount` double data into shared memory and synchronize, and then the root process (rank 0) will validate the data correctness among `bufcount x nproc` elements.

Follow the steps below, you will have

1. Make sure you have mpi install in your environment (mpif90 and mpiexec)

2. `bash run.sh shmem`

```sh
Elapsed time:    0.1929090 s
```

in stdout.

There are 8 processes and each process copies 10,000 double to root process.

The elapsed time includes 512 benchmark steps.

Hence, one step takes 0.192909 / 512 = 0.377ms

To compare with `MPI_Gather`, you can run `bash run.sh gather` to do benchmark on **Gather** API.

```sh
Elapsed time:    0.5812940 s
```

The elapsed time is  0.5812940s, so one step takes 0.581294 / 512 = 1.135ms

In our example, the shared memory outperformed gather by 3x.
