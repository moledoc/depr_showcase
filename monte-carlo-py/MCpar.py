
from mpi4py import MPI
from mpi4py.MPI import ANY_SOURCE

import numpy as np
import time
import sys

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

n = int(sys.argv[1])

def f(x):
    return(x**2 + x**4+ np.sin(x) + np.cos(x) +x**25)

analytical = 613/390 + np.sin(1) - np.cos(1)


def MCpar(n):
    if n%size!=0:
        print("Nr of iterations is not divisible by nr of process")
        exit()
    ni = int(n//size)
    pyunif = np.random.uniform(0,1,ni)
    # TODO: explain multiplying with size
    EXi = np.mean(f(pyunif))/size
    if rank == 0:
        EX = np.empty(1)
    else:
        EX = None
    comm.Reduce(EXi,EX,op=MPI.SUM,root=0)
    if rank == 0:
        EX = EX[0]
        error = np.abs(EX-analytical)
        return(EX,error)
    else:
        return (None,None)

start = time.time()
ex,error = MCpar(n)
end = time.time()
if rank == 0:
    print(n,ex,error,"MCpar",str(end-start),str(size),sep=",")
