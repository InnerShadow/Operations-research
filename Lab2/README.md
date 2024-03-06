# Lab2

Find the data placement on computational stations in a distributed computing network based on clusters (see Figure 1). Conduct an analysis of constraints.

![Figure 1. Structure of a distributed computing network based on clusters](Img1.png)

In the global computer network, a distributed computing environment has been formed, consisting of $N$ high-performance workstations grouped into $M$ clusters. The data for processing is homogeneous, and the computational complexity depends only on their volume. The data is independent, and their individual arrays can be processed completely independently. The processing time for 1 MB of data on each workstation is known as $q_i$. The goal is to find the optimal distribution of the specified data volume for processing on the stations. Since workstations must also be used to solve other (local) tasks, it is necessary to minimize the overall loading time of all workstations. It is desirable for the processing results from different clusters to arrive simultaneously. Additionally, cluster owners may restrict both the volumes of information processed by their clusters and the volumes processed by individual workstations. The set of possible alternatives is determined by the data volume $s_i$ directed for processing on the $i$-th station.

Variable parameters include the vector of data volume values directed for processing on each station. Fixed independent parameters include processing times $a_i$ for 1 MB of data by the $i$-th station, the maximum allowable volumes of information that can be processed by the $i$-th station $P_i$, and the $j$-th cluster $R_j$, $j=1,2,...,M$; the volume of data to be processed $X$.

The objective is to minimize the total loading time of all stations.

Constraints: the total volume of processed data is equal to $X$, the volume of data processed by each $i$-th station is greater than or equal to 0 but less than or equal to $P_i$, the volume of data processed by each $j$-th cluster is less than or equal to $R_j$; processing times of data by clusters are equal.

Thus, the linear model can be expressed as the minimization of the total data processing time in the system:

$$\min{F(\vec{x})}=\sum_{i=1}^{N} q_ix_i$$

subject to the condition that the actual volume of data processed by the workstations equals the required: $\sum_{i=1}^{N} x_i=X$, and the load on each station is within the permissible range: $x_i\geq0,x_i\leq P_i, i=1,2,...,N$, and the total volume of data processed by the cluster does not exceed the specified value: $\sum_{i=1}^{m_1} x_i\leq R_1, \sum_{i=m_1+1}^{m_2} x_i\leq R_2, \sum_{i=m_{M-1}+1}^{N} x_i\leq R_M$, and the processing times are equal on all clusters: $\sum_{i=1}^{m_1} x_i =\sum_{i=m_1+1}^{m_2} x_i, \sum_{i=m_2+1}^{m_3} x_i =\sum_{i=1}^{m_1} x_i, \sum_{i=m_{M-1}+1}^{N} x_i =\sum_{i=1}^{m_1} x_i$.

Optimization problem for my variant:

$$\begin{cases}
      \min{F(x)=10x_1+4x_2+8x_3+6x_4+2x_5+3x_6+8x_7+2x_8+6x_9+6x_{10}}\\
      x_1+x_2+x_3+x_4\leq400\\
      x_5+x_6\leq800\\
      x_7+x_8+x_9+x_{10}\leq600\\
      \sum_{i=1}^{10} x_i=1000\\
      10x_1+4x_2+8x_3+6x_4-2x_5-3x_6=0\\
      x_1+x_2+x_3+x_4+x_7+x_8+x_9+x_{10}\geq700\\
      \forall{i}: 0\leq x_i\leq700
\end{cases}\,$$

1. This problem was solved using two R libraries, lpSolve and lpSolveAPI.
2. Subsequently, the problem was solved using lpSolveAPI without the criterion of simultaneous completion of all clusters in time.
3. The same problem was solved under the condition that one of the clusters failed.

The complete notebook with the solution is presented in .rmd format [here](/Lab2/Updated_Lab2Task.Rmd), the clean R script is available [here](/Lab2/La2_Rscript.R), and the compiled html report is accessible [here](/Lab2/Updated_Lab2Task.html).