# Lab 3

## [**Code**](/Lab3/Lab3_RScript.R)

## Task
Let's consider a telecommunication network consisting of *p* router nodes *R* and *q* connecting communication lines *P*. Each communication line is characterized by signal delay time measured in milliseconds: 

\[W=\{w_1,w_2,...,w_q\}\]

The routers have hot redundancy multiplicity: 

\[SR=\{sr_1,sr_2,...,sr_p\},| 1$\leq$ sr_i$\leq$ 5\]

The communication lines have duplication multiplicity: 

\[SP=\{sp_1,sp_2,...,sp_q\},| 1$\leq$ sp_i$\leq$ 5\]

For highly critical applications, such as military technology, a reserve channel for combat control is mandatory, and the data transmission rate significantly affects the system's response as a whole.

It is required to find two (in general, *r* can be considered) reserved channels from node *k* to node *l*, not involving jointly unduplicated routers and communication lines, ensuring a collectively minimal delay time.

## Solutions steps
1. Find all $N$ non-loop routes $Rt$ from node $k$ to node $l$. This can be done manually. For a high grade, it is necessary to develop and implement an algorithm that constructs such routes. The use of a library function is permitted.

2. Number all possible routes from $1$ to $n$ and associate each of them with a corresponding delay time $t_i$, equal to the sum of times over all edges of route $i$.

3. Introduce binary variables $x_i$, $i=1,...,n$ as variables. Here $x_i=1$ if route number $i$ is selected and $x_i=0$ otherwise. Then the objective function to be minimized will be: $F(\vec{x})=\sum_{i=1}^{N}t_ix_i\\$ Since it is necessary to find $r$ independent channels, the first constraint takes the form (in the simplest case for dual redundancy $r=2$): $\sum_{i=1}^{N}x_i=r$

4. Next, it is required to construct resource constraints for routes. The columns of the constraint matrix will be routes, and the rows will be the involved routers and communication lines. Therefore, for all routes that jointly use a router (communication line), the constraint can be written as: $\sum_{i\in V(Rt)} x_i \leq SR$.
$\sum_{i\in E(Rt)} x_i \leq SP$$

   In graph theory, the set of vertices of a graph $g$ is denoted as $V(g)$, and the set of its edges as $E(g)$.

5. Solve the integer linear programming problem.

6. Construct a graph with the identified routes highlighted.

## Solution

1. Find all non-loop routes usin R all_simple_paths function.
2. Define calculate_time_delay function to calculate time delay of each route. Save this values in *time_delays*.
3. Create limmitations for simplex method with binary varibals.
4. Solve task:
   ```{r}
    optimum <- lp(
        direction = "min",
        objective.in = Fun,
        const.mat = A,
        const.dir = CD,
        const.rhs = B,
        all.bin = TRUE
        )
   ```
5. Show find routes on graph plot.

![](/Lab3/Lab3Task_files/figure-html/unnamed-chunk-25-2.png)
![](/Lab3/Lab3Task_files/figure-html/unnamed-chunk-25-3.png)
![](/Lab3/Lab3Task_files/figure-html/unnamed-chunk-25-4.png)
![](/Lab3/Lab3Task_files/figure-html/unnamed-chunk-25-5.png)