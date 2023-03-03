package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */
public class ForkJoinSolver
    extends SequentialSolver
{
    private static Set<Integer> globalVisited = new ConcurrentSkipListSet<>();
    private static Set<Integer> validStart = new ConcurrentSkipListSet<>();
    private static boolean finishedFlag = false;
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

    public ForkJoinSolver(Maze maze, int forkAfter, int start) {
        this(maze, forkAfter);
        this.start = start;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch()
    {
        // one player active on the maze at start
        int player = maze.newPlayer(start);
        // start with start node
        frontier.push(start);
        // as long as not all nodes have been processed
        while (!frontier.empty()) {
            if (finishedFlag) {
                return null;
            }
            // get the new node to process
            int current = frontier.pop();
            // if current node has a goal
            if (maze.hasGoal(current)) {
                // move player to goal
                maze.move(player, current);
                // search finished: reconstruct and return path
                finishedFlag = true;
                return pathFromTo(start, current);
            }
            // if current node has not been visited yet
            if (globalVisited.add(current)) {
                // move player to current node
                
                // mark node as visited
                //globalVisited.add(current);
                maze.move(player, current);
                // for every node nb adjacent to current
                for (int nb: maze.neighbors(current)) {
                    // add nb to the nodes to be processed
                    frontier.push(nb);
                    // if nb has not been already visited,
                    // nb can be reached from current (i.e., current is nb's predecessor)
                    if (!globalVisited.contains(nb))
                        predecessor.put(nb, current);
                }
            }

            List<Integer> forks = new ArrayList<>();
            for (int nb : frontier) {
                if (!globalVisited.contains(nb)) {
                    forks.add(nb);
                }
            }
            if (forks.size() > 1) {
                List<ForkJoinSolver> solvers = new ArrayList<>();
                
                for (int nb : forks) {
                    if (!validStart.add(nb)) {
                        break;
                    }
                    //validStart.add(nb);
                    ForkJoinSolver solver = new ForkJoinSolver(maze, forkAfter, nb);
                    solvers.add(solver);
                    solver.fork();
                }
                
                for (ForkJoinSolver solver : solvers) {
                    
                    List<Integer> solverPath = solver.join();
                    if (solverPath != null) {
                        List<Integer> result = pathFromTo(start, current);
                        result.addAll(solverPath);
                        return result;
                    }
                }
                return null;
            }
            
        }

        

        // all nodes explored, no goal found
        return null;
    }
}
