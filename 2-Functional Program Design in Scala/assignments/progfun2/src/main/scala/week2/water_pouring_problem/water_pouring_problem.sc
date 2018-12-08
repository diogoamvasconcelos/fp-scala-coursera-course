/*
Water Pouring Problem
having glasses of arbitrary capacity, you need to fill a certain glass with a specific quantity, but the glasses have no measuring scale.
You can only fill a glass, empty a glass, or pour from a glass to another (until source is empty or destination is full)

Presenting States and Actions
Glass: Int (quantity)
State: Vector[Int] (one element per glass)
Moves: Empty(glass), Fill(glass), Pour(from, to)

Bruteforce approach, branching from each new option by trying all the options, generating the "moves" or "paths" until desired state is reached (or no more new states possible
, in a breath first search
Some actions can return to already visited states



 */

// ----------------- POURING ------------------

class Pouring(capacity: Vector[Int])
{
    // States
    type State = Vector[Int]

    val initialState = capacity map (x => 0) //vectors of all 0s

    // Moves  (make it a case class for convenience

    trait Move {
        def change(state: State): State
    }

    case class Empty(glass: Int) extends Move {
        override def change(state: State): State = state.updated(glass, 0)
    }

    case class Fill(glass: Int) extends Move {
        override def change(state: State): State = state.updated(glass, capacity(glass))
    }
    case class Pour(from: Int, to: Int) extends Move {
        override def change(state: State): State = {
            val amount = state(from) min (capacity(to) - state(to))
            state updated (from, state(from) - amount) updated (to, state(to) + amount)
        }

    }

    val glasses = 0 until capacity.length

    val moves = //define all possible moves
        (for (g <- glasses) yield Empty(g)) ++
            (for (g <- glasses) yield Fill(g)) ++
            (for (g1 <- glasses; g2 <- glasses if g1 != g2) yield Pour(g1, g2))

    // Paths

    class Path(history: List[Move]) {  //List is reverse. Last move in a path comes first

        /*
        def endState: State = trackState(history)

        private def trackState(xs: List[Move]): State = xs match {
            case Nil => initialState
            case move :: xs1 => move.change(trackState(xs1)) //works because the list is reversed
        }
        */

        //this looks like a fold right, so we will replace it
        def endState: State = (history foldRight initialState) (_ change _)  //this gets called a lot, could be optimized (slow for long histories)

        def extend(move: Move) = new Path(move :: history)

        override def toString: String = history.reverse mkString(" ") + "--> " + endState
    }

    val initialPath = new Path(Nil)

    def from(paths: Set[Path], explored: Set[State]) : Stream[Set[Path]] =
        if (paths.isEmpty) Stream.empty
        else {
            val more = for {
                path <- paths
                next <- moves map path.extend //for each of the possible moves, we create a new path that extends for each move
                if !(explored contains next.endState) //filter states we already have been/explored. ambiguous!
            } yield next

            paths #:: from(more, explored ++ (more map (_.endState))) //build with the other paths, adding more and more
        }

    val pathSets = from(Set(initialPath), Set(initialState))   //stream list of all the possible paths

    // Solution
    //go through all the Path sets and from each pick the paths that are solutions (have the correct target state)
    //and concatenate all the results in another stream (order by their length)
    def solutions(target: Int): Stream[Path] =
        for {
            pathSet <- pathSets
            path <- pathSet
            if path.endState contains target
        } yield path
}



//  -------------- TEST -------------------------
val problem = new Pouring(Vector(4, 9))
problem.moves.length

val test_paths = problem.pathSets

test_paths.take(3).toList

problem.solutions(6 )


