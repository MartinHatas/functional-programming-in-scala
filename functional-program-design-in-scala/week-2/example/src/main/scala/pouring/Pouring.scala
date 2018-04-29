package pouring

import scala.collection.immutable

class Pouring(capacity: Vector[Int]) {

  type State = Vector[Int]

  val initState: Vector[Int] = capacity map (x => 0)

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
      state updated(from, state(from) - amount) updated(to, state(to) + amount)
    }
  }

  val glasses: Range = capacity.indices

  val moves: immutable.IndexedSeq[Move] =
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
        (for (g1 <- glasses; g2 <- glasses; if g1 != g2) yield Pour(g1, g2))


  class Path(history: List[Move]) {

    def extend(move: Move): Path = new Path(move :: history)

    def endState: State = (history foldRight initState)((move, state) => move.change(state))

    override def toString: String = (history.reverse.mkString(" ")) + s" --> $endState"
  }

  val initialPath = new Path(Nil)

  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
      if (paths.isEmpty) Stream.empty
      else {
        val more = for {
          path <- paths
          next <- moves map path.extend
          if !explored.contains(next.endState)
        } yield next
        more #:: from(more, explored ++ more.map(_.endState))
      }
  }

  val pathSets = from(Set(initialPath), Set())

  def solution(target: Int): Stream[Path] = {
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState.contains(target)
    } yield path
  }
}
