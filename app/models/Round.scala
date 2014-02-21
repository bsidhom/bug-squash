package models

import scala.language.higherKinds

/**
 * @author bsidhom
 */
object Round {
  import scalax.collection.Graph
  import scalax.collection.GraphPredef._
  import scalax.collection.GraphEdge._
  val g: Graph[Symbol, UnDiEdge] = Graph('A ~ 'B)
  val edge = 'A ~ 'B

  def createPlayerGraph(players: Vector[Player]): Graph[Player, UnDiEdge] = {
    val pairs = for {
      (a, i) <- players.view.zipWithIndex
      b <- players.drop(i + 1)
      if a.floor != b.floor
    } yield a ~ b
    Graph.from(players, pairs)
  }

  /**
   * Find a Hamiltonian cycle in the given graph. Returns a Hamiltonian path on the
   * inner nodes of the given graph if and only if the given graph is an Ore graph.
   * Path representation is a sequence of nodes in the order they should be followed.
   * Note that if a path is found, the first and final elements will be the same node
   * (as we are finding a Hamiltonian cycle rather than path).
   * @param g graph to find Hamiltonian cycle for
   * @tparam A node type
   * @tparam B edge type
   * @return a hamiltonian cycle, if possible
   */
  def hamCycle[A, B[X] <: UnDiEdge[X]](g: Graph[A, B]): Option[Vector[g.NodeT]] = {
    // helper functions
    def seqPairs(v: Seq[g.NodeT]): Iterator[(g.NodeT, g.NodeT)] = {
      for ((a +: b +: _) <- v.sliding(2)) yield (a, b)
    }
    def firstGap(path: Seq[g.NodeT], startIndex: Int): Option[((g.NodeT, g.NodeT), Int)] = {
      // recall that head and last must be connected
      // TODO: connect head and last in caller
      val consecutivePairs = seqPairs(path.drop(startIndex))
      consecutivePairs.zipWithIndex find {
        case ((u, v), i) => !(u.neighbors contains v)
      }
    }
    def crossingChord(path: Seq[g.NodeT], gap: ((g.NodeT, g.NodeT), Int)): Option[((g.NodeT, g.NodeT), Int)] = {
      val ((s, t), i) = gap
      val pairs = seqPairs(path)
      pairs.zipWithIndex find {
        case ((u, v), j) => v != s && v != t && (u.neighbors contains s) && (v.neighbors contains t)
      }
    }
    @annotation.tailrec
    def go(path: Vector[g.NodeT], startIndex: Int = 0): Option[Vector[g.NodeT]] = {
      val circular = path :+ path.head
      val gap1 = firstGap(circular, startIndex)
      gap1 match {
        case Some(el @ (_, i)) =>
          // can't use monadic for-comprehension with tail recursion
          crossingChord(circular, el) match {
            case Some((_, j)) =>
              val section = path.slice(i+1, j+1)
              go(path.patch(i + 1, section.reverse, section.length), i + 1)
            case None => None // we couldn't find any pairs to fix the gap (contradiction)
          }
        case None => Some(circular) // our path has no gaps!
      }
    }

    if (g.nodes.size < 3 || !isOre(g)) {
      // only attempt this method when guaranteed to converge
      None
    } else {
      // randomly shuffle nodes into a circle, regardless of graph edges
      val nodes = util.shuffle(g.nodes)
      // call recursive helper
      go(nodes)
    }
  }

  /**
    * Detects whether the given graph meets the Ore conditions.
    * See http://en.wikipedia.org/wiki/Ore's_theorem
    *
    */
  def isOre[A, B[X] <: UnDiEdge[X]](g: Graph[A, B]): Boolean = {
    val n = g.nodes.size
    if (n < 3) {
      false
    } else {
      // skip self edges and duplicate edges as graph is undirected
      val nodePairs = for {
        (a, i) <- g.nodes.view.zipWithIndex
        b <- g.nodes.view.drop(i + 1)
      } yield (a, b)
      nodePairs forall {
        case (a, b) =>
        // a adjacent to b or degree(a) + degree(b) >= n
        (a.neighbors contains b) || (a.degree + b.degree >= n)
      }
    }
  }

  def isMeyniel[A, B[X] <: DiEdge[X]](g: Graph[A, B]): Boolean = {
    val n = g.nodes.size
    // don't skip new parities
    val nodePairs = for {
      a <- g.nodes.view
      b <- g.nodes.view
    } yield (a, b)
    nodePairs forall {
      case (a, b) =>
        (a.neighbors contains b) || (a.degree + b.degree >= 2*n - 1)
    }
  }
}

