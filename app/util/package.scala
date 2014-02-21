import java.security.SecureRandom

/**
 * @author bsidhom
 */
package object util {
  // Fisher-Yates shuffle
  def shuffle[A](as: Traversable[A]): Vector[A] = {
    val rand = new SecureRandom()
    as.foldLeft(Vector[A]()) {
      case (shuffled, a) =>
        val size = shuffled.length
        val j = rand.nextInt(size + 1)
        if (j == size) shuffled :+ a
        else (shuffled :+ shuffled(j)).updated(j, a)
    }
  }

}

