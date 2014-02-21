package models

/**
 * @author bsidhom
 */
case class Player(id: CorpId, name: String, floor: Int)

object Player {
  def all(): List[Player] = Nil
}

