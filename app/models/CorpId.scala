package models

/**
 * @author bsidhom
 */
case class CorpId(id: String) {
  require(id.length > 0)
}

