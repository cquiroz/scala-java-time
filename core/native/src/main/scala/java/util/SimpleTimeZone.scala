package java.util

class SimpleTimeZone(rawOffset: Int, var ID: String) extends TimeZone {
  override def getRawOffset            = rawOffset
  /* concrete methods */
  override def getID: String           = ID
  override def setID(id: String): Unit = ID = id

}
