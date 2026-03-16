package it.unibo.pps.u02

import org.junit.*
import org.junit.Assert.*

class RecordTest:

  case class Item(desc: String, code: Int)
  val record = Item("a", 10)

  @Test def testMatches(): Unit =
    assertTrue(record match {case Item(x, 10) => true; case _ => false})
    assertTrue: // significant indentation style
      record match
        case Item("a", 10) => true
        case _ => false
    
  @Test def testEquality(): Unit =
    assertEquals(record, Item("a", 10))
    assertNotEquals(record, Item("a", 11))

  @Test def testChangeCode(): Unit =
    def changeCode(item: Item)(f: Int => Int): Item = item match
      case Item(desc, code) => Item(desc, f(code))
    assertEquals(Item("a", 11), changeCode(Item("a", 10))(_ + 1))
    