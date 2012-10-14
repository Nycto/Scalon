package com.roundeights.scalon

import org.specs2.mutable._

class nArrayTest extends Specification {

    val ary = nParser.json("""[ "test", 1234, 3.1415 ]""").asArray

    "Accessing indexes" should {

        "Return elems" in {
            ary(0) must_== nString("test")
            ary(1) must_== nInt(1234)
            ary(2) must_== nFloat(3.1415)
        }

        "throw exceptions when the index is out of bounds" in {
            ary(3) must throwAn[IndexOutOfBoundsException]
            ary(4) must throwAn[IndexOutOfBoundsException]
            ary(-1) must throwAn[IndexOutOfBoundsException]
        }
    }

    "Arrays" should {

        "Convert to Strings" in {
            ary.toString must_== """["test",1234,3.1415]"""
        }

        "Return the size" in {
            ary.length must_== 3
            nParser.json("""[]""").asArray.length must_== 0
        }

        "Be iterable" in {
            val base = nParser.json("""[1,2,3,4]""").asArray
            base.foldRight( List[Int]() )( _.asInt.intValue :: _ ) must_==
                List(1,2,3,4)
        }

        "respond to equality" in {
            ary must_== ary
            nParser.json("[]") must_== nParser.json("[]")
            nParser.json("[1,2,3]") must_== nParser.json("[1,2,3]")
            nParser.json("[1,2]") must_!= nParser.json("[1,2,3]")
            nParser.json("[]") must_!= nParser.json("[1,2,3]")
            nParser.json("[1,2,3]") must_!= nParser.json("[1,5,3]")
        }
    }

}


