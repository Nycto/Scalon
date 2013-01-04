package com.roundeights.scalon

import org.specs2.mutable._

class nListTest extends Specification {

    val ary = nParser.json("""[ "test", 1234, 3.1415 ]""").asArray

    "Lists" should {

        "Convert to Strings" in {
            ary.toString must_== """["test",1234,3.1415]"""
        }

        "Return the size" in {
            ary.length must_== 3
            nParser.json("""[]""").asArray.length must_== 0
        }

        "Allow access to indexes for GSON based lists" in {
            ary(0) must_== nString("test")
            ary(1) must_== nInt(1234)
        }

        "Allow access to indexes for native nLists" in {
            val list = 1 :: 2 :: nList()
            list(0) must_== nInt(1)
            list(1) must_== nInt(2)
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

        "allow values to be prepended" in {
            val list = 1 :: 2 :: nParser.jsonList("[3,4]")
            list must_== nParser.json("[1,2,3,4]")
        }

        "allow lists to be prepended" in {
            val list = nParser.jsonList("[1,2]") ::: nParser.jsonList("[3,4]")
            list must_== nParser.json("[1,2,3,4]")
        }

    }

}


