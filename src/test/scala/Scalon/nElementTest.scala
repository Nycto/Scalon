package com.roundeights.scalon

import org.specs2.mutable._

class nElementTest extends Specification {

    "nElement apply" should {

        "Handle null" in {
            nElement(null) must_== nNull()
        }

        "Pass other nElements through" in {
            nElement( nString("Blah") ) must_== nString("Blah")
        }

        "Handle strings" in {
            nElement("Data") must_== nString("Data")
        }

        "Handle ints" in {
            nElement(123) must_== nInt(123)
            nElement( BigInt(123) ) must_== nInt(123)
        }

        "Handle floats" in {
            nElement(3.1415) must_== nFloat(3.1415)
            nElement( BigDecimal(3.1415) ) must_== nFloat(3.1415)
        }

        "Handle booleans" in {
            nElement(true) must_== nBool(true)
        }

        "Handle List" in {
            nElement( List() ) must_== nList()
            nElement( List(1, 2, 3) ) must_== nList(1, 2, 3)
        }

        "Handle Options within Lists" in {
            nElement( List(Some(1), None, Some(2)) ) must_== nList(1, 2)
        }

        "Handle Maps" in {
            nElement( Map() ) must_== nObject()
            nElement( Map(1 -> "one", 2 -> "two") )
                .must_==( nObject( 1 -> "one", 2 -> "two" ) )
        }

        "Handle Options within Maps" in {
            nElement( Map(1 -> Some("one"), 2 -> None) )
                .must_==( nObject( 1 -> "one") )
        }

    }

}


