package com.roundeights.scalon

import org.specs2.mutable._

class nParserTest extends Specification {

    "Parsing primitives" should {

        "Produce a string" in {
            nParser.json(""" "test" """) must_== nString("test")
        }

        "Produce an integer" in {
            nParser.json("1234") must_== nInt(1234)
        }

        "Produce a float" in {
            nParser.json("3.1415") must_== nFloat(3.1415)
        }

        "Produce null" in {
            nParser.json("null") must_== nNull()
        }

        "Produce true" in {
            nParser.json("true") must_== nBool(true)
        }

        "Produce false" in {
            nParser.json("false") must_== nBool(false)
        }
    }

    "Parser errors" should {
        "throw nParserExceptions" in {
            nParser.json("{") must throwA[nParserException]
            nParser.json("bare words") must throwA[nParserException]
        }
    }

}


