package com.roundeights.scalon

import org.specs2.mutable._

class nPrimitiveTest extends Specification {

    val str = nString("str")
    val int = nInt(1234)
    val float = nFloat(3.1415)
    val nil = nNull()
    val truthy = nBool(true)
    val falsey = nBool(false)

    "Primitives" should {

        "Be of the proper type" in {
            str.getType must_== nType.String
            int.getType must_== nType.Int
            float.getType must_== nType.Float
            nil.getType must_== nType.Null
            truthy.getType must_== nType.Bool
            falsey.getType must_== nType.Bool
        }

        "Equal or not equal appropriately" in {
            (str == nString("str")) must_== true
            (str == nString("other")) must_== false

            (int == nInt(1234)) must_== true
            (int == nInt(4321)) must_== false

            (float == nFloat(3.1415)) must_== true
            (float == nFloat(2.78)) must_== false

            (nil == nNull()) must_== true

            (truthy == nBool(true)) must_== true
            (truthy == nBool(false)) must_== false

            (falsey == nBool(false)) must_== true
            (falsey == nBool(true)) must_== false
        }

        "Convert to the appropriate json value" in {
            str.json must_== "\"str\""
            int.json must_== "1234"
            float.json must_== "3.1415"
            nil.json must_== "null"
            truthy.json must_== "true"
            falsey.json must_== "false"
        }

        "Convert to the appropriate string value" in {
            str.toString must_== "str"
            int.toString must_== "1234"
            float.toString must_== "3.1415"
            nil.toString must_== ""
            truthy.toString must_== "true"
            falsey.toString must_== "false"
        }

        "Be convertable to strings (where appropriate)" in {
            str.asString must_== "str"
            int.asString must_== "1234"
            float.asString must_== "3.1415"
            nil.asString must_== ""
            truthy.asString must_== "true"
            falsey.asString must_== "false"
        }

        "Loosely convert to booleans" in {
            nBool(true).asBool_~ must_== true
            nBool(false).asBool_~ must_== false

            nString("True").asBool_~ must_== true
            nString("T").asBool_~ must_== true
            nString("Yes").asBool_~ must_== true
            nString("Y").asBool_~ must_== true
            nString("On").asBool_~ must_== true

            nString("False").asBool_~ must_== false
            nString("F").asBool_~ must_== false
            nString("No").asBool_~ must_== false
            nString("N").asBool_~ must_== false
            nString("Off").asBool_~ must_== false
            nString("").asBool_~ must_== false

            nString("Unrecognized").asBool_~? must_== None

            nInt(1).asBool_~ must_== true
            nInt(0).asBool_~ must_== false
            nInt( 123 ).asBool_~? must_== None
            nInt( -123 ).asBool_~? must_== None

            nNull().asBool_~ must_== false

            nFloat( 3.14 ).asBool_~? must_== None
        }

    }

}


