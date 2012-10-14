package com.roundeights.scalon

import org.specs2.mutable._

class nObjectTest extends Specification {

    val obj = nParser.json("""{
        str: "Something",
        int: 1234,
        float: 3.1415,
        nil: null,
        truthy: true,
        falsey: false,
        obj: { 1:1 },
        ary: [ 1, 2, 3 ]
    }""").asObject

    "Accessing values" should {

        "Provide access to strings" in {
            obj.str("str") must_== Some("Something")
            obj.elem("str") must_== Some(nString("Something"))
            obj.hasKey("str") must_== true
            obj.str("int") must_== None
        }

        "Provide access to ints" in {
            obj.int("int") must_== Some(BigInt(1234))
            obj.elem("int") must_== Some(nInt(1234))
            obj.hasKey("int") must_== true
            obj.int("str") must_== None
        }

        "Provide access to floats" in {
            obj.float("float") must_== Some(BigDecimal(3.1415))
            obj.elem("float") must_== Some(nFloat(3.1415))
            obj.hasKey("float") must_== true
            obj.float("str") must_== None
        }

        "Provide access to Booleans" in {
            obj.bool("str") must_== None

            obj.bool("truthy") must_== Some(true)
            obj.elem("truthy") must_== Some(nBool(true))
            obj.hasKey("truthy") must_== true

            obj.bool("falsey") must_== Some(false)
            obj.elem("falsey") must_== Some(nBool(false))
            obj.hasKey("falsey") must_== true
        }

        "Provide access to Nulls" in {
            obj.isNull("nil") must_== true
            obj.isNull("str") must_== false
            obj.isNull("notKey") must_== false
        }

        "Provide access to objects" in {
            val equals = nParser.json("{1:1}").asObject
            obj.obj("obj") must_== Some( equals )
            obj.elem("obj") must_== Some( equals )
            obj.hasKey("obj") must_== true
            obj.obj("int") must_== None
        }

    }

    "Objects" should {

        "List their keys" in {
            obj.keys must_== Set(
                "str", "int", "float", "nil",
                "truthy", "falsey", "obj", "ary"
            )
        }

        "respond to equality" in {
            obj must_!= nParser.json("{1:1}")
            nParser.json("{1:2}") must_!= nParser.json("{1:1}")
            nParser.json("{2:1}") must_!= nParser.json("{1:1}")
            nParser.json("""{"str":1}""") must_!= nParser.json("""{"STR":1}""")
            nParser.json("{ 5:6, 7:8 }") must_== nParser.json("{ 7:8, 5:6 }")
            nParser.json("{1:1}") must_== nParser.json("{1:1}")
        }

        "Convert to strings" in {
            nParser.json("{1:2,3:4}").toString must_== """{"1":2,"3":4}"""
        }
    }

}

