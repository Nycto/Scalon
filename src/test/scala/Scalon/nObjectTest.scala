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
            obj.get("str") must_== Some(nString("Something"))
            obj.str("int") must_== None
        }

        "Provide access to ints" in {
            obj.int("int") must_== Some(BigInt(1234))
            obj.get("int") must_== Some(nInt(1234))
            obj.int("str") must_== None
        }

        "Provide access to floats" in {
            obj.float("float") must_== Some(BigDecimal(3.1415))
            obj.get("float") must_== Some(nFloat(3.1415))
            obj.float("str") must_== None
        }

        "Provide access to Booleans" in {
            obj.bool("str") must_== None

            obj.bool("truthy") must_== Some(true)
            obj.get("truthy") must_== Some(nBool(true))

            obj.bool("falsey") must_== Some(false)
            obj.get("falsey") must_== Some(nBool(false))
        }

        "Provide access to Nulls" in {
            obj.isNull("nil") must_== true
            obj.isNull("str") must_== false
            obj.isNull("notKey") must_== false
        }

        "Provide access to objects" in {
            val equals = nParser.json("{1:1}").asObject
            obj.obj("obj") must_== Some( equals )
            obj.get("obj") must_== Some( equals )
            obj.obj("int") must_== None
        }

    }

    "Objects" should {

        "List their keys" in {
            obj.keySet must_== Set(
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

        "Convert to maps" in {
            nParser.jsonObj("{1:2,3:4}").toMap must_== Map(
                "1" -> nInt(2), "3" -> nInt(4)
            )
        }

        "allow keys to be added" in {
            val result = nParser.jsonObj("{1:2}") + (("3", 4)) + (("5", "six"))
            result.toString must_== """{"1":2,"3":4,"5":"six"}"""
        }

        "allow keys to be removed" in {
            val result = nParser.jsonObj("{1:2,3:4,5:6}") - "3"
            result.toString must_== """{"1":2,"5":6}"""
        }

    }

}

