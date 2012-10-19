package com.roundeights.scalon

import com.google.gson.{JsonElement, Gson}

/**
 * The various types of values
 */
object nType extends Enumeration {
    type nType = Value
    val String, Int, Float, Bool, Null, Object, Array = Value
}

/**
 * Companion
 */
object nElement {

    implicit def str_to_nString ( data: String ): nString = new nString( data )
    implicit def nString_to_str ( data: nString ): String = data.asString

    implicit def int_to_nInt ( data: Int ): nInt = new nInt( data )
    implicit def nInt_to_int ( data: nInt ): BigInt = data.asInt

    implicit def float_to_nFloat ( data: Float ): nFloat = new nFloat( data )
    implicit def nFloat_to_float ( data: nFloat ): BigDecimal= data.asFloat

    implicit def bool_to_nBool ( data: Boolean ): nBool = new nBool( data )
    implicit def nBool_to_bool ( data: nBool ): Boolean = data.asBool

    implicit def map_to_nObject (
        data: Map[String, nElement]
    ): nObject = new nObject.Native( data )
    implicit def nObject_to_map (
        data: nObject
    ): Map[String, nElement] = data.toMap

    implicit def list_to_nList ( data: List[nElement] ): nList = nList( data )
    implicit def nList_to_list ( data: nList ): List[nElement] = data.toList

}

/**
 * The shared type of all Scalon objects
 */
trait nElement extends Equals {

    /**
     * Returns the type of this object
     */
    def getType: nType.nType

    /**
     * Returns this object as a GSON object
     */
    private[scalon] def gson: JsonElement

    /**
     * Returns this object as a JSON string
     */
    def json: String = new Gson().toJson( gson )

    /**
     * Type detection methods
     */
    def isString: Boolean = getType == nType.String
    def isInt: Boolean = getType == nType.Int
    def isFloat: Boolean = getType == nType.Float
    def isBool: Boolean = getType == nType.Bool
    def isNull: Boolean = getType == nType.Null
    def isObject: Boolean = getType == nType.Object
    def isArray: Boolean = getType == nType.Array

    /**
     * Type casting methods
     */
    def asString: String = throw nTypeMismatch( "String", getType )
    def asInt: BigInt = throw nTypeMismatch( "Int", getType )
    def asFloat: BigDecimal = throw nTypeMismatch( "Float", getType )
    def asBool: Boolean = throw nTypeMismatch( "Bool", getType )
    def asObject: nObject = throw nTypeMismatch( "Object", getType )
    def asArray: nList = throw nTypeMismatch( "Array", getType )

}

