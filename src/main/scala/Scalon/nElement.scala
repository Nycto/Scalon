package com.roundeights.scalon

/**
 * The various types of values
 */
object nType extends Enumeration {
    type nType = Value
    val String, Int, Float, Bool, Null, Object, Array = Value
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
    def asArray: nArray = throw nTypeMismatch( "Array", getType )

}

