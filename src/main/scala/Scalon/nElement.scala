package com.roundeights.scalon

import com.google.gson.{JsonElement, Gson}
import scala.language.implicitConversions
import java.util.UUID

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

    /** For objects that can be converted into nElement instances */
    trait ToJson {

        /** Returns this object as a JSON element */
        def toJson: nElement
    }

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

    implicit def elem_to_optStr ( data: nElement ): Option[String]
        = data.asString_?
    implicit def elem_to_optBigInt ( data: nElement ): Option[BigInt]
        = data.asInt_?
    implicit def elem_to_optInt ( data: nElement ): Option[Int]
        = data.asInt_?.filter( _.isValidInt ).map( _.intValue )
    implicit def elem_to_optBigFloat ( data: nElement ): Option[BigDecimal]
        = data.asFloat_?
    implicit def elem_to_optDouble ( data: nElement ): Option[Double]
        = data.asFloat_?.map( _.doubleValue )
    implicit def elem_to_optBool ( data: nElement ): Option[Boolean]
        = data.asBool_?
    implicit def elem_to_optObj ( data: nElement ): Option[nObject]
        = data.asObject_?
    implicit def elem_to_optList ( data: nElement ): Option[nList]
        = data.asArray_?
    implicit def elem_to_uuid ( data: nElement ): Option[UUID] = data.asUUID_?

    /** Parses a map */
    private def parseMap( data: Map[_, _] ) = nObject(
        data.foldLeft( Map[String,nElement]() ) {
            (accum, pair) => pair match {
                case (_, None) => accum
                case (key, Some(value))
                    => accum + ( key.toString -> apply(value) )
                case (key, value)
                    => accum + ( key.toString -> apply(value) )
            }
        }
    )

    /** Parses a list of elements */
    private def parseList ( data: Traversable[_] ): nList = {
        data.foldRight( nList() ) {
            (value, accum) => value match {
                case None => accum
                case Some(inner) => apply(inner) :: accum
                case _ => apply(value) :: accum
            }
        }
    }

    /**
     * Converts a value into a notation element
     */
    def apply ( elem: Any ): nElement = elem match {
        case null => nNull()
        case data: nElement => data
        case data: ToJson => data.toJson
        case data: String => nString(data)
        case data: Int => nInt( BigInt(data) )
        case data: BigInt => nInt( data )
        case data: Float => nFloat( BigDecimal(data) )
        case data: Double => nFloat( BigDecimal(data) )
        case data: BigDecimal => nFloat( data )
        case data: Boolean => nBool( data )
        case data: UUID => nString( data.toString )
        case data: Set[_] => parseList( data )
        case data: Seq[_] => parseList( data )
        case data: Map[_, _] => parseMap( data )
        case _ => throw new nParserException(
            "Unparsable type: %s".format( elem.getClass.getName )
        )
    }

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
    def asString: String
        = asString_?.getOrElse( throw nTypeMismatch( "String", getType ) )
    def asInt: BigInt
        = asInt_?.getOrElse( throw nTypeMismatch( "Int", getType ) )
    def asFloat: BigDecimal
        = asFloat_?.getOrElse( throw nTypeMismatch( "Float", getType ) )
    def asBool: Boolean
        = asBool_?.getOrElse( throw nTypeMismatch( "Bool", getType ) )
    def `asBool_~`: Boolean
        = asBool_~?.getOrElse( throw nTypeMismatch( "Bool", getType ) )
    def asObject: nObject
        = asObject_?.getOrElse( throw nTypeMismatch( "Object", getType ) )
    def asArray: nList
        = asArray_?.getOrElse( throw nTypeMismatch( "Array", getType ) )
    def asUUID: UUID
        = asUUID_?.getOrElse( throw nTypeMismatch( "UUID", getType ) )

    /**
     * Safe type casting methods
     */
    def `asString_?`: Option[String] = None
    def `asInt_?`: Option[BigInt] = None
    def `asFloat_?`: Option[BigDecimal] = None
    def `asBool_?`: Option[Boolean] = None
    def `asBool_~?`: Option[Boolean] = None
    def `asObject_?`: Option[nObject] = None
    def `asArray_?`: Option[nList] = None

    def `asUUID_?`: Option[UUID] = {
        asString_?.flatMap( value => try {
            Some( UUID.fromString(value) )
        } catch {
            case _: IllegalArgumentException => None
        })
    }

}

