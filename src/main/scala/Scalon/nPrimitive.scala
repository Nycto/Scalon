package com.roundeights.scalon

import com.google.gson.{JsonPrimitive, JsonElement, JsonNull}

/**
 * String type
 */
case class nString ( override val asString: String ) extends nElement {

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.String

    /** {@inheritDoc} */
    override private[scalon] def gson: JsonElement
        = new JsonPrimitive(asString)

    /** {@inheritDoc} */
    override def asString_? = Some( asString )

    /** {@inheritDoc} */
    override def toString: String = asString

    /** {@inheritDoc} */
    override def asBool_~? = asString.toLowerCase match {
        case "true" => Some(true)
        case "t" => Some(true)
        case "yes" => Some(true)
        case "y" => Some(true)
        case "on" => Some(true)
        case "false" => Some(false)
        case "f" => Some(false)
        case "no" => Some(false)
        case "n" => Some(false)
        case "off" => Some(false)
        case "" => Some(false)
        case _ => None
    }
}

/**
 * Int type
 */
case class nInt ( override val asInt: BigInt ) extends nElement {

    /**
     * Parses a string as an Int
     */
    def this( int: String ) = this( BigInt(int) )

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.Int

    /** {@inheritDoc} */
    override private[scalon] def gson: JsonElement
        = new JsonPrimitive(asInt)

    /** {@inheritDoc} */
    override def asInt_? = Some( asInt )

    /** {@inheritDoc} */
    override def asString_? = Some( asInt.toString )

    /** {@inheritDoc} */
    override def toString: String = asInt.toString

    /** {@inheritDoc} */
    override def asBool_~? = {
        if ( asInt == 1 ) Some(true)
        else if ( asInt == 0 ) Some(false)
        else None
    }
}

/**
 * Decimal Type
 */
case class nFloat ( override val asFloat: BigDecimal ) extends nElement {

    /**
     * Parses a string as a float
     */
    def this( float: String ) = this( BigDecimal(float) )

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.Float

    /** {@inheritDoc} */
    override private[scalon] def gson: JsonElement
        = new JsonPrimitive(asFloat)

    /** {@inheritDoc} */
    override def asFloat_? = Some( asFloat )

    /** {@inheritDoc} */
    override def asString_? = Some( asFloat.toString )

    /** {@inheritDoc} */
    override def toString: String = asFloat.toString
}

/**
 * Boolean type
 */
case class nBool ( override val asBool: Boolean ) extends nElement {

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.Bool

    /** {@inheritDoc} */
    override private[scalon] def gson: JsonElement
        = new JsonPrimitive(asBool)

    /** {@inheritDoc} */
    override def asBool_? = Some( asBool )

    /** {@inheritDoc} */
    override def asBool_~? = Some( asBool )

    /** {@inheritDoc} */
    override def asString_? = Some( asBool.toString )

    /** {@inheritDoc} */
    override def toString: String = asBool.toString
}

/**
 * Null type
 */
case class nNull () extends nElement {

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.Null

    /** {@inheritDoc} */
    override private[scalon] def gson: JsonElement = JsonNull.INSTANCE

    /** {@inheritDoc} */
    override def asString_? = Some("")

    /** {@inheritDoc} */
    override def toString: String = ""

    /** {@inheritDoc} */
    override def asBool_~? = Some( false )
}


