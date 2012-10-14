package com.roundeights.scalon

/**
 * String type
 */
case class nString ( override val asString: String ) extends nElement {

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.String

    /** {@inheritDoc} */
    override def toString: String = asString
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
    override def toString: String = asInt.toString
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
    override def toString: String = asFloat.toString
}

/**
 * Boolean type
 */
case class nBool ( override val asBool: Boolean ) extends nElement {

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.Bool

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
    override def toString: String = ""
}


