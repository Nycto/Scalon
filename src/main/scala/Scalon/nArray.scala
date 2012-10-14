package com.roundeights.scalon

import com.google.gson.{JsonArray, Gson}
import scala.collection.IndexedSeq

/**
 * Notation Array
 */
trait nArray extends nElement with IndexedSeq[nElement] {

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.Array

    /** {@inheritDoc} */
    override def asArray: nArray = this

    /** {@inheritDoc} */
    override def equals ( that: Any ): Boolean = that match {
        case thatAry: nArray if (
            thatAry.canEqual( this )
            && length == thatAry.length
        ) => (0 until length).forall( i => apply(i) == thatAry(i) )
        case _ => false
    }

    /** {@inheritDoc} */
    override def canEqual ( that: Any ) = that.isInstanceOf[nArray]

}

/**
 * Companion
 */
object nArray {

    /**
     * A GSON based nArray
     */
    class GSON ( private val ary: JsonArray ) extends nArray {

        /** {@inheritDoc} */
        override def apply( idx: Int ): nElement = nParser.gson( ary.get(idx) )

        /** {@inheritDoc} */
        override def length: Int = ary.size()

        /** {@inheritDoc} */
        override def toString: String = new Gson().toJson( ary )
    }

}


