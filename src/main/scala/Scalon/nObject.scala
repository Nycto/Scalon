package com.roundeights.scalon

import com.google.gson.{JsonObject, Gson}
import scala.collection.{Iterable, Iterator}

/**
 * Notation Objects
 */
trait nObject extends nElement with Iterable[(String,nElement)] {

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.Object

    /** {@inheritDoc} */
    override def asObject: nObject = this

    /**
     * Returns a key as an element
     */
    def elem ( key: String ): Option[nElement]

    /**
     * Returns a set of all the keys in this object
     */
    def keys: Set[String] = iterator.foldLeft[Set[String]]( Set() )( _ + _._1 )

    /**
     * Returns whether a key exists
     */
    def hasKey ( key: String ): Boolean = elem(key).isDefined

    /**
     * Returns whether a key exists and is null
     */
    def isNull ( key: String ): Boolean = elem( key ).exists( _.isNull )

    /**
     * Returns a key as a String, if the key exists and it is the
     * correct type
     */
    def str ( key: String ): Option[String]
        = elem( key ).filter( _.isString ).map( _.asString )

    /**
     * Returns a key as an Integer, if the key exists and it is the
     * correct type
     */
    def int ( key: String ): Option[BigInt]
        = elem( key ).filter( _.isInt ).map( _.asInt )

    /**
     * Returns a key as a Float, if the key exists and it is the
     * correct type
     */
    def float ( key: String ): Option[BigDecimal]
        = elem( key ).filter( _.isFloat ).map( _.asFloat )

    /**
     * Returns a key as a Boolean, if the key exists and it is the
     * correct type
     */
    def bool ( key: String ): Option[Boolean]
        = elem( key ).filter( _.isBool ).map( _.asBool )

    /**
     * Returns a key as an Object, if the key exists and it is the
     * correct type
     */
    def obj ( key: String ): Option[nObject]
        = elem( key ).filter( _.isObject ).map( _.asObject )

    /**
     * Returns a key as an Array, if the key exists and it is the
     * correct type
     */
    def ary ( key: String ): Option[nArray]
        = elem( key ).filter( _.isArray ).map( _.asArray )

    /** {@inheritDoc} */
    override def equals ( that: Any ): Boolean = that match {
        case thatObj: nObject if (
            thatObj.canEqual( this )
            && keys.diff( thatObj.keys ) != 0
        ) => forall {
            (entry) => elem(entry._1) == thatObj.elem(entry._1)
        }
        case _ => false
    }

    /** {@inheritDoc} */
    override def canEqual ( that: Any ) = that.isInstanceOf[nObject]
}

/**
 * Companion
 */
object nObject {

    /**
     * A GSON based nObject
     */
    class GSON ( private val inner: JsonObject ) extends nObject {

        /** {@inheritDoc} */
        override def iterator = new Iterator[(String,nElement)] {
            private val entries = inner.entrySet.iterator
            override def hasNext: Boolean = entries.hasNext
            override def next: (String, nElement) = {
                val elem = entries.next
                ( elem.getKey, nParser.gson( elem.getValue ) )
            }
        }

        /** {@inheritDoc} */
        override def elem ( key: String ): Option[nElement]
            = Option( inner.get(key) ).map( nParser.gson( _ ) )

        /** {@inheritDoc} */
        override def toString: String = new Gson().toJson( inner )
    }

}

