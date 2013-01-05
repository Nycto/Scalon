package com.roundeights.scalon

import com.google.gson.{JsonObject, JsonElement, Gson}
import scala.collection.{Iterable, Iterator}
import scala.collection.immutable.Map

/**
 * Notation Objects
 */
trait nObject extends nElement with nObject.Interface[nObject] with Equals {

    /** {@inheritDoc} */
    protected def build ( obj: nObject ): nObject = obj

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.Object

    /** {@inheritDoc} */
    override def asObject: nObject = this

    /** {@inheritDoc} */
    override def equals ( that: Any ): Boolean = that match {
        case thatObj: nObject => {
            thatObj.canEqual( this ) &&
            keySet.diff( thatObj.keySet ) != 0 &&
            this.forall { (entry) => get(entry._1) == thatObj.get(entry._1) }
        }
        case _ => false
    }

    /** {@inheritDoc} */
    override def canEqual ( that: Any ) = that.isInstanceOf[nObject]

    /** {@inheritDoc} */
    override def hashCode = this.foldLeft(1) { (accum, entry) =>
        41 * (41 * accum + entry._1.hashCode) + entry._2.hashCode
    }

    /** {@inheritDoc} */
    override def toString: String = json

}

/**
 * Companion
 */
object nObject {

    /**
     * Creates a new nList
     */
    def apply ( source: Map[String, nElement] = Map() ): nObject
        = new nObject.Native( source )

    /**
     * The interface for nObjects
     *
     * @param [T] The type of object produced when you add or remove an
     *      element from this object
     */
    trait Interface [T] extends Iterable[(String, nElement)] {

        /**
         * Builds a new output object any time an element is added or removed
         */
        protected def build ( obj: nObject ): T

        /**
         * Returns this object as a map
         */
        def toMap: Map[String, nElement]

        /**
         * Returns a value from this object
         */
        def get( key: String ): Option[nElement]

        /**
         * Adds a new value to this object
         */
        def + ( keyVal: (String, nElement) ): T
            = build( new nObject.Native( toMap + keyVal ) )

        /**
         * Adds a new value to this object
         */
        def - ( key: String ): T = build( new nObject.Native( toMap - key ) )

        /**
         * Returns whether a key exists
         */
        def contains( key: String ): Boolean = get(key).isDefined

        /**
         * Returns a set of all the keys in this object
         */
        def keySet: Set[String] = iterator.foldLeft( Set[String]() )( _ + _._1 )

        /**
         * Returns whether a key exists and is null
         */
        def isNull ( key: String ): Boolean = get( key ).exists( _.isNull )

        /**
         * Returns a key as a String, if the key exists and it is the
         * correct type
         */
        def str ( key: String ): Option[String]
            = get( key ).filter( _.isString ).map( _.asString )

        /**
         * Returns a key as an Integer, if the key exists and it is the
         * correct type
         */
        def int ( key: String ): Option[BigInt]
            = get( key ).filter( _.isInt ).map( _.asInt )

        /**
         * Returns a key as a Float, if the key exists and it is the
         * correct type
         */
        def float ( key: String ): Option[BigDecimal]
            = get( key ).filter( _.isFloat ).map( _.asFloat )

        /**
         * Returns a key as a Boolean, if the key exists and it is the
         * correct type
         */
        def bool ( key: String ): Option[Boolean]
            = get( key ).filter( _.isBool ).map( _.asBool )

        /**
         * Returns a key as an Object, if the key exists and it is the
         * correct type
         */
        def obj ( key: String ): Option[nObject]
            = get( key ).filter( _.isObject ).map( _.asObject )

        /**
         * Returns a key as an Array, if the key exists and it is the
         * correct type
         */
        def ary ( key: String ): Option[nList]
            = get( key ).filter( _.isArray ).map( _.asArray )

    }

    /**
     * A GSON based nObject
     */
    case class GSON ( private val inner: JsonObject ) extends nObject {

        /** {@inheritDoc} */
        override private[scalon] def gson: JsonElement = inner

        /** {@inheritDoc} */
        override def iterator: Iterator[(String, nElement)]
            = new Iterator[(String, nElement)] {
                private val entries = inner.entrySet.iterator
                override def hasNext: Boolean = entries.hasNext
                override def next: (String, nElement) = {
                    val elem = entries.next
                    ( elem.getKey, nParser.gson( elem.getValue ) )
                }
            }

        /** {@inheritDoc} */
        override def toMap: Map[String, nElement]
            = iterator.foldLeft( Map[String, nElement]() ){ _ + _ }

        /** {@inheritDoc} */
        override def get( key: String ): Option[nElement]
            = Option( inner.get(key) ).map( nParser.gson( _ ) )

    }

    /**
     * An object implemented using a map
     */
    case class Native (
        override val toMap: Map[String, nElement] = Map()
    ) extends nObject {

        /** {@inheritDoc} */
        override private[scalon] def gson: JsonElement = {
            val result = new JsonObject()
            toMap.foreach { (item) => result.add( item._1, item._2.gson ) }
            result
        }

        /** {@inheritDoc} */
        override def iterator: Iterator[(String, nElement)] = toMap.iterator

        /** {@inheritDoc} */
        override def get( key: String ): Option[nElement] = toMap.get( key )

    }

}

