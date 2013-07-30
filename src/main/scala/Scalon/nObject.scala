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
    override def asObject_? = Some( this )

    /** {@inheritDoc} */
    override def equals ( that: Any ): Boolean = that match {
        case thatObj: nObject => {
            thatObj.canEqual( this ) &&
            keySet.diff( thatObj.keySet ) != 0 &&
            this.forall { (item) => get_?(item._1) == thatObj.get_?(item._1) }
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
     * Creates a new nObject
     */
    def apply ( source: Map[String, nElement] = Map() ): nObject
        = new nObject.Native( source )

    /**
     * Creates a new nObject
     */
    def apply ( data: (Any, Any)* ): nObject
        = nElement( Map(data:_*) ).asObject

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
        def get_?( key: String ): Option[nElement]

        /**
         * Returns a value from this object
         */
        def get( key: String ): nElement
            = get_?( key ).getOrElse( throw nMissingKey(key, "Element") )

        /**
         * Adds a new value to this object
         */
        def + ( keyVal: (Any, Any) ): T = build( new nObject.Native(
            toMap + ( keyVal._1.toString -> nElement( keyVal._2 ) )
        ) )

        /**
         * Adds a new value to this object
         */
        def - ( key: String ): T = build( new nObject.Native( toMap - key ) )

        /**
         * Returns whether a key exists
         */
        def contains( key: String ): Boolean = get_?(key).isDefined

        /**
         * Returns a set of all the keys in this object
         */
        def keySet: Set[String] = iterator.foldLeft( Set[String]() )( _ + _._1 )

        /**
         * Returns whether a key exists and is null
         */
        def isNull ( key: String ): Boolean = get_?( key ).exists( _.isNull )

        /**
         * Returns a key as a String, if the key exists and it is the
         * correct type
         */
        def str_? ( key: String ): Option[String]
            = get_?( key ).filter( _.isString ).map( _.asString )

        /**
         * Returns a key as a String, if the key exists and it is the
         * correct type
         */
        def str ( key: String ): String
            = str_?( key ).getOrElse( throw nMissingKey(key, "String") )

        /**
         * Returns a key as an Integer, if the key exists and it is the
         * correct type
         */
        def int_? ( key: String ): Option[BigInt]
            = get_?( key ).filter( _.isInt ).map( _.asInt )

        /**
         * Returns a key as an Integer, if the key exists and it is the
         * correct type
         */
        def int ( key: String ): BigInt
            = int_?( key ).getOrElse( throw nMissingKey(key, "Integer") )

        /**
         * Returns a key as a Float, if the key exists and it is the
         * correct type
         */
        def float_? ( key: String ): Option[BigDecimal]
            = get_?( key ).filter( _.isFloat ).map( _.asFloat )

        /**
         * Returns a key as a Float, if the key exists and it is the
         * correct type
         */
        def float ( key: String ): BigDecimal
            = float_?( key ).getOrElse( throw nMissingKey(key, "Float") )

        /**
         * Returns a key as a Boolean, if the key exists and it is the
         * correct type
         */
        def bool_? ( key: String ): Option[Boolean]
            = get_?( key ).filter( _.isBool ).map( _.asBool )

        /**
         * Returns a key as a Boolean, if the key exists and it is the
         * correct type
         */
        def bool ( key: String ): Boolean
            = bool_?( key ).getOrElse( throw nMissingKey(key, "Boolean") )

        /**
         * Returns a key as an Object, if the key exists and it is the
         * correct type
         */
        def obj_? ( key: String ): Option[nObject]
            = get_?( key ).filter( _.isObject ).map( _.asObject )

        /**
         * Returns a key as an Object, if the key exists and it is the
         * correct type
         */
        def obj ( key: String ): nObject
            = obj_?( key ).getOrElse( throw nMissingKey(key, "Object") )

        /**
         * Returns a key as an Array, if the key exists and it is the
         * correct type
         */
        def ary_? ( key: String ): Option[nList]
            = get_?( key ).filter( _.isArray ).map( _.asArray )

        /**
         * Returns a key as an Array, if the key exists and it is the
         * correct type
         */
        def ary ( key: String ): nList
            = ary_?( key ).getOrElse( throw nMissingKey(key, "Array") )


        /** The type definition for implicit path methods */
        type PatchType[A] = (nElement) => Option[A]

        /**
        * A patch object, which is used to make progressive changes
        * to an initial object
        */
        class Patch[U] ( private val initial: U ) {

            /** Returns the final product of this patch sequence */
            def done: U = initial

            /** Applies a callback if a key exists */
            def patch[I : PatchType] (
                key: String, mutator: (U, I) => U
            ): Patch[U] = {
                get_?(key)
                    .flatMap( implicitly[nElement => Option[I]] _ )
                    .map( value => new Patch( mutator(initial, value) ) )
                    .getOrElse(this)
            }
        }

        /** Begins a patching process for a set of keys */
        def patch[U] ( initial: U ) = new Patch[U]( initial )

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
        override def get_?( key: String ): Option[nElement]
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
        override def get_?( key: String ): Option[nElement] = toMap.get( key )

    }

}

