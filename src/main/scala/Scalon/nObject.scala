package com.roundeights.scalon

import com.google.gson.{JsonObject, JsonElement, Gson}
import scala.collection.{Iterable, Iterator}
import scala.collection.immutable.Map
import java.util.UUID

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

    /** Creates a new nObject */
    def apply ( source: Map[String, nElement] = Map() ): nObject
        = new nObject.Native( source )

    /** Creates a new nObject */
    def from ( source: Map[String, nElement.ToJson] ): nObject = {
        new nObject.Native(
            source.foldLeft( Map[String, nElement]() ) {
                (accum, pair) => accum + ( pair._1 -> pair._2.toJson )
            }
        )
    }

    /** Creates a new nObject */
    def apply ( data: (Any, Any)* ): nObject
        = nElement( Map(data:_*) ).asObject

    /**
     * The interface for nObjects
     *
     * @param [T] The type of object produced when you add or remove an
     *      element from this object
     */
    trait Interface [T] extends Iterable[(String, nElement)] {

        /** Builds a new object any time an element is added or removed */
        protected def build ( obj: nObject ): T

        /** Returns this object as a map */
        def toMap: Map[String, nElement]

        /** Converts an nElement to another type for the 'toMapOf' method */
        type MapCast[A] = (nElement) => Option[A]

        /** Converts this element to a map of a specific type */
        def toMapOf[O : MapCast]: Map[String, O] = {
            iterator.foldLeft( Map[String,O]() ) { (accum, pair) =>
                val value: Option[O] = pair._2
                value match {
                    case None => accum
                    case Some(item) => accum + (pair._1 -> item)
                }
            }
        }

        /** Returns a value from this object */
        def get_?( key: String ): Option[nElement]

        /** Returns a value from this object */
        def get( key: String ): nElement
            = get_?( key ).getOrElse( throw nMissingKey(key, "Element") )

        /** Adds a new value to this object */
        def + ( keyVal: (Any, Any) ): T = build( new nObject.Native(
            toMap + ( keyVal._1.toString -> nElement( keyVal._2 ) )
        ) )

        /** Adds a new value to this object */
        def - ( key: String ): T = build( new nObject.Native( toMap - key ) )

        /** Returns whether a key exists */
        def contains( key: String ): Boolean = get_?(key).isDefined

        /** Returns a set of all the keys in this object */
        def keySet: Set[String] = iterator.foldLeft( Set[String]() )( _ + _._1 )

        /** Returns whether a key exists and is null */
        def isNull ( key: String ): Boolean = get_?( key ).exists( _.isNull )

        /** Returns a key as a String, if it exists and is the right type */
        def str_? ( key: String ): Option[String]
            = get_?( key ).filter( _.isString ).map( _.asString )

        /** Returns a key as a String, if it exists and is the right type */
        def str ( key: String ): String
            = str_?( key ).getOrElse( throw nMissingKey(key, "String") )

        /** Returns a key as an Integer, if it exists and is the right type */
        def int_? ( key: String ): Option[BigInt]
            = get_?( key ).filter( _.isInt ).map( _.asInt )

        /** Returns a key as an Integer, if it exists and is the right type */
        def int ( key: String ): BigInt
            = int_?( key ).getOrElse( throw nMissingKey(key, "Integer") )

        /** Returns a key as a Float, if it exists and is the right type */
        def float_? ( key: String ): Option[BigDecimal]
            = get_?( key ).filter( _.isFloat ).map( _.asFloat )

        /** Returns a key as a Float, if it exists and is the right type */
        def float ( key: String ): BigDecimal
            = float_?( key ).getOrElse( throw nMissingKey(key, "Float") )

        /** Returns a key as a Boolean, if it exists and is the right type */
        def bool_? ( key: String ): Option[Boolean]
            = get_?( key ).filter( _.isBool ).map( _.asBool )

        /** Returns a key as a Boolean, if it exists and is the right type */
        def bool ( key: String ): Boolean
            = bool_?( key ).getOrElse( throw nMissingKey(key, "Boolean") )

        /**
         * Returns a key as a Boolean, if the key exists. Loosely intereprets
         * other data types and converts them when possible
         */
        def bool_~? ( key: String ): Option[Boolean]
            = get_?( key ).flatMap( _.asBool_~? )

        /**
         * Returns a key as a Boolean, if the key exists. Loosely intereprets
         * other data types and converts them when possible
         */
        def bool_~ ( key: String ): Boolean
            = bool_~?( key ).getOrElse( throw nMissingKey(key, "Boolean") )

        /** Returns a key as an Object, if it exists and is the right type */
        def obj_? ( key: String ): Option[nObject]
            = get_?( key ).filter( _.isObject ).map( _.asObject )

        /** Returns a key as an Object, if it exists and is the right type */
        def obj ( key: String ): nObject
            = obj_?( key ).getOrElse( throw nMissingKey(key, "Object") )

        /** Returns a key as an Array, if it exists and is the right type */
        def ary_? ( key: String ): Option[nList]
            = get_?( key ).filter( _.isArray ).map( _.asArray )

        /** Returns a key as an Array, if it exists and is the right type */
        def ary ( key: String ): nList
            = ary_?( key ).getOrElse( throw nMissingKey(key, "Array") )

        /** Returns a key as a String, if it exists and is the right type */
        def uuid_? ( key: String ): Option[UUID]
            = get_?( key ).flatMap( _.asUUID_? )

        /** Returns a key as a UUID, if it exists and is the right type */
        def uuid ( key: String ): UUID
            = uuid_?( key ).getOrElse( throw nMissingKey(key, "UUID") )


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

            /** Applies a callback if a key exists */
            def patchElem (
                key: String, mutator: (U, nElement) => U
            ): Patch[U] = {
                get_?(key)
                    .map( value => new Patch( mutator(initial, value) ) )
                    .getOrElse(this)
            }

            /** Applies a callback if a key exists, using an explicit cast */
            def patchAs[I] (
                key: String, cast: (nElement) => I, mutator: (U, I) => U
            ): Patch[U] = {
                get_?(key)
                    .map( cast )
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

