package com.roundeights.scalon

import com.google.gson.{JsonArray, JsonElement, Gson}
import scala.collection.{Iterable, Iterator}

/**
 * Notation Array
 */
trait nList extends nElement with Seq[nElement] {

    /** {@inheritDoc} */
    override lazy val getType: nType.nType = nType.Array

    /** {@inheritDoc} */
    override def asArray: nList = this

    /** {@inheritDoc} */
    override def toString: String = json

    /**
     * Prepends a value to this list
     */
    def :: ( elem: nElement ): nList = nList( elem :: toList )

    /**
     * Prepends a list of values to this list
     */
    def ::: ( elems: List[nElement] ): nList = nList( elems ::: toList )

    /** {@inheritDoc} */
    override def equals ( that: Any ): Boolean = that match {
        case thatAry: nList if (
            thatAry.canEqual( this )
            && length == thatAry.length
        ) => zip( thatAry ).forall( (elem) => elem._1 == elem._2 )
        case _ => false
    }

    /** {@inheritDoc} */
    override def canEqual ( that: Any ) = that.isInstanceOf[nList]

    /** {@inheritDoc} */
    override def hashCode = this.foldLeft(1) { 41 * _ + _.hashCode }

}

/**
 * Companion
 */
object nList {

    /**
     * Creates a new nList
     */
    def apply ( source: Seq[nElement] = List() ): nList
        = new nList.Native( source.toList )

    /**
     * A GSON based nList
     */
    class GSON ( private val ary: JsonArray ) extends nList {

        /** {@inheritDoc} */
        override private[scalon] def gson: JsonElement = ary

        /** {@inheritDoc} */
        override def apply ( idx: Int ): nElement
            = nParser.gson( ary.get(idx) )

        /** {@inheritDoc} */
        override def length: Int = ary.size()

        /** {@inheritDoc} */
        override def iterator: Iterator[nElement] = new Iterator[nElement] {
            private val entries = ary.iterator
            override def hasNext: Boolean = entries.hasNext
            override def next: nElement = nParser.gson( entries.next )
        }

    }

    /**
     * An nList based on a list
     */
    class Native ( private val data: Seq[nElement] ) extends nList {

        /** Coverts this element to a list */
        override def toList: List[nElement] = data.toList

        /** {@inheritDoc} */
        override private[scalon] def gson: JsonElement = {
            val result = new JsonArray()
            data.foreach { (item) => result.add( item.gson ) }
            result
        }

        /** {@inheritDoc} */
        override def apply ( idx: Int ): nElement = data(idx)

        /** {@inheritDoc} */
        override def length: Int = data.length

        /** {@inheritDoc} */
        override def iterator: Iterator[nElement] = data.iterator

    }

}


