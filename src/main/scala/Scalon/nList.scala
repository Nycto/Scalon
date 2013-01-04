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

}

/**
 * Companion
 */
object nList {

    /**
     * Creates a new nList
     */
    def apply ( source: List[nElement] = List() ): nList
        = new nList.Native( source )

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
    class Native ( override val toList: List[nElement] ) extends nList {

        /** {@inheritDoc} */
        override private[scalon] def gson: JsonElement = {
            val result = new JsonArray()
            toList.foreach { (item) => result.add( item.gson ) }
            result
        }

        /** {@inheritDoc} */
        override def apply ( idx: Int ): nElement = toList(idx)

        /** {@inheritDoc} */
        override def length: Int = toList.length

        /** {@inheritDoc} */
        override def iterator: Iterator[nElement] = toList.iterator

    }

}


