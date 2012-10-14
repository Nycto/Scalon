package com.roundeights.scalon

/**
 * The base class for Scalon exceptions
 */
class nException ( message: String ) extends Exception ( message )

/**
 * Type mismatch errors
 */
case class nTypeMismatch (
    requested: String, actual: nType.nType
) extends nException (
    "Type mismatch. Expected: %s, Actual: %s".format( requested, actual )
)


