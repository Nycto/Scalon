package com.roundeights.scalon

/**
 * The base class for all nParser Exception
 */
abstract class nException(message: String) extends Exception(message)

/**
 * Parsing Exception
 */
case class nParserException (message: String) extends nException(message)

/**
 * Type mismatch errors
 */
case class nTypeMismatch (
    requested: String, actual: nType.nType
) extends nException (
    "Type mismatch. Expected: %s, Actual: %s".format( requested, actual )
)

/**
 * When a requested nObject key is missing
 */
case class nMissingKey (
    key: String,
    expectedType: String
) extends nException(
    "nObject is missing the '%s' key of type '%s'".format(key, expectedType)
)


