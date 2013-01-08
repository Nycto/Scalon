package com.roundeights.scalon

/**
 * Parsing Exception
 */
case class nParserException (message: String) extends Exception (message)

/**
 * Type mismatch errors
 */
case class nTypeMismatch (
    requested: String, actual: nType.nType
) extends Exception (
    "Type mismatch. Expected: %s, Actual: %s".format( requested, actual )
)

/**
 * When a requested nObject key is missing
 */
case class nMissingKey (
    key: String,
    expectedType: String
) extends NoSuchElementException (
    "nObject is missing the '%s' key of type '%s'".format(key, expectedType)
)


