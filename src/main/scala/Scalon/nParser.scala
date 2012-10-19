package com.roundeights.scalon

import com.google.gson.{JsonElement, JsonParser}

/**
 * An interface for parsing
 */
object nParser {

    /**
     * Parses a GSON element
     */
    private[scalon] def gson ( elem: JsonElement ): nElement = {
        if ( elem.isJsonObject )
            new nObject.GSON( elem.getAsJsonObject )
        else if ( elem.isJsonArray )
            new nList.GSON( elem.getAsJsonArray )
        else if ( elem.isJsonNull )
            nNull()
        else if ( elem.isJsonPrimitive ) {
            val primitive = elem.getAsJsonPrimitive
            if ( primitive.isString )
                nString( primitive.getAsString )
            else if ( primitive.isBoolean )
                nBool( primitive.getAsBoolean )
            else if ( primitive.isNumber ) {
                try {
                    new nInt( primitive.getAsBigInteger.toString )
                }
                catch {
                    case _: NumberFormatException
                        => new nFloat( primitive.getAsBigDecimal.toString )
                }
            }
            else
                throw new nException("Unexpected GSON Primitive type")
        }
        else
            throw new nException("Unexpected GSON type")
    }

    /**
     * Parses a JSON string
     */
    def json ( str: String ): nElement = gson( new JsonParser().parse(str) )

    /**
     * Parsers a json element and casts it to an object
     */
    def jsonObj ( str: String ): nObject = json( str ).asObject

    /**
     * Parsers a json element and casts it to an array
     */
    def jsonList ( str: String ): nList = json( str ).asArray

}


