package com.roundeights.scalon

import com.google.gson.{JsonElement, JsonParser, JsonSyntaxException}
import java.io.File
import scala.io.Source

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
            else {
                throw new nParserException("Unexpected GSON Primitive type")
            }
        }
        else {
            throw new nParserException("Unexpected GSON type")
        }
    }

    /**
     * Parses a JSON string
     */
    def json ( str: String ): nElement = {
        gson( try {
            new JsonParser().parse(str)
        } catch {
            case err: JsonSyntaxException
                => throw new nParserException( err.getMessage )
        })
    }

    /**
     * Parses a JSON File
     */
    def json ( file: File ): nElement = json( Source.fromFile(file).mkString )

    /**
     * Parsers a json element and casts it to an object
     */
    def jsonObj ( str: String ): nObject = json( str ).asObject

    /**
     * Parsers a json element and casts it to an array
     */
    def jsonList ( str: String ): nList = json( str ).asArray

}


