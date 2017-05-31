package com.mchange.sc.v2

import java.net.IDN
import java.nio.charset.StandardCharsets.US_ASCII

import com.mchange.sc.v1.consuela.ethereum.EthAddress
import com.mchange.sc.v1.consuela.ethereum.EthHash

/**
  *  See https://github.com/ethereum/EIPs/issues/137
  */ 
package object ens {
  class EnsException( message : String, cause : Throwable = null ) extends Exception( message, cause )

  private val NullHash = EthHash.withBytes( Array.fill[Byte](32)(0.toByte) )

  private def tokenizeReverse( name : String ) : List[String] = {
    val arr = if ( name.length == 0 ) Array.empty[String] else name.split("""\.""")
    val len = arr.length

    def build( nextIndex : Int, accum : List[String] ) : List[String] = {
      nextIndex match {
        case `len` => accum
        case i     => build( i + 1, arr(i) :: accum )
      }
    }

    build(0, Nil)
  }
  def toBytes( nameComponent : String ) = IDN.toASCII( nameComponent, IDN.USE_STD3_ASCII_RULES ).getBytes( US_ASCII )

  def hash( component : String ) : EthHash = EthHash.hash( toBytes( component ) )

  def namehash( name : String ) : EthHash = {
    val components = tokenizeReverse( name )
    components.foldLeft( NullHash ) { ( last, next ) =>
      EthHash.hash( last.bytes ++ hash( next ).bytes )
    }
  }

  val StandardNameServiceAddress = EthAddress( "0x314159265dd8dbb310642f98f50c066173c1259b" )
}

