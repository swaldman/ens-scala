/*
 * Distributed as part of ens-scala v.0.0.9
 *
 * Copyright (C) 2019 Machinery For Change, LLC
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php
 *
 */

package com.mchange.sc.v2

import java.net.IDN
import java.nio.charset.StandardCharsets.US_ASCII

import scala.collection._

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{EthAddress, EthHash}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Invoker
import com.mchange.sc.v1.consuela.ethereum.specification.Denominations
import com.mchange.sc.v1.consuela.ethereum.stub.{sol, TransactionInfo}

/**
  *  See https://github.com/ethereum/EIPs/issues/137
  */ 
package object ens extends Denominations {
  class EnsException( message : String, cause : Throwable = null ) extends Exception( message, cause )
  class RevealerIsNotBidderException( revealerAddress : EthAddress, bid : Bid ) extends EnsException( s"Revealer with address '0x${revealerAddress.hex}' cannot reveal bid '${bid}'." )
  class UnexpectedBidStoreStateException( bid : Bid, state : BidStore.State ) extends EnsException(
    "We expect bids to be in state 'Accepted' when they are revealed. " +
      s"Bid '${bid}' is in state '${state}. " +
      "To ignore this and force an attempt to reveal, set 'force = true' when revealing bids."
  )
  class SomeRevealsFailedException( tally : immutable.Seq[Either[FailedReveal,(Bid, TransactionInfo.Base)]] ) extends EnsException( s"At least one attempt to reveal multiple bis has failed. tally: ${tally}" )
  class NoResolverSetException( entity : String ) extends EnsException( s"No resolver set for entity '${entity}'." )
  class OnlyOwnerException( name : String, caller : EthAddress, owner : EthAddress ) extends EnsException( s"Only the owner of name '${name}', 0x${owner.hex}, can call this function. This call by 0x${caller.hex} would fail." )
  class MustBeOwnedException( name : String ) extends EnsException( s"Only the owner of name '${name}' can call this function, but '${name}' has no owner." )

  // bring these into the ens package for convenience
  val  MarkupOrOverride = Invoker.MarkupOrOverride
  type MarkupOrOverride = Invoker.MarkupOrOverride

  val  Markup = Invoker.Markup
  type Markup = Invoker.Markup

  val  Override = Invoker.Override
  type Override = Invoker.Override

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

  def componentHash( component : String ) : EthHash = EthHash.hash( toBytes( component ) )

  def namehash( name : String ) : EthHash = {
    val components = tokenizeReverse( name )
    components.foldLeft( NullHash ) { ( last, next ) =>
      EthHash.hash( last.bytes ++ componentHash( next ).bytes )
    }
  }

  def normalizeNameForTld( name : String, tld : String ) : String = {
    val tldSuffix = s".${tld}".toLowerCase
    def deDotTld( str : String ) = if ( str.endsWith( tldSuffix ) ) str.take( str.length - tldSuffix.length ) else str
    deDotTld( name.toLowerCase ) ensuring ( _.indexOf('.') < 0, s"We expect a simple name (with no '.' characters) or else a <simple-name>.${tld}. Bad name: ${name}." )
  }

  val StandardNameServiceAddress = EthAddress( "0x314159265dd8dbb310642f98f50c066173c1259b" )
  val StandardNameServicePublicResolver = EthAddress( "0x1da022710df5002339274aadee8d58218e9d6ab5" )

  val StandardNameServiceTld = "eth"
  val StandardNameServiceReverseTld = "addr.reverse"

  val ControllerInterfaceId = sol.Bytes4("0x018fac06".decodeHex)
}

