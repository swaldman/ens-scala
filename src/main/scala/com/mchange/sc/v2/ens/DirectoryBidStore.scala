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

package com.mchange.sc.v2.ens

import java.io.File

import scala.collection._

import scala.io.{Codec,Source}

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash}
import com.mchange.sc.v2.io.RichFile
import com.mchange.sc.v2.lang.borrowExplicit


/**
  *  This is a toy bidstore. You'll probably want something better.
  * 
  *  As the state of the bid changes, we append a later state, without
  *  eliminating prior states. We don't want to risk an overwrite that might
  *  lead to a loss of the bid information.
  */ 
object DirectoryBidStore {
  val BufferSize = 8 * 1024

  val BidFileRegex = """^\p{XDigit}{64}$""".r

  def canBeBidFile( file : File ) = file.isFile && BidFileRegex.findFirstIn( file.getName() ).isDefined

  def bidString( bid : Bid ) = {
    import bid._
    s"${bidHash.hex}|${simpleName}|${bidderAddress.hex}|${valueInWei}|${salt.hex}" + System.lineSeparator
  }
  def parseBidString( line : String ) : Bid = {
    val split = line.split("""\s*\|\s*""")
    Bid( EthHash.withBytes( split(0).decodeHex ), split(1), EthAddress( split(2) ), BigInt( split(3) ), split(4).decodeHexAsSeq )
  }
}
class DirectoryBidStore( dir : File ) extends BidStore {
  import DirectoryBidStore._

  dir.mkdirs()
  require( dir.isDirectory && dir.canWrite, "DirectoryBidStore storage directory must exist or be creatable, and be writable. '${dir}' is not." )

  val removeDir = new File( dir, "removed" )
  removeDir.mkdirs()

  def store( bid : Bid ) : Unit = {
    val bidFile = new File( dir, bid.bidHash.hex )
    bidFile.replaceContents( bidString( bid ) + BidStore.State.Created + "\n", Codec.UTF8 )
  }
  def remove( bid : Bid ) : Unit = {
    val bidFile = new File( dir, bid.bidHash.hex )
    val destFile = new File( removeDir, bid.bidHash.hex )
    bidFile.renameTo( destFile )
  }
  private def mark( bidHash : EthHash, state : BidStore.State ) : Unit = {
    val bidFile = new File( dir, bidHash.hex )

    require( bidFile.exists(), s"Bid with hash '${bidHash.hex}' unknown!" )

    bidFile.appendContents( state.toString + System.lineSeparator )
  }

  def markAccepted( bidHash : EthHash )  : Unit = mark( bidHash, BidStore.State.Accepted )

  def markRevealed( bidHash : EthHash )  : Unit = mark( bidHash, BidStore.State.Revealed )

  private def readFile( bidFile : File ) : ( Bid, BidStore.State ) = {
    borrowExplicit( Source.fromFile( bidFile, BufferSize )( Codec.UTF8 ) )( _.close ) { source =>
      val goodLines = source.getLines().toVector.map( _.trim ).filter( line => !line.startsWith("#") ).filter( _.length > 0 )
      val bid = parseBidString( goodLines.head )
      val state = BidStore.State.fromString( goodLines.last ).get // assert that the state can be found

      ( bid, state )
    }
  }

  def findByHash( bidHash : EthHash ) : ( Bid, BidStore.State ) = {

    val bidFile = new File( dir, bidHash.hex )

    require( bidFile.exists(), s"Bid with hash '${bidHash.hex}' unknown!" )

    readFile( bidFile )
  }

  def findByNameBidderAddress( simpleName : String, bidderAddress : EthAddress ) : immutable.Seq[ ( Bid, BidStore.State ) ] = {
    val rawTuples = dir.listFiles.filter( canBeBidFile ).map( readFile )

    val nameAddressTuples = rawTuples filter { case ( bid, state ) =>
      bid.simpleName == simpleName && bid.bidderAddress == bidderAddress
    }

    nameAddressTuples.toVector
  }
}
