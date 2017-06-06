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
