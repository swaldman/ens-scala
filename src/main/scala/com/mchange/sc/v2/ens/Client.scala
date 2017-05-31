package com.mchange.sc.v2.ens

import contract._

import scala.collection._
import scala.concurrent.ExecutionContext

import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash,EthSigner}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc20.Invoker

import com.mchange.sc.v1.consuela.ethereum.ethabi.stub
import stub.sol
import stub.Sender

object Client {
  def apply( ethJsonRpcUrl : String )( implicit econtext : ExecutionContext ) = new Client()( Invoker.Context( ethJsonRpcUrl ), econtext )
}
class Client( nameServiceAddress : EthAddress = StandardNameServiceAddress, tld : String = "eth", reverseTld : String = "addr.reverse" )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) extends stub.Utilities {
  lazy val nameService = ENS( nameServiceAddress )

  private def stubnamehash( name : String ) : sol.Bytes32 = sol.Bytes32( namehash( name ).bytes )

  private def simplehash( str : String ) = sol.Bytes32( EthHash.hash( toBytes( str ) ).bytes )

  def owner( name : String ) : Option[EthAddress] = {
    val raw = nameService.constant.owner( stubnamehash( name ) )( Sender.Default )
    if ( raw == EthAddress.Zero ) None else Some( raw )
  }

  def resolver( name : String ) : Option[EthAddress] = {
    val raw = nameService.constant.resolver( stubnamehash( name ) )( Sender.Default )
    if ( raw == EthAddress.Zero ) None else Some( raw )
  }

  def address( name : String ) : Option[EthAddress] = {
    resolver( name ) flatMap { resolverAddr =>
      val r = new Resolver( resolverAddr )
      val raw = r.constant.addr( stubnamehash( name ) )( Sender.Default ) // yes, this needlessly hashes twice
      if ( raw == EthAddress.Zero ) None else Some( raw )
    }
  }

  lazy val registrar : Registrar = new Registrar( owner( tld ).get ) // we assert that it exists

  lazy val reverseRegistrar : ReverseRegistrar = new ReverseRegistrar( owner( reverseTld ).get ) // we assert that it exists

  def nameStatus( simpleName : String ) : NameStatus = {
    val normalized = normalizeName( simpleName )
    val code = registrar.constant.state( simplehash( normalized ) )( Sender.Default )
    NameStatus.byCode( code.widen )
  }

  // note that at the auction registrar, standard Ethereum Keccak hashes, rather than namehashes, are used

  private val tldSuffix    = s".${tld}".toLowerCase

  private def requireSimpleName( simpleName : String ) = {
    require( simpleName.indexOf('.') < 0, s"The registrar auctions simple names in the '.${tld}' domain. These may not contain a '.'. (Bad Name: '${simpleName}'.)" )
  }

  private def normalizeName( name : String ) = {
    def deDotTld( str : String ) = if ( str.endsWith( tldSuffix ) ) str.take( str.length - tldSuffix.length ) else str
    deDotTld( name.toLowerCase ) ensuring ( _.indexOf('.') < 0, s"We expect a simple name (with no '.' characters) or else a <simple-name>.${tld}. Bad name: ${name}." )
  }

  class Auctioneer( bidder : EthSigner, store : BidStore ) {

    private implicit val sender = stub.Sender.Basic( bidder )

    private lazy val entropy = new java.security.SecureRandom

    private def randomHash : EthHash = {
      val raw = Array.ofDim[Byte]( EthHash.HashLength )
      entropy.nextBytes( raw )
      EthHash.withBytes( raw )
    }

    def startAuction( name : String, numDiversions : Int = 0 ) : Unit = {
      val normalized = normalizeName( name )

      val diversions : Set[EthHash] = immutable.HashSet( (0 until numDiversions).map( _ => randomHash ) : _* )
      val real : EthHash            = EthHash.hash( toBytes( normalized ) )

      val allSeq = (diversions + real).toList.map( hash => sol.Bytes32( hash.bytes ) )

      val hash = registrar.transaction.startAuctions( allSeq )

      requireTransactionReceipt( hash ) // just keeping things synchronous... if this returns without error, the call has succeeded
    }

    def newSalt = randomHash.bytes

    private def sealedBid( normalized : String, valueInWei : BigInt , salt : immutable.Seq[Byte] ) : EthHash = {
      val address = sender.address
      val bytes32 = registrar.constant.shaBid( simplehash( normalized ), sender.address, sol.UInt256( valueInWei ), sol.Bytes32( salt ) )
      EthHash.withBytes( bytes32.widen )
    }

    def newBid( name : String, valueInWei : BigInt, overpaymentInWei : BigInt = 0 ) : Unit = {
      val normalized = normalizeName( name )
      val salt = randomHash
      val bid = sealedBid( normalized, valueInWei, salt.bytes )
    }
  }
}
