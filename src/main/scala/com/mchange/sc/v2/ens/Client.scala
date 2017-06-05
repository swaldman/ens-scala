package com.mchange.sc.v2.ens

import contract._

import java.time.Instant
import java.time.temporal.ChronoUnit.DAYS

import scala.collection._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash,EthPrivateKey,EthSigner,wallet}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc.Invoker

import com.mchange.sc.v1.consuela.ethereum.stub
import stub.sol
import stub.Sender

object Client {

  val DefaultGasLimitMarkup = Markup( 0.2 ) // a 20% margin over the estimated gas requirement
  val DefaultPollPeriod     = 5.seconds

  def apply(
    ethJsonRpcUrl : String,
    gasPriceTweak : MarkupOrOverride = MarkupOrOverride.None,
    gasLimitTweak : MarkupOrOverride = DefaultGasLimitMarkup,
    pollPeriod    : Duration         = DefaultPollPeriod
  )( implicit econtext : ExecutionContext ) = new Client()( Invoker.Context( ethJsonRpcUrl, gasPriceTweak, gasLimitTweak, pollPeriod ), econtext )
}
class Client( nameServiceAddress : EthAddress = StandardNameServiceAddress, tld : String = "eth", reverseTld : String = "addr.reverse" )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) extends stub.Utilities {

  private lazy val nameService = ENS( nameServiceAddress )

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

  private lazy val registrar : Registrar = new Registrar( owner( tld ).get ) // we assert that it exists

  private lazy val reverseRegistrar : ReverseRegistrar = new ReverseRegistrar( owner( reverseTld ).get ) // we assert that it exists

  def nameStatus( name : String ) : NameStatus = {
    val normalized = normalizeName( name )
    val code = registrar.constant.state( simplehash( normalized ) )( Sender.Default )
    NameStatus.byCode( code.widen )
  }

  def auctionEnd( name : String ) : Option[Instant] = {
    nameStatus( name ) match {
      case NameStatus.Auction => {
        val normalized = normalizeName( name )
        val entries = registrar.constant.entries( simplehash( normalized ) )( Sender.Default )
        Some( Instant.ofEpochSecond( entries._3.widen.toLong ) )
      }
      case _ => None
    }
  }

  def revealStart( name : String ) : Option[Instant] = auctionEnd( name ).map( _.minus( 2, DAYS ) )

  def transferName( from : EthSigner, to : EthAddress, name : String ) : Unit = {
    val normalized = normalizeName( name )

    val txnhash = registrar.transaction.transfer( simplehash( name ), to )( Sender.Basic( from ) )

    requireTransactionReceipt( txnhash ) // just keeping things synchronous... if this returns without error, the call has succeeded
  }

  def releaseName( owner : EthSigner, name : String ) : Unit = {
    val normalized = normalizeName( name )

    val txnhash = registrar.transaction.releaseDeed( simplehash( name ) )( Sender.Basic( owner ) )

    requireTransactionReceipt( txnhash ) // just keeping things synchronous... if this returns without error, the call has succeeded
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

  private lazy val entropy = new java.security.SecureRandom

  private def randomHash : EthHash = {
    val raw = Array.ofDim[Byte]( EthHash.HashLength )
    entropy.nextBytes( raw )
    EthHash.withBytes( raw )
  }

  private def newSalt = randomHash.bytes

  private def sealedBid( normalized : String, bidderAddress : EthAddress, valueInWei : BigInt , salt : immutable.Seq[Byte] ) : EthHash = {

    // this is a constant function that does not make use of a notional sender, Sender.Default is fine
    val bytes32 = registrar.constant.shaBid( simplehash( normalized ), bidderAddress, sol.UInt256( valueInWei ), sol.Bytes32( salt ) )( Sender.Default ) 

    EthHash.withBytes( bytes32.widen )
  }

  private def sealedBid( bid : Bid ) : EthHash = sealedBid( bid.simpleName, bid.bidderAddress, bid.valueInWei, bid.salt )

  def startAuction( from : EthSigner, name : String, numDiversions : Int = 0 ) : Unit = {
    val normalized = normalizeName( name )

    val diversions : Set[EthHash] = immutable.HashSet( (0 until numDiversions).map( _ => randomHash ) : _* )
    val real : EthHash            = EthHash.hash( toBytes( normalized ) )

    val allSeq = (diversions + real).toList.map( hash => sol.Bytes32( hash.bytes ) )

    val txnhash = registrar.transaction.startAuctions( allSeq )( Sender.Basic( from ) )

    requireTransactionReceipt( txnhash ) // just keeping things synchronous... if this returns without error, the call has succeeded
  }

  def newBid( from : EthSigner, name : String, valueInWei : BigInt, overpaymentInWei : BigInt = 0 )( implicit store : BidStore ) : Unit = {
    val normalized = normalizeName( name )
    val saltBytes = randomHash.bytes
    val bidHash = sealedBid( normalized, from.address, valueInWei, saltBytes )
    val bid = Bid( bidHash, normalized, from.address, valueInWei, saltBytes, System.currentTimeMillis )
    store.store( bid ) // no matter what, persist what will be needed to reconstruct the bid hash!

    val txnhash = {
      try {
        registrar.transaction.newBid( sol.Bytes32( bidHash.bytes ), Some( sol.UInt256(valueInWei + overpaymentInWei) ) )( Sender.Basic( from ) )
      } catch {
        case t : Throwable => {
          store.remove( bid )
          throw t
        }
      }
    }

    requireTransactionReceipt( txnhash ) // keeping things synchronous... if this returns without error, the call has succeeded

    store.markAccepted( bidHash )
  }

  def revealBid( from : EthSigner, bidHash : EthHash )( implicit store : BidStore ) : Unit = {
    val (bid, state) = store.findByHash( bidHash ) // will throw if we can't find the bid!

    val txnhash = registrar.transaction.unsealBid( sol.Bytes32( simplehash( bid.simpleName ) ), sol.UInt256( bid.valueInWei ), sol.Bytes32( bid.salt ) )( Sender.Basic( from ) )

    requireTransactionReceipt( txnhash ) // keeping things synchronous... if this returns without error, the call has succeeded

    store.markRevealed( bidHash )
  }

  def revealBid( from : EthSigner, name : String )( implicit store : BidStore ) : Unit = {
    val normalized = normalizeName( name )
    val bids = store.findByName( normalized )

    bids.length match {
      case 0 => throw new EnsException( s"Uh oh. Can't find a stored bid with name '${normalized}'!" )
      case 1 => revealBid( from, bids.head._1.bidHash )( store )
      case 2 => throw new EnsException( s"Multiple bids exist with that name. Please reveal by unique 'bidHash'. Bids: ${bids.map( _._1 )}" )
    }
  }

  def cancelExpiredBid( canceller : EthSigner, lameBidder : EthAddress, bidHash : EthHash ) : Unit = {
      val txnhash = registrar.transaction.cancelBid( lameBidder, sol.Bytes32( bidHash.bytes ) )( Sender.Basic( canceller ) )

    requireTransactionReceipt( txnhash ) // keeping things synchronous... if this returns without error, the call has succeeded
  }

  def finalizeAuction( from : EthSigner, name : String ) : Unit = {
    val normalized = normalizeName( name )

    val txnhash = registrar.transaction.finalizeAuction( simplehash( normalized ) )( Sender.Basic( from ) )

    requireTransactionReceipt( txnhash ) // keeping things synchronous... if this returns without error, the call has succeeded
  }
}
