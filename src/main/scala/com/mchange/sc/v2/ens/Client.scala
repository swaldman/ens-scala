package com.mchange.sc.v2.ens

import contract._

import java.time.{Duration => JDuration, Instant}
import java.time.temporal.ChronoUnit.DAYS

import scala.collection._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.control.NonFatal

import com.mchange.sc.v1.consuela._
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
  )( implicit econtext : ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global ) = new Client()( Invoker.Context( ethJsonRpcUrl, gasPriceTweak, gasLimitTweak, pollPeriod ), econtext )
}
class Client( nameServiceAddress : EthAddress = StandardNameServiceAddress, tld : String = "eth", reverseTld : String = "addr.reverse" )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) extends stub.Utilities {

  private lazy val nameService = ENS( nameServiceAddress )

  private def stubnamehash( name : String ) : sol.Bytes32 = sol.Bytes32( namehash( name ).bytes )

  private def _simplehash( str : String ) = EthHash.hash( toBytes( str ) )

  private def stubsimpleshash( str : String ) = sol.Bytes32( _simplehash( str ).bytes )

  private def ethsigner    [T : EthSigner.Source] ( source : T ) : EthSigner  = implicitly[EthSigner.Source[T]].toEthSigner(source)

  private def ethaddress[T : EthAddress.Source]( source : T ) : EthAddress = implicitly[EthAddress.Source[T]].toEthAddress(source)

  private lazy val registrar : Registrar = new Registrar( owner( tld ).get ) // we assert that it exists

  private lazy val reverseRegistrar : ReverseRegistrar = new ReverseRegistrar( owner( reverseTld ).get ) // we assert that it exists

  def owner( name : String ) : Option[EthAddress] = {
    val raw = nameService.constant.owner( stubnamehash( name ) )( Sender.Default )
    if ( raw == EthAddress.Zero ) None else Some( raw )
  }

  def setOwner[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, address : T ) : Unit = {
    val txnhash = nameService.transaction.setOwner( stubnamehash( name ), ethaddress(address) )( Sender.Basic( ethsigner(signer) ) )
    requireTransactionReceipt( txnhash )
  }

  def ttl( name : String ) : JDuration = {
    val raw = nameService.constant.ttl( stubnamehash( name ) )( Sender.Default )
    JDuration.ofSeconds( raw.widen.toValidLong )
  }

  def setTTL[S : EthSigner.Source]( signer : S, name : String, ttl : Long ) : Unit = {
    val txnhash = nameService.transaction.setTTL( stubnamehash( name ), sol.UInt64( ttl ) )( Sender.Basic( ethsigner(signer) ) )
    requireTransactionReceipt( txnhash )
  }

  private def _resolver( stubnodehash : sol.Bytes32 ) : Option[EthAddress] = {
    val raw = nameService.constant.resolver( stubnodehash )( Sender.Default )
    if ( raw == EthAddress.Zero ) None else Some( raw )
  }

  def resolver( name : String ) : Option[EthAddress] = _resolver( stubnamehash( name ) )

  def setResolver[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, resolver : T ) : Unit = {
    val txnhash = nameService.transaction.setResolver( stubnamehash( name ), ethaddress( resolver ) )( Sender.Basic( ethsigner(signer) ) )
    requireTransactionReceipt( txnhash )
  }

  private def withResolver[T]( name : String )( op : ( sol.Bytes32, Resolver ) => Option[T] ) = {
    val stubnodehash = stubnamehash( name )
    _resolver( stubnodehash ) flatMap { resolverAddr =>
      val resolver = new Resolver( resolverAddr )
      op( stubnodehash, resolver )
    }
  }

  def address( name : String ) : Option[EthAddress] = {
    withResolver( name ){ ( stubnodehash, resolver ) =>
      val raw = resolver.constant.addr( stubnodehash )( Sender.Default )
      if ( raw == EthAddress.Zero ) None else Some( raw )
    }
  }

  def setAddress[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, address : T ) : Unit = {
    val _address = ethaddress( address )
    val mbTxnhash = withResolver( name ){ ( stubnodehash, resolver ) =>
      Some( resolver.transaction.setAddr( stubnodehash, _address )( Sender.Basic( ethsigner(signer) ) ) )
    }
    mbTxnhash match {
      case Some( txnhash ) => requireTransactionReceipt( txnhash ) // just keeping things synchronous... if this returns without error, the call has succeeded
      case _ => throw new NoResolverSetException( name )
    }
  }

  def nameStatus( name : String ) : NameStatus = {
    val normalized = normalizeName( name )
    val code = registrar.constant.state( stubsimpleshash( normalized ) )( Sender.Default )
    NameStatus.byCode( code.widen )
  }

  def ownedNotFinalized( name : String ) : Boolean = {
    nameStatus( name ) == NameStatus.Owned && owner( name ).isEmpty 
  }

  def auctionEnd( name : String ) : Option[Instant] = {
    nameStatus( name ) match {
      case NameStatus.Auction | NameStatus.Reveal => {
        val normalized = normalizeName( name )
        val entries = registrar.constant.entries( stubsimpleshash( normalized ) )( Sender.Default )
        Some( Instant.ofEpochSecond( entries._3.widen.toLong ) )
      }
      case _ => None
    }
  }

  def revealStart( name : String ) : Option[Instant] = auctionEnd( name ).map( _.minus( 2, DAYS ) )

  def whenAvailable( name : String ) : Instant = {
    val normalized = normalizeName( name )
    val second = registrar.constant.getAllowedTime( stubsimpleshash( normalized ) )( Sender.Default )
    Instant.ofEpochSecond( second.widen.toValidLong )
  }

  def transferDeed[S : EthSigner.Source, T : EthAddress.Source]( from : S, to : T, name : String ) : Unit = {
    val normalized = normalizeName( name )

    val txnhash = registrar.transaction.transfer( stubsimpleshash( normalized ), ethaddress(to) )( Sender.Basic( ethsigner(from) ) )

    requireTransactionReceipt( txnhash ) // just keeping things synchronous... if this returns without error, the call has succeeded
  }

  def releaseDeed[S : EthSigner.Source]( owner : S, name : String ) : Unit = {
    val normalized = normalizeName( name )

    val txnhash = registrar.transaction.releaseDeed( stubsimpleshash( normalized ) )( Sender.Basic( ethsigner(owner) ) )

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
    val bytes32 = registrar.constant.shaBid( stubsimpleshash( normalized ), bidderAddress, sol.UInt256( valueInWei ), sol.Bytes32( salt ) )( Sender.Default ) 

    EthHash.withBytes( bytes32.widen )
  }

  private def sealedBid( bid : Bid ) : EthHash = sealedBid( bid.simpleName, bid.bidderAddress, bid.valueInWei, bid.salt )

  def startAuction[T : EthSigner.Source]( from : T, name : String, numDiversions : Int = 0 ) : Unit = {
    val normalized = normalizeName( name )

    val diversions : Set[EthHash] = immutable.HashSet( (0 until numDiversions).map( _ => randomHash ) : _* )
    val real : EthHash            = _simplehash( normalized )

    val allSeq = (diversions + real).toList.map( hash => sol.Bytes32( hash.bytes ) )

    val txnhash = registrar.transaction.startAuctions( allSeq )( Sender.Basic( ethsigner(from) ) )

    requireTransactionReceipt( txnhash ) // just keeping things synchronous... if this returns without error, the call has succeeded
  }

  def createRawBid[T : EthAddress.Source]( fromAddress : T, name : String, valueInWei : BigInt ) : Bid = {
    val _from = ethaddress( fromAddress )
    val normalized = normalizeName( name )
    val saltBytes = randomHash.bytes
    val bidHash = sealedBid( normalized, _from, valueInWei, saltBytes )
    Bid( bidHash, normalized, _from, valueInWei, saltBytes )
  }

  def placeNewBid[T : EthSigner.Source]( bidder : T, name : String, valueInWei : BigInt, overpaymentInWei : BigInt = 0 )( implicit store : BidStore ) : Bid = {
    val _bidder = ethsigner( bidder )
    val bid : Bid = createRawBid( _bidder.address, name, valueInWei )

    _placeRawBid( _bidder, bid, overpaymentInWei, Some( store ) )

    bid
  }

  private def _placeRawBid[T : EthSigner.Source]( bidder : T, bid : Bid, overpaymentInWei : BigInt, mbStore : Option[BidStore] ) : Unit = {
    val _bidder = ethsigner( bidder )

    mbStore.foreach( _.store( bid ) ) // no matter what, persist what will be needed to reconstruct the bid hash!

    val txnhash = {
      try {
        registrar.transaction.newBid( sol.Bytes32( bid.bidHash.bytes ), Some( sol.UInt256(bid.valueInWei + overpaymentInWei) ) )( Sender.Basic( _bidder ) )
      } catch {
        case t : Throwable => {
          mbStore.foreach{ _.remove( bid ) }
          throw t
        }
      }
    }

    requireTransactionReceipt( txnhash ) // keeping things synchronous... if this returns without error, the call has succeeded

    mbStore.foreach( _.markAccepted( bid.bidHash ) )
  }

  def placeRawBid[T : EthSigner.Source]( bidder : T, bid : Bid, overpaymentInWei : Int = 0 ) : Unit = {
    val _bidder = ethsigner(bidder)
    if ( _bidder.address != bid.bidderAddress ) {
      throw new EnsException( s"Bidder '0x${_bidder.address.hex}' does not match address in Bid ('0x${bid.bidderAddress.hex}'). Cannot place bid." )
    } else {
      _placeRawBid( _bidder, bid, overpaymentInWei, None )
    }
  }

  def revealRawBid[T : EthSigner.Source]( bidder : T, bid : Bid ) : Unit = {
    val _bidder = ethsigner(bidder)
    if ( _bidder.address != bid.bidderAddress ) {
      throw new EnsException( s"Bidder '0x${_bidder.address.hex}' does not match address in Bid ('0x${bid.bidderAddress.hex}'). Cannot reveal bid." )
    } else {
      _revealRawBid( _bidder, bid )
    }
  }

  private def _revealRawBid[T : EthSigner.Source]( bidder : T, bid : Bid ) : Unit = {
    revealRawBid( _simplehash( bid.simpleName ), bidder, bid.valueInWei, bid.salt )
  }

  def revealRawBid[T : EthSigner.Source]( nodeHash : EthHash, bidder : T, valueInWei : BigInt, salt : immutable.Seq[Byte] ) : Unit = {
    val _bidder = ethsigner( bidder )
    val txnhash = registrar.transaction.unsealBid( sol.Bytes32( nodeHash.bytes ), sol.UInt256( valueInWei ), sol.Bytes32( salt ) )( Sender.Basic( _bidder ) )
    requireTransactionReceipt( txnhash ) // keeping things synchronous... if this returns without error, the call has succeeded
  }

  def revealBid[T : EthSigner.Source]( from : T, bidHash : EthHash, force : Boolean )( implicit store : BidStore ) : Unit = {
    val (bid, state) = store.findByHash( bidHash ) // will throw if we can't find the bid!

    val _from = ethsigner( from )
    val revealerAddress = _from.address
    if (_from.address != bid.bidderAddress ) {
      throw new RevealerIsNotBidderException( revealerAddress, bid )
    }
    if ( !force && state != BidStore.State.Accepted ) {
      throw new UnexpectedBidStoreStateException( bid, state )
    }

    _revealRawBid( _from, bid )

    store.markRevealed( bidHash )
  }

  def revealBid[T : EthSigner.Source]( from : T, name : String, force : Boolean = false )( implicit store : BidStore ) : Unit = {
    val _from = ethsigner( from )
    val normalized = normalizeName( name )
    val bids = store.findByNameBidderAddress( normalized, _from.address )

    bids.length match {
      case 0 => throw new EnsException( s"Uh oh. Can't find a stored bid with name '${normalized}'!" )
      case 1 => revealBid( _from, bids.head._1.bidHash, force )
      case 2 => throw new EnsException( s"Multiple bids exist with that name. Please reveal by unique 'bidHash'. Bids: ${bids.map( _._1 )}" )
    }
  }

  def revealAllBids[T : EthSigner.Source]( from : T, name : String, force : Boolean = false )( implicit store : BidStore ) : Unit = {
    val _from = ethsigner( from )
    val normalized = normalizeName( name )
    val bids = store.findByNameBidderAddress( normalized, _from.address )

    if ( bids.length == 0 ) {
      throw new EnsException( s"No bids were found with name '${normalized}' for bidder '${_from.address} in bid store '${store}'." ) 
    }

    val tally = {
      bids map { case ( bid, state ) => 
        try {
          Right{
            revealBid( _from, bid.bidHash, force )
            bid
          }
        }
        catch {
          case NonFatal( t ) => Left( FailedReveal( t, bid ) )
        }
      }
    }
    if ( tally.forall( _.isRight ) ) {
      tally map {
        case Right( bid ) => bid
        case other => throw new InternalError( s"Huh? We just checked that these were all Rights, yet... ${tally}" ) // should never happen
      }
    }
    else if ( tally.size == 1 ) {
      val Left( failedReveal ) = tally.head // at least one failed and there's only one, it must be the first, it must be a left
      throw failedReveal.failure
    }
    else {
      throw new SomeRevealsFailedException( tally )
    }
  }

  def cancelExpiredBid[S : EthSigner.Source, T : EthAddress.Source] ( canceller : S, lameBidder : T, bidHash : EthHash ) : Unit = {
    val txnhash = registrar.transaction.cancelBid( ethaddress( lameBidder ), sol.Bytes32( bidHash.bytes ) )( Sender.Basic( ethsigner(canceller) ) )

    requireTransactionReceipt( txnhash ) // keeping things synchronous... if this returns without error, the call has succeeded
  }

  def finalizeAuction[T : EthSigner.Source]( from : T, name : String ) : Unit = {
    val normalized = normalizeName( name )

    val txnhash = registrar.transaction.finalizeAuction( stubsimpleshash( normalized ) )( Sender.Basic( ethsigner(from) ) )

    requireTransactionReceipt( txnhash ) // keeping things synchronous... if this returns without error, the call has succeeded
  }
}
