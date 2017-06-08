/*

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

object AsyncClient {

  val DefaultGasLimitMarkup = Markup( 0.2 ) // a 20% margin over the estimated gas requirement
  val DefaultPollPeriod     = 5.seconds

  def apply(
    ethJsonRpcUrl : String,
    gasPriceTweak : MarkupOrOverride = MarkupOrOverride.None,
    gasLimitTweak : MarkupOrOverride = DefaultGasLimitMarkup,
    pollPeriod    : Duration         = DefaultPollPeriod
  )( implicit econtext : ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global ) = new Client()( Invoker.Context( ethJsonRpcUrl, gasPriceTweak, gasLimitTweak, pollPeriod ), econtext )
}
class AsyncClient( nameServiceAddress : EthAddress = StandardNameServiceAddress, tld : String = "eth", reverseTld : String = "addr.reverse" )( implicit icontext : Invoker.Context, econtext : ExecutionContext )
    extends stub.Utilities {

  private lazy val nameService = AsyncENS( nameServiceAddress )

  private lazy val registrar : AsyncRegistrar = new AsyncRegistrar( owner( tld ).get ) // we assert that it exists

  private lazy val reverseRegistrar : AsyncReverseRegistrar = new AsyncReverseRegistrar( owner( reverseTld ).get ) // we assert that it exists

  private def stubnamehash( name : String ) : sol.Bytes32 = sol.Bytes32( namehash( name ).bytes )

  private def _simplehash( str : String ) = EthHash.hash( toBytes( str ) )

  private def stubsimpleshash( str : String ) = sol.Bytes32( _simplehash( str ).bytes )

  private def ethsigner    [T : EthSigner.Source] ( source : T ) : EthSigner  = implicitly[EthSigner.Source[T]].toEthSigner(source)

  private def ethaddress[T : EthAddress.Source]( source : T ) : EthAddress = implicitly[EthAddress.Source[T]].toEthAddress(source)

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

  def owner( name : String ) : Future[Option[EthAddress]] = {
    val fraw = nameService.constant.owner( stubnamehash( name ) )( Sender.Default )
    fraw map { raw => if ( raw == EthAddress.Zero ) None else Some( raw ) }
  }

  def setOwner[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, address : T ) : Future[EthHash] = {
    nameService.transaction.setOwner( stubnamehash( name ), ethaddress(address) )( Sender.Basic( ethsigner(signer) ) )
  }

  def ttl( name : String ) : Future[JDuration] = {
    val fraw = nameService.constant.ttl( stubnamehash( name ) )( Sender.Default )
    fraw map { raw => JDuration.ofSeconds( raw.widen.toValidLong ) }
  }

  def setTTL[S : EthSigner.Source]( signer : S, name : String, ttl : Long ) : Future[EthHash] = {
    nameService.transaction.setTTL( stubnamehash( name ), sol.UInt64( ttl ) )( Sender.Basic( ethsigner(signer) ) )
  }

  def setResolver[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, resolver : T ) : Future[EthHash] = {
     nameService.transaction.setResolver( stubnamehash( name ), ethaddress( resolver ) )( Sender.Basic( ethsigner(signer) ) )
  }

  def resolver( name : String ) : Future[Option[EthAddress]] = {
    val fraw = nameService.constant.resolver( stubnamehash( name ) )( Sender.Default )
    fraw map { raw => if ( raw == EthAddress.Zero ) None else Some( raw ) }
  }

  def address( name : String ) : Future[Option[EthAddress]] = {
    val stubhash = stubnamehash( name )
    val fResolverAddress = nameService.constant.resolver( stubhash )( Sender.Default )
    fResolverAddress flatMap { 
      case EthAdress.Zero  => Future.successful( None )
      case resolverAddress => {
        val nameResolver = new AsyncResolver( resolverAddress )
        aresolver.constant.addr( stubhash )( Sender.Default ) map { 
          case EthAdress.Zero  => Future.successful( None )
          case theAddress      => Some( theAddress )
        }
      }
    }
  }

  def setAddress[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, address : T ) : Future[EthHash] = {
    val stubhash = stubnamehash( name )
    val _address = ethaddress( address )

    val fResolverAddress = nameService.constant.resolver( stubhash )( Sender.Default )
    fResolverAddress flatMap {
      case EthAdress.Zero  => throw new NoResolverSetException( name )
      case resolverAddress => resolver.transaction.setAddr( stubnodehash, _address )( Sender.Basic( ethsigner(signer) ) )
    }
  }

  def nameStatus( name : String ) : Future[NameStatus] = {
    val normalized = normalizeName( name )
    val fcode = registrar.constant.state( stubsimpleshash( normalized ) )( Sender.Default )
    fcode map { code => NameStatus.byCode( code.widen ) }
  }

  def ownedNotFinalized( name : String ) : Future[Boolean] = {
    for {
      status <- nameStatus( name );
      o      <- owner( name )
    } yield {
      status == NameStatus.Owned && o.isEmpty
    }
  }

  def auctionEnd( name : String ) : Future[Option[Instant]] = {
    nameStatus( name ) flatMap { status =>
      status match {
        case NameStatus.Auction | NameStatus.Reveal => {
          val normalized = normalizeName( name )
          val fentries = registrar.constant.entries( stubsimpleshash( normalized ) )( Sender.Default )
          fentries.map( entries => Some( Instant.ofEpochSecond( entries._3.widen.toLong ) ) )
        }
        case _ => Future.successful( None )
      }
    }
  }

  def revealStart( name : String ) : Future[Option[Instant]] = auctionEnd( name ).map { mbInstant =>
    mbInstant.map( _.minus( 2, DAYS ) )
  }

  def whenAvailable( name : String ) : Future[Instant] = {
    val normalized = normalizeName( name )
    val fsecond = registrar.constant.getAllowedTime( stubsimpleshash( normalized ) )( Sender.Default )
    fsecond.map( second => Instant.ofEpochSecond( second.widen.toValidLong ) )
  }

  def transferDeed[S : EthSigner.Source, T : EthAddress.Source]( from : S, to : T, name : String ) : Future[EthHash] = {
    val normalized = normalizeName( name )
    registrar.transaction.transfer( stubsimpleshash( normalized ), ethaddress(to) )( Sender.Basic( ethsigner(from) ) )
  }

  def releaseDeed[S : EthSigner.Source]( owner : S, name : String ) : Future[EthHash] = {
    val normalized = normalizeName( name )
    registrar.transaction.releaseDeed( stubsimpleshash( normalized ) )( Sender.Basic( ethsigner(owner) ) )
  }

  // note that at the auction registrar, standard Ethereum Keccak hashes, rather than namehashes, are used

  private def sealedBid( normalized : String, bidderAddress : EthAddress, valueInWei : BigInt , salt : immutable.Seq[Byte] ) : Future[EthHash] = {

    // this is a constant function that does not make use of a notional sender, Sender.Default is fine
    val fbytes32 = registrar.constant.shaBid( stubsimpleshash( normalized ), bidderAddress, sol.UInt256( valueInWei ), sol.Bytes32( salt ) )( Sender.Default ) 

    fbytes32.map( bytes32 => EthHash.withBytes( bytes32.widen ) )
  }

  private def sealedBid( bid : Bid ) : Future[EthHash] = sealedBid( bid.simpleName, bid.bidderAddress, bid.valueInWei, bid.salt )

  def startAuction[T : EthSigner.Source]( from : T, name : String, numDiversions : Int = 0 ) : Future[EthHash] = {
    val normalized = normalizeName( name )

    val diversions : Set[EthHash] = immutable.HashSet( (0 until numDiversions).map( _ => randomHash ) : _* )
    val real : EthHash            = _simplehash( normalized )

    val allSeq = (diversions + real).toList.map( hash => sol.Bytes32( hash.bytes ) )

    registrar.transaction.startAuctions( allSeq )( Sender.Basic( ethsigner(from) ) )
  }

  def createRawBid[T : EthAddress.Source]( fromAddress : T, name : String, valueInWei : BigInt ) : Future[Bid] = {
    val _from = ethaddress( fromAddress )
    val normalized = normalizeName( name )
    val saltBytes = randomHash.bytes
    val fBidHash = sealedBid( normalized, _from, valueInWei, saltBytes )
    fBidHash.map( bidHash => Bid( bidHash, normalized, _from, valueInWei, saltBytes ) )
  }

  def placeNewBid[T : EthSigner.Source]( bidder : T, name : String, valueInWei : BigInt, overpaymentInWei : BigInt = 0 )( implicit store : BidStore ) : Future[(EthHash, Bid)] = {
    val _bidder = ethsigner( bidder )
    val bid : Bid = createRawBid( _bidder.address, name, valueInWei )

    val ftxnhash = _placeRawBid( _bidder, bid, overpaymentInWei, Some( store ) )

    ftxnhash.map( txnhash => ( txnhash, bid ) )
  }

  // XXX: Get rid or -- or more likely standardize! -- the internal poll
  private def _placeRawBid[T : EthSigner.Source]( bidder : T, bid : Bid, overpaymentInWei : BigInt, mbStore : Option[BidStore] ) : Future[EthHash] = {
    Future.successful( ethsigner( bidder ) ) map { _bidder =>
      mbStore.foreach( _.store( bid ) ) // no matter what, persist what will be needed to reconstruct the bid hash!

      try {
        registrar.transaction.newBid( sol.Bytes32( bid.bidHash.bytes ), Some( sol.UInt256(bid.valueInWei + overpaymentInWei) ) )( Sender.Basic( _bidder ) )
      } catch {
        case t : Throwable => {
          mbStore.foreach{ _.remove( bid ) }
          throw t
        }
      }
      requireTransactionReceipt( txnhash ) // keeping things synchronous... if this returns without error, the call has succeeded

      mbStore.foreach( _.markAccepted( bid.bidHash ) )
    }
  }

  def placeRawBid[T : EthSigner.Source]( bidder : T, bid : Bid, overpaymentInWei : Int = 0 ) : Future[EthHash] = {
    val _bidder = ethsigner(bidder)
    if ( _bidder.address != bid.bidderAddress ) {
      throw new EnsException( s"Bidder '0x${_bidder.address.hex}' does not match address in Bid ('0x${bid.bidderAddress.hex}'). Cannot place bid." )
    } else {
      _placeRawBid( _bidder, bid, overpaymentInWei, None )
    }
  }

  def revealRawBid[T : EthSigner.Source]( bidder : T, bid : Bid ) : Future[EthHash] = {
    val _bidder = ethsigner(bidder)
    if ( _bidder.address != bid.bidderAddress ) {
      throw new EnsException( s"Bidder '0x${_bidder.address.hex}' does not match address in Bid ('0x${bid.bidderAddress.hex}'). Cannot reveal bid." )
    } else {
      _revealRawBid( _bidder, bid )
    }
  }

  private def _revealRawBid[T : EthSigner.Source]( bidder : T, bid : Bid ) : Future[EthHash] = {
    revealRawBid( _simplehash( bid.simpleName ), bidder, bid.valueInWei, bid.salt )
  }

  def revealRawBid[T : EthSigner.Source]( nodeHash : EthHash, bidder : T, valueInWei : BigInt, salt : immutable.Seq[Byte] ) : Future[EthHash] = {
    val _bidder = ethsigner( bidder )
    registrar.transaction.unsealBid( sol.Bytes32( nodeHash.bytes ), sol.UInt256( valueInWei ), sol.Bytes32( salt ) )( Sender.Basic( _bidder ) )
  }

  // XXX: Get rid or -- or more likely standardize! -- the internal poll
  def revealBid[T : EthSigner.Source]( from : T, bidHash : EthHash, force : Boolean )( implicit store : BidStore ) : Future[EthHash] = {
    val prepare = Future {
      val (bid, state) = store.findByHash( bidHash ) // will throw if we can't find the bid!

      val _from = ethsigner( from )
      val revealerAddress = _from.address
      if (_from.address != bid.bidderAddress ) {
        throw new RevealerIsNotBidderException( revealerAddress, bid )
      }
      if ( !force && state != BidStore.State.Accepted ) {
        throw new UnexpectedBidStoreStateException( bid, state )
      }
      ( _from, bid )
    }
    val txnhash = prepare flatMap { case ( _from, bid ) =>
      _revealRawBid( _from, bid )
    }
    requireTransactionReceipt( txnhash ) // keeping things synchronous... if this returns without error, the call has succeeded

    store.markRevealed( bidHash )
  }

  def revealBid[T : EthSigner.Source]( from : T, name : String, force : Boolean = false )( implicit store : BidStore ) : Future[EthHash] = {
    val _from = ethsigner( from )
    val normalized = normalizeName( name )
    val bids = store.findByNameBidderAddress( normalized, _from.address )

    bids.length match {
      case 0 => throw new EnsException( s"Uh oh. Can't find a stored bid with name '${normalized}'!" )
      case 1 => revealBid( _from, bids.head._1.bidHash, force )
      case 2 => throw new EnsException( s"Multiple bids exist with that name. Please reveal by unique 'bidHash'. Bids: ${bids.map( _._1 )}" )
    }
  }

  // We were here!
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

*/ 
