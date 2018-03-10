package com.mchange.sc.v2.ens

import contract._

import java.time.{Duration => JDuration, Instant}
import java.time.temporal.ChronoUnit.DAYS

import scala.collection._
import scala.concurrent.{Await,Future,ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Try,Success,Failure}
import scala.util.control.NonFatal

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash,EthPrivateKey,EthSigner,wallet}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.stub
import stub.{sol, Sender, TransactionInfo}

import com.mchange.sc.v2.concurrent.{Poller, Scheduler}
import com.mchange.sc.v2.net.URLSource

object AsyncClient {

  def apply[ U : URLSource ](
    jsonRpcUrl          : U,
    nameServiceAddress  : EthAddress               = StandardNameServiceAddress,
    tld                 : String                   = StandardNameServiceTld,
    reverseTld          : String                   = StandardNameServiceReverseTld,
    gasPriceTweak       : stub.MarkupOrOverride    = stub.Context.Default.GasPriceTweak,
    gasLimitTweak       : stub.MarkupOrOverride    = stub.Context.Default.GasLimitTweak,
    pollPeriod          : Duration                 = stub.Context.Default.PollPeriod,
    pollTimeout         : Duration                 = stub.Context.Default.PollTimeout,
    transactionApprover : stub.TransactionApprover = stub.Context.Default.TransactionApprover,
    transactionLogger   : stub.TransactionLogger   = stub.Context.Default.TransactionLogger,
    eventConfirmations  : Int                      = stub.Context.Default.EventConfirmations
  )( implicit
    cfactory  : jsonrpc.Client.Factory = stub.Context.Default.ClientFactory,
    poller    : Poller                 = stub.Context.Default.Poller,
    scheduler : Scheduler              = stub.Context.Default.Scheduler,
    econtext  : ExecutionContext       = stub.Context.Default.ExecutionContext
  ) = {
    val scontext = stub.Context.fromUrl(
      jsonRpcUrl          = jsonRpcUrl,
      gasPriceTweak       = gasPriceTweak,
      gasLimitTweak       = gasLimitTweak,
      pollPeriod          = pollPeriod,
      pollTimeout         = pollTimeout,        
      transactionApprover = transactionApprover,        
      transactionLogger   = transactionLogger,
      eventConfirmations  = eventConfirmations
    )( implicitly[URLSource[U]], cfactory, poller, scheduler, econtext )
    new AsyncClient( nameServiceAddress, tld, reverseTld )( scontext )
  }

  final object LoadBalanced {
    def apply[ U : URLSource ](
      jsonRpcUrls         : immutable.Iterable[U],
      nameServiceAddress  : EthAddress               = StandardNameServiceAddress,
      tld                 : String                   = StandardNameServiceTld,
      reverseTld          : String                   = StandardNameServiceReverseTld,
      gasPriceTweak       : stub.MarkupOrOverride    = stub.Context.Default.GasPriceTweak,
      gasLimitTweak       : stub.MarkupOrOverride    = stub.Context.Default.GasLimitTweak,
      pollPeriod          : Duration                 = stub.Context.Default.PollPeriod,
      pollTimeout         : Duration                 = stub.Context.Default.PollTimeout,
      transactionApprover : stub.TransactionApprover = stub.Context.Default.TransactionApprover,
      transactionLogger   : stub.TransactionLogger   = stub.Context.Default.TransactionLogger,
      eventConfirmations  : Int                      = stub.Context.Default.EventConfirmations
    )( implicit
      cfactory  : jsonrpc.Client.Factory  = stub.Context.Default.ClientFactory,
      poller    : Poller                  = stub.Context.Default.Poller,
      scheduler : Scheduler               = stub.Context.Default.Scheduler,
      econtext  : ExecutionContext        = stub.Context.Default.ExecutionContext
    ) = {
      val scontext = stub.Context.fromUrls(
        jsonRpcUrls         = jsonRpcUrls,
        gasPriceTweak       = gasPriceTweak,
        gasLimitTweak       = gasLimitTweak,
        pollPeriod          = pollPeriod,
        pollTimeout         = pollTimeout,
        transactionApprover = transactionApprover,
        transactionLogger   = transactionLogger,
        eventConfirmations  = eventConfirmations
      )( implicitly[URLSource[U]], cfactory, poller, scheduler, econtext )
      new AsyncClient( nameServiceAddress, tld, reverseTld )( scontext )
    }
  }
}
class AsyncClient(
  val nameServiceAddress : EthAddress = StandardNameServiceAddress,
  val tld                : String     = StandardNameServiceTld,
  val reverseTld         : String     = StandardNameServiceReverseTld
)( implicit scontext : stub.Context ) {

  implicit val econtext = scontext.icontext.econtext

  private lazy val nameService = AsyncENS( nameServiceAddress )

  private def stubnamehash( name : String ) : sol.Bytes32 = sol.Bytes32( namehash( name ).bytes )

  private def _simplehash( str : String ) = componentHash( str )

  private def stubsimplehash( str : String ) = sol.Bytes32( _simplehash( str ).bytes )

  private def ethsigner    [T : EthSigner.Source] ( source : T ) : EthSigner  = implicitly[EthSigner.Source[T]].toEthSigner(source)

  private def ethaddress[T : EthAddress.Source]( source : T ) : EthAddress = implicitly[EthAddress.Source[T]].toEthAddress(source)

  private lazy val fregistrar : Future[AsyncRegistrar] = {
    owner( tld ) map { mbRegistrarAddress =>
      new AsyncRegistrar( mbRegistrarAddress.get ) // we assert that it exists
    }
  }

  private lazy val freverseRegistrar : Future[AsyncReverseRegistrar] = {
    owner( reverseTld ) map { mbReverseRegistrarAddress =>
      new AsyncReverseRegistrar( mbReverseRegistrarAddress.get ) // we assert that it exists
    }
  }

  def owner( name : String ) : Future[Option[EthAddress]] = {
    val fAddr = nameService.constant.owner( stubnamehash( name ) )( Sender.Default )
    fAddr.map( addr => if ( addr == EthAddress.Zero ) None else Some( addr ) )
  }

  def setOwner[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, address : T ) : Future[TransactionInfo] = {
    nameService.transaction.setOwner( stubnamehash( name ), ethaddress(address) )( Sender.Basic( ethsigner(signer) ) )
  }

  def ttl( name : String ) : Future[JDuration] = {
    val fSeconds = nameService.constant.ttl( stubnamehash( name ) )( Sender.Default )
    fSeconds.map( seconds => JDuration.ofSeconds( seconds.widen.toValidLong ) )
  }

  def setTTL[S : EthSigner.Source]( signer : S, name : String, ttl : Long ) : Future[TransactionInfo] = {
    nameService.transaction.setTTL( stubnamehash( name ), sol.UInt64( ttl ) )( Sender.Basic( ethsigner(signer) ) )
  }

  private def _resolver( stubnodehash : sol.Bytes32 ) : Future[Option[EthAddress]] = {
    val fAddr = nameService.constant.resolver( stubnodehash )( Sender.Default )
    fAddr.map( addr => if ( addr == EthAddress.Zero ) None else Some( addr ) )
  }

  def resolver( name : String ) : Future[Option[EthAddress]] = _resolver( stubnamehash( name ) )

  def setResolver[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, resolver : T ) : Future[TransactionInfo] = {
    nameService.transaction.setResolver( stubnamehash( name ), ethaddress( resolver ) )( Sender.Basic( ethsigner(signer) ) )
  }
  
  private def withResolver[T]( name : String )( op : ( sol.Bytes32, AsyncResolver ) => Future[T] ) : Future[T] = {
    val stubnodehash = stubnamehash( name )
    val fmbResolver : Future[Option[EthAddress]] = _resolver( stubnodehash )
    fmbResolver flatMap { mbResolver =>
      val mbFut = mbResolver map { resolverAddr =>
        val resolver = new AsyncResolver( resolverAddr )
        op( stubnodehash, resolver )
      }
      mbFut match {
        case None        => Future.failed( new NoResolverSetException( name ) )
        case Some( fut ) => fut
      }
    }
  }

  def address( name : String ) : Future[Option[EthAddress]] = {
    withResolver( name ){ ( stubnodehash, resolver ) =>
      val fAddr = resolver.constant.addr( stubnodehash )( Sender.Default )
      fAddr.map( addr => if ( addr == EthAddress.Zero ) None else Some( addr ) )
    }
  }

  def setAddress[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, address : T ) : Future[TransactionInfo] = {
    val _address = ethaddress( address )
    withResolver( name ){ ( stubnodehash, resolver ) =>
      resolver.transaction.setAddr( stubnodehash, _address )( Sender.Basic( ethsigner(signer) ) )
    }
  }

  def nameStatus( name : String ) : Future[NameStatus] = {
    val normalized = normalizeName( name )
    val fcode = fregistrar flatMap { registrar =>
      registrar.constant.state( stubsimplehash( normalized ) )( Sender.Default )
    }
    fcode.map( code => NameStatus.byCode( code.widen ) )
  }

  def ownedNotFinalized( name : String ) : Future[Boolean] = {
    nameStatus( name ) flatMap { ns =>
      owner( name ) map { mbOwner =>
        ns == NameStatus.Owned && mbOwner.isEmpty
      }
    }
  }

  def auctionEnd( name : String ) : Future[Option[Instant]] = {
    nameStatus( name ) flatMap { ns =>
      ns match {
        case NameStatus.Auction | NameStatus.Reveal => {
          val normalized = normalizeName( name )
          val fentries = fregistrar flatMap { registrar =>
            registrar.constant.entries( stubsimplehash( normalized ) )( Sender.Default )
          }
          fentries.map( entries => Some( Instant.ofEpochSecond( entries._3.widen.toLong ) ) )
        }
        case _ => Future.successful( None )
      }
    }
  }

  def revealStart( name : String ) : Future[Option[Instant]] = auctionEnd( name ).map { mbEnd =>
    mbEnd.map( _.minus( 2, DAYS ) )
  }

  def whenAvailable( name : String ) : Future[Instant] = {
    val normalized = normalizeName( name )
    val fsecond = fregistrar flatMap { registrar =>
      registrar.constant.getAllowedTime( stubsimplehash( normalized ) )( Sender.Default )
    }
    fsecond.map( second => Instant.ofEpochSecond( second.widen.toValidLong ) )
  }

  def transferDeed[S : EthSigner.Source, T : EthAddress.Source]( from : S, to : T, name : String ) : Future[TransactionInfo] = {
    val normalized = normalizeName( name )

    fregistrar flatMap { registrar =>
      registrar.transaction.transfer( stubsimplehash( normalized ), ethaddress(to) )( Sender.Basic( ethsigner(from) ) )
    }
  }

  def releaseDeed[S : EthSigner.Source]( owner : S, name : String ) : Future[TransactionInfo] = {
    val normalized = normalizeName( name )

    fregistrar flatMap { registrar => 
      registrar.transaction.releaseDeed( stubsimplehash( normalized ) )( Sender.Basic( ethsigner(owner) ) )
    }
  }

  // note that at the auction registrar, standard Ethereum Keccak hashes, rather than namehashes, are used

  private def requireSimpleName( simpleName : String ) = {
    require( simpleName.indexOf('.') < 0, s"The registrar auctions simple names in the '.${tld}' domain. These may not contain a '.'. (Bad Name: '${simpleName}'.)" )
  }

  private def normalizeName( name : String ) = normalizeNameForTld( name, tld )

  private lazy val entropy = new java.security.SecureRandom

  private def randomHash : EthHash = {
    val raw = Array.ofDim[Byte]( EthHash.HashLength )
    entropy.nextBytes( raw )
    EthHash.withBytes( raw )
  }

  private def newSalt = randomHash.bytes

  private def sealedBid( normalized : String, bidderAddress : EthAddress, valueInWei : BigInt , salt : immutable.Seq[Byte] ) : Future[EthHash] = {

    // this is a constant function that does not make use of a notional sender, Sender.Default is fine
    val fbytes32 = fregistrar flatMap { registrar =>
      registrar.constant.shaBid( stubsimplehash( normalized ), bidderAddress, sol.UInt256( valueInWei ), sol.Bytes32( salt ) )( Sender.Default )
    }

    fbytes32.map( bytes32 => EthHash.withBytes( bytes32.widen ) )
  }

  private def sealedBid( bid : Bid ) : Future[EthHash] = sealedBid( bid.simpleName, bid.bidderAddress, bid.valueInWei, bid.salt )

  def startAuction[T : EthSigner.Source]( from : T, name : String, numDiversions : Int = 0 ) : Future[TransactionInfo] = {
    val normalized = normalizeName( name )

    val diversions : Set[EthHash] = immutable.HashSet( (0 until numDiversions).map( _ => randomHash ) : _* )
    val real : EthHash            = _simplehash( normalized )

    val allSeq = (diversions + real).toList.map( hash => sol.Bytes32( hash.bytes ) )

    fregistrar flatMap { registrar =>
      registrar.transaction.startAuctions( allSeq )( Sender.Basic( ethsigner(from) ) )
    }
  }

  def createRawBid[T : EthAddress.Source]( fromAddress : T, name : String, valueInWei : BigInt ) : Future[Bid] = {
    val _from = ethaddress( fromAddress )
    val normalized = normalizeName( name )
    val saltBytes = randomHash.bytes
    val fbidHash = sealedBid( normalized, _from, valueInWei, saltBytes )
    fbidHash.map( bidHash => Bid( bidHash, normalized, _from, valueInWei, saltBytes ) )
  }

  def placeNewBid[T : EthSigner.Source]( bidder : T, name : String, valueInWei : BigInt, overpaymentInWei : BigInt = 0 )( implicit store : BidStore ) : Future[(Bid, TransactionInfo)] = {
    val _bidder = ethsigner( bidder )

    for {
      bid     <- createRawBid( _bidder.address, name, valueInWei )
      txnInfo <- _placeRawBid( _bidder, bid, overpaymentInWei, Some( store ) )
    } yield {
      ( bid, txnInfo )
    }
  }

  private def _placeRawBid[T : EthSigner.Source]( bidder : T, bid : Bid, overpaymentInWei : BigInt, mbStore : Option[BidStore] ) : Future[TransactionInfo] = {
    val _bidder = ethsigner( bidder )

    mbStore.foreach( _.store( bid ) ) // no matter what, persist what will be needed to reconstruct the bid hash!

    val ftxnInfo = fregistrar flatMap { registrar =>
      registrar.transaction.newBid( sol.Bytes32( bid.bidHash.bytes ), Some( sol.UInt256(bid.valueInWei + overpaymentInWei) ) )( Sender.Basic( _bidder ) )
    }
    ftxnInfo.onComplete { attempt =>
      attempt match {
        case Success( _ ) => mbStore.foreach( _.markAccepted( bid.bidHash ) )
        case Failure( _ ) => mbStore.foreach( _.remove( bid ) )
      }
    }
    ftxnInfo
  }

  def placeRawBid[T : EthSigner.Source]( bidder : T, bid : Bid, overpaymentInWei : Int = 0 ) : Future[TransactionInfo] = {
    val _bidder = ethsigner(bidder)
    if ( _bidder.address != bid.bidderAddress ) {
      Future.failed( new EnsException( s"Bidder '0x${_bidder.address.hex}' does not match address in Bid ('0x${bid.bidderAddress.hex}'). Cannot place bid." ) )
    } else {
      _placeRawBid( _bidder, bid, overpaymentInWei, None )
    }
  }

  def revealRawBid[T : EthSigner.Source]( bidder : T, bid : Bid ) : Future[TransactionInfo] = {
    val _bidder = ethsigner(bidder)
    if ( _bidder.address != bid.bidderAddress ) {
      Future.failed( new EnsException( s"Bidder '0x${_bidder.address.hex}' does not match address in Bid ('0x${bid.bidderAddress.hex}'). Cannot reveal bid." ) )
    } else {
      _revealRawBid( _bidder, bid )
    }
  }

  private def _revealRawBid[T : EthSigner.Source]( bidder : T, bid : Bid ) : Future[TransactionInfo] = {
    revealRawBid( _simplehash( bid.simpleName ), bidder, bid.valueInWei, bid.salt )
  }

  def revealRawBid[T : EthSigner.Source]( nodeHash : EthHash, bidder : T, valueInWei : BigInt, salt : immutable.Seq[Byte] ) : Future[TransactionInfo] = {
    val _bidder = ethsigner( bidder )

    fregistrar flatMap { registrar =>
      registrar.transaction.unsealBid( sol.Bytes32( nodeHash.bytes ), sol.UInt256( valueInWei ), sol.Bytes32( salt ) )( Sender.Basic( _bidder ) )
    }
  }

  def revealBid[T : EthSigner.Source]( from : T, bidHash : EthHash, force : Boolean )( implicit store : BidStore ) : Future[TransactionInfo] = {
    val (bid, state) = store.findByHash( bidHash ) // will throw if we can't find the bid!

    val _from = ethsigner( from )
    val revealerAddress = _from.address
    if (_from.address != bid.bidderAddress ) {
      Future.failed( new RevealerIsNotBidderException( revealerAddress, bid ) )
    }
    else if ( !force && state != BidStore.State.Accepted ) {
      Future.failed( new UnexpectedBidStoreStateException( bid, state ) )
    } else {
      val ftxnInfo = _revealRawBid( _from, bid )
      ftxnInfo.onComplete {
        case Success( _ ) => store.markRevealed( bidHash ) 
        case Failure( _ ) => /* ignore */
      }
      ftxnInfo
    }
  }

  def revealBid[T : EthSigner.Source]( from : T, name : String, force : Boolean = false )( implicit store : BidStore ) : Future[TransactionInfo] = {
    val _from = ethsigner( from )
    val normalized = normalizeName( name )
    val bids = store.findByNameBidderAddress( normalized, _from.address )

    bids.length match {
      case 0 => Future.failed( new EnsException( s"Uh oh. Can't find a stored bid with name '${normalized}'!" ) )
      case 1 => revealBid( _from, bids.head._1.bidHash, force )
      case _ => Future.failed( new EnsException( s"Multiple bids exist with that name. Please reveal by unique 'bidHash'. Bids: ${bids.map( _._1 )}" ) )
    }
  }

  def revealAllBids[T : EthSigner.Source]( from : T, name : String, force : Boolean = false )( implicit store : BidStore ) : Future[immutable.Seq[(Bid, TransactionInfo)]] = {
    val _from = ethsigner( from )
    val normalized = normalizeName( name )
    val bids = store.findByNameBidderAddress( normalized, _from.address )

    if ( bids.length == 0 ) {
      Future.failed( new EnsException( s"No bids were found with name '${normalized}' for bidder '${_from.address} in bid store '${store}'." ) )
    }
    else {
      val ftally = Future.sequence (
        bids map { case ( bid, state ) => {
          revealBid( _from, bid.bidHash, force )
            .map( ti => Right( Tuple2( bid, ti ) ) )
            .recover { case NonFatal( t ) => Left( FailedReveal( t, bid ) ) }
        } }
      )
      ftally flatMap { tally =>
        if ( tally.forall( _.isRight ) ) {
          Future.successful (
            tally map {
              case Right( pair ) => pair
              case other => throw new InternalError( s"Huh? We just checked that these were all Rights, yet... ${tally}" ) // should never happen
            }
          )
        }
        else if ( tally.size == 1 ) {
          val Left( failedReveal ) = tally.head // at least one failed and there's only one, it must be the first, it must be a left
          Future.failed( failedReveal.failure )
        }
        else {
          Future.failed( new SomeRevealsFailedException( tally ) )
        }
      }
    }
  }

  def cancelExpiredBid[S : EthSigner.Source, T : EthAddress.Source] ( canceller : S, lameBidder : T, bidHash : EthHash ) : Future[TransactionInfo] = {
    fregistrar flatMap { registrar =>
      registrar.transaction.cancelBid( ethaddress( lameBidder ), sol.Bytes32( bidHash.bytes ) )( Sender.Basic( ethsigner(canceller) ) )
    }
  }

  def finalizeAuction[T : EthSigner.Source]( from : T, name : String ) : Future[TransactionInfo] = {
    val normalized = normalizeName( name )

    fregistrar flatMap { registrar =>
      registrar.transaction.finalizeAuction( stubsimplehash( normalized ) )( Sender.Basic( ethsigner(from) ) )
    }
  }
}

