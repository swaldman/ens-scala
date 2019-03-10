package com.mchange.sc.v2.ens

import contract._

import java.time.{Duration => JDuration, Instant}
import java.time.temporal.ChronoUnit.DAYS

import scala.collection._
import scala.concurrent.{Await,ExecutionContext,Future}
import scala.concurrent.duration._

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash,EthPrivateKey,EthSigner,wallet}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.stub
import stub.{sol, Sender, TransactionInfo}

import com.mchange.sc.v2.jsonrpc.Exchanger
import com.mchange.sc.v2.concurrent.{Poller, Scheduler}
import com.mchange.sc.v2.net.URLSource

object Client {

  val DefaultExecutionTimeout = Duration.Inf

  def apply[ U : URLSource ](
    jsonRpcUrl          : U,
    nameServiceAddress  : EthAddress               = StandardNameServiceAddress,
    tld                 : String                   = StandardNameServiceTld,
    reverseTld          : String                   = StandardNameServiceReverseTld,
    gasPriceTweak       : stub.MarkupOrOverride    = stub.Context.Default.GasPriceTweak,
    gasLimitTweak       : stub.MarkupOrOverride    = stub.Context.Default.GasLimitTweak,
    pollPeriod          : Duration                 = stub.Context.Default.PollPeriod,
    pollTimeout         : Duration                 = stub.Context.Default.PollTimeout,
    httpTimeout         : Duration                 = stub.Context.Default.HttpTimeout,
    transactionApprover : stub.TransactionApprover = stub.Context.Default.TransactionApprover,
    transactionLogger   : stub.TransactionLogger   = stub.Context.Default.TransactionLogger,
    eventConfirmations  : Int                      = stub.Context.Default.EventConfirmations,
    executionTimeout    : Duration                 = Client.DefaultExecutionTimeout
  )( implicit
    efactory  : Exchanger.Factory = stub.Context.Default.ExchangerFactory,
    poller    : Poller            = stub.Context.Default.Poller,
    scheduler : Scheduler         = stub.Context.Default.Scheduler,
    econtext  : ExecutionContext  = stub.Context.Default.ExecutionContext
  ) = {
    val scontext = stub.Context.fromUrl(
      jsonRpcUrl          = jsonRpcUrl,
      gasPriceTweak       = gasPriceTweak,
      gasLimitTweak       = gasLimitTweak,
      pollPeriod          = pollPeriod,
      pollTimeout         = pollTimeout,        
      httpTimeout         = httpTimeout,        
      transactionApprover = transactionApprover,        
      transactionLogger   = transactionLogger,
      eventConfirmations  = eventConfirmations
    )( implicitly[URLSource[U]], efactory, poller, scheduler, econtext )
    new Client( nameServiceAddress, tld, reverseTld, executionTimeout )( scontext )
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
      httpTimeout         : Duration                 = stub.Context.Default.HttpTimeout,
      transactionApprover : stub.TransactionApprover = stub.Context.Default.TransactionApprover,
      transactionLogger   : stub.TransactionLogger   = stub.Context.Default.TransactionLogger,
      eventConfirmations  : Int                      = stub.Context.Default.EventConfirmations,
      executionTimeout    : Duration                 = Client.DefaultExecutionTimeout
    )( implicit
      efactory  : Exchanger.Factory  = stub.Context.Default.ExchangerFactory,
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
        httpTimeout         = httpTimeout,
        transactionApprover = transactionApprover,
        transactionLogger   = transactionLogger,
        eventConfirmations  = eventConfirmations
      )( implicitly[URLSource[U]], efactory, poller, scheduler, econtext )
      new Client( nameServiceAddress, tld, reverseTld, executionTimeout )( scontext )
    }
  }
}
class Client(
  val nameServiceAddress : EthAddress = StandardNameServiceAddress,
  val tld                : String     = StandardNameServiceTld,
  val reverseTld         : String     = StandardNameServiceReverseTld,
  val executionTimeout   : Duration   = Client.DefaultExecutionTimeout
)( implicit scontext : stub.Context ) {

  private val inner : AsyncClient = new AsyncClient( nameServiceAddress, tld, reverseTld )( scontext )

  private def await[T]( ft : Future[T] ) = Await.result( ft, executionTimeout )

  private def awaitTransactionInfo( fti : Future[TransactionInfo.Async] ) = await( fti ).await

  def owner( name : String ) : Option[EthAddress] = await( inner.owner( name ) )

  def setOwner[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, address : T, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.setOwner( signer, name, address, forceNonce = forceNonce ) )
  }

  def setSubnodeOwner[S : EthSigner.Source, T : EthAddress.Source]( signer : S, parentName : String, subnodeLabel : String, address : T, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.setSubnodeOwner( signer, parentName, subnodeLabel, address, forceNonce = forceNonce ) )
  }

  def ttl( name : String ) : JDuration = await( inner.ttl( name ) )

  def setTTL[S : EthSigner.Source]( signer : S, name : String, ttl : Long, forceNonce : Option[BigInt] = None ) : TransactionInfo = awaitTransactionInfo( inner.setTTL( signer, name, ttl, forceNonce = forceNonce ) )

  def resolver( name : String ) : Option[EthAddress] = await( inner.resolver( name ) )

  def setResolver[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, resolver : T, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.setResolver( signer, name, resolver, forceNonce = forceNonce ) )
  }

  def address( name : String ) : Option[EthAddress] = await( inner.address( name ) )

  def setAddress[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, address : T, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.setAddress( signer, name, address, forceNonce = forceNonce ) )
  }

  def nameStatus( name : String ) : NameStatus = {
    await( inner.nameStatus( name ) )
  }

  def ownedNotFinalized( name : String ) : Boolean = await( inner.ownedNotFinalized( name ) )

  def auctionEnd( name : String ) : Option[Instant] = await( inner.auctionEnd( name ) )

  def revealStart( name : String ) : Option[Instant] = await( inner.revealStart( name ) )

  def whenAvailable( name : String ) : Instant = await( inner.whenAvailable( name ) )

  def transferDeed[S : EthSigner.Source, T : EthAddress.Source]( from : S, to : T, name : String, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.transferDeed( from, to, name, forceNonce = forceNonce ) )
  }

  def releaseDeed[S : EthSigner.Source]( owner : S, name : String, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.releaseDeed( owner, name, forceNonce = forceNonce ) )
  }

  def startAuction[T : EthSigner.Source]( from : T, name : String, numDiversions : Int = 0, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.startAuction( from, name, numDiversions, forceNonce = forceNonce ) )
  }
  
  def createRawBid[T : EthAddress.Source]( fromAddress : T, name : String, valueInWei : BigInt ) : Bid = {
    await( inner.createRawBid( fromAddress, name, valueInWei ) )
  }

  def placeNewBid[T : EthSigner.Source]( bidder : T, name : String, valueInWei : BigInt, overpaymentInWei : BigInt = 0, forceNonce : Option[BigInt] = None )( implicit store : BidStore ) : (Bid, TransactionInfo) = {
    val tup = await( inner.placeNewBid( bidder, name, valueInWei, overpaymentInWei, forceNonce = forceNonce ) )
    ( tup._1, tup._2.await )
  }

  def placeRawBid[T : EthSigner.Source]( bidder : T, bid : Bid, overpaymentInWei : Int = 0, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.placeRawBid( bidder, bid, forceNonce = forceNonce ) )
  }

  def revealRawBid[T : EthSigner.Source]( bidder : T, bid : Bid ) : TransactionInfo = {
    awaitTransactionInfo( inner.revealRawBid( bidder, bid ) )
  }

  def revealRawBid[T : EthSigner.Source]( bidder : T, bid : Bid, forceNonce : Option[BigInt] ) : TransactionInfo = {
    awaitTransactionInfo( inner.revealRawBid( bidder, bid, forceNonce = forceNonce ) )
  }

  def revealRawBid[T : EthSigner.Source]( nodeHash : EthHash, bidder : T, valueInWei : BigInt, salt : immutable.Seq[Byte] ) : TransactionInfo = {
    awaitTransactionInfo( inner.revealRawBid( nodeHash, bidder, valueInWei, salt ) )
  }

  def revealRawBid[T : EthSigner.Source]( nodeHash : EthHash, bidder : T, valueInWei : BigInt, salt : immutable.Seq[Byte], forceNonce : Option[BigInt] ) : TransactionInfo = {
    awaitTransactionInfo( inner.revealRawBid( nodeHash, bidder, valueInWei, salt, forceNonce = forceNonce ) )
  }

  def revealBidByHash[T : EthSigner.Source]( from : T, bidHash : EthHash, force : Boolean = false )( implicit store : BidStore ) : TransactionInfo = {
    awaitTransactionInfo( inner.revealBidByHash( from, bidHash, force ) )
  }

  def revealBidByHash[T : EthSigner.Source]( from : T, bidHash : EthHash, force : Boolean, forceNonce : Option[BigInt] )( implicit store : BidStore ) : TransactionInfo = {
    awaitTransactionInfo( inner.revealBidByHash( from, bidHash, force, forceNonce = forceNonce ) )
  }

  def revealBidByName[T : EthSigner.Source]( from : T, name : String, force : Boolean = false )( implicit store : BidStore ) : TransactionInfo = {
    awaitTransactionInfo( inner.revealBidByName( from, name, force ) )
  }

  def revealBidByName[T : EthSigner.Source]( from : T, name : String, force : Boolean, forceNonce : Option[BigInt] )( implicit store : BidStore ) : TransactionInfo = {
    awaitTransactionInfo( inner.revealBidByName( from, name, force, forceNonce = forceNonce ) )
  }

  def revealAllBids[T : EthSigner.Source]( from : T, name : String, force : Boolean = false )( implicit store : BidStore ) : immutable.Seq[(Bid, TransactionInfo)] = {
    val asyncs = await( inner.revealAllBids( from, name, force ) )
    asyncs.map { case (bid, tia) => (bid, tia.await) }
  }

  def cancelExpiredBid[S : EthSigner.Source, T : EthAddress.Source] ( canceller : S, lameBidder : T, bidHash : EthHash, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.cancelExpiredBid( canceller, lameBidder, bidHash, forceNonce = forceNonce ) )
  }

  def finalizeAuction[T : EthSigner.Source]( from : T, name : String, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.finalizeAuction( from, name, forceNonce = forceNonce ) )
  }
}


