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

  def owner( name : String ) : Option[EthAddress] = await( inner.owner( name ) )

  def setOwner[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, address : T ) : TransactionInfo = {
    await( inner.setOwner( signer, name, address ) )
  }

  def ttl( name : String ) : JDuration = await( inner.ttl( name ) )

  def setTTL[S : EthSigner.Source]( signer : S, name : String, ttl : Long ) : TransactionInfo = await( inner.setTTL( signer, name, ttl ) )

  def resolver( name : String ) : Option[EthAddress] = await( inner.resolver( name ) )

  def setResolver[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, resolver : T ) : TransactionInfo = {
    await( inner.setResolver( signer, name, resolver ) )
  }

  def address( name : String ) : Option[EthAddress] = await( inner.address( name ) )

  def setAddress[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, address : T ) : TransactionInfo = {
    await( inner.setAddress( signer, name, address ) )
  }

  def nameStatus( name : String ) : NameStatus = {
    await( inner.nameStatus( name ) )
  }

  def ownedNotFinalized( name : String ) : Boolean = await( inner.ownedNotFinalized( name ) )

  def auctionEnd( name : String ) : Option[Instant] = await( inner.auctionEnd( name ) )

  def revealStart( name : String ) : Option[Instant] = await( inner.revealStart( name ) )

  def whenAvailable( name : String ) : Instant = await( inner.whenAvailable( name ) )

  def transferDeed[S : EthSigner.Source, T : EthAddress.Source]( from : S, to : T, name : String ) : TransactionInfo = {
    await( inner.transferDeed( from, to, name ) )
  }

  def releaseDeed[S : EthSigner.Source]( owner : S, name : String ) : TransactionInfo = {
    await( inner.releaseDeed( owner, name ) )
  }

  def startAuction[T : EthSigner.Source]( from : T, name : String, numDiversions : Int = 0 ) : TransactionInfo = {
    await( inner.startAuction( from, name, numDiversions ) )
  }
  
  def createRawBid[T : EthAddress.Source]( fromAddress : T, name : String, valueInWei : BigInt ) : Bid = {
    await( inner.createRawBid( fromAddress, name, valueInWei ) )
  }

  def placeNewBid[T : EthSigner.Source]( bidder : T, name : String, valueInWei : BigInt, overpaymentInWei : BigInt = 0 )( implicit store : BidStore ) : (Bid, TransactionInfo) = {
    await( inner.placeNewBid( bidder, name, valueInWei, overpaymentInWei ) )
  }

  def placeRawBid[T : EthSigner.Source]( bidder : T, bid : Bid, overpaymentInWei : Int = 0 ) : TransactionInfo = {
    await( inner.placeRawBid( bidder, bid ) )
  }

  def revealRawBid[T : EthSigner.Source]( bidder : T, bid : Bid ) : TransactionInfo = {
    await( inner.revealRawBid( bidder, bid ) )
  }

  def revealRawBid[T : EthSigner.Source]( nodeHash : EthHash, bidder : T, valueInWei : BigInt, salt : immutable.Seq[Byte] ) : TransactionInfo = {
    await( inner.revealRawBid( nodeHash, bidder, valueInWei, salt ) )
  }

  def revealBid[T : EthSigner.Source]( from : T, bidHash : EthHash, force : Boolean )( implicit store : BidStore ) : TransactionInfo = {
    await( inner.revealBid( from, bidHash, force ) )
  }

  def revealBid[T : EthSigner.Source]( from : T, name : String, force : Boolean = false )( implicit store : BidStore ) : TransactionInfo = {
    await( inner.revealBid( from, name, force ) )
  }

  def revealAllBids[T : EthSigner.Source]( from : T, name : String, force : Boolean = false )( implicit store : BidStore ) : immutable.Seq[(Bid, TransactionInfo)] = {
    await( inner.revealAllBids( from, name, force ) )
  }

  def cancelExpiredBid[S : EthSigner.Source, T : EthAddress.Source] ( canceller : S, lameBidder : T, bidHash : EthHash ) : TransactionInfo = {
    await( inner.cancelExpiredBid( canceller, lameBidder, bidHash ) )
  }

  def finalizeAuction[T : EthSigner.Source]( from : T, name : String ) : TransactionInfo = {
    await( inner.finalizeAuction( from, name ) )
  }
}


