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

import contract._

import java.time.{Duration => JDuration, Instant}
import java.time.temporal.ChronoUnit.DAYS

import scala.collection._
import scala.concurrent.{Await,Future,ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Try,Success,Failure}
import scala.util.control.NonFatal

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthChainId,EthHash,EthPrivateKey,EthSigner,wallet}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.stub
import stub.{sol, Sender, TransactionInfo}

import com.mchange.sc.v2.jsonrpc.Exchanger
import com.mchange.sc.v2.concurrent.{Poller, Scheduler}
import com.mchange.sc.v2.net.URLSource

object AsyncClient {

  def apply[ U : URLSource ](
    jsonRpcUrl          : U,
    chainId             : Option[EthChainId]       = stub.Context.Default.ChainId,
    nameServiceAddress  : EthAddress               = StandardNameServiceAddress,
    gasPriceTweak       : stub.MarkupOrOverride    = stub.Context.Default.GasPriceTweak,
    gasLimitTweak       : stub.MarkupOrOverride    = stub.Context.Default.GasLimitTweak,
    pollPeriod          : Duration                 = stub.Context.Default.PollPeriod,
    pollTimeout         : Duration                 = stub.Context.Default.PollTimeout,
    httpTimeout         : Duration                 = stub.Context.Default.HttpTimeout,
    transactionApprover : stub.TransactionApprover = stub.Context.Default.TransactionApprover,
    transactionLogger   : stub.TransactionLogger   = stub.Context.Default.TransactionLogger,
    eventConfirmations  : Int                      = stub.Context.Default.EventConfirmations
  )( implicit
    efactory  : Exchanger.Factory = stub.Context.Default.ExchangerFactory,
    poller    : Poller            = stub.Context.Default.Poller,
    scheduler : Scheduler         = stub.Context.Default.Scheduler,
    econtext  : ExecutionContext  = stub.Context.Default.ExecutionContext
  ) = {
    val scontext = stub.Context.fromUrl(
      jsonRpcUrl          = jsonRpcUrl,
      chainId             = chainId,
      gasPriceTweak       = gasPriceTweak,
      gasLimitTweak       = gasLimitTweak,
      pollPeriod          = pollPeriod,
      pollTimeout         = pollTimeout,        
      httpTimeout         = httpTimeout,        
      transactionApprover = transactionApprover,        
      transactionLogger   = transactionLogger,
      eventConfirmations  = eventConfirmations
    )( implicitly[URLSource[U]], efactory, poller, scheduler, econtext )
    new AsyncClient( nameServiceAddress )( scontext )
  }

  final object LoadBalanced {
    def apply[ U : URLSource ](
      jsonRpcUrls         : immutable.Iterable[U],
      chainId             : Option[EthChainId]       = stub.Context.Default.ChainId,
      nameServiceAddress  : EthAddress               = StandardNameServiceAddress,
      gasPriceTweak       : stub.MarkupOrOverride    = stub.Context.Default.GasPriceTweak,
      gasLimitTweak       : stub.MarkupOrOverride    = stub.Context.Default.GasLimitTweak,
      pollPeriod          : Duration                 = stub.Context.Default.PollPeriod,
      pollTimeout         : Duration                 = stub.Context.Default.PollTimeout,
      httpTimeout         : Duration                 = stub.Context.Default.HttpTimeout,
      transactionApprover : stub.TransactionApprover = stub.Context.Default.TransactionApprover,
      transactionLogger   : stub.TransactionLogger   = stub.Context.Default.TransactionLogger,
      eventConfirmations  : Int                      = stub.Context.Default.EventConfirmations
    )( implicit
      efactory  : Exchanger.Factory  = stub.Context.Default.ExchangerFactory,
      poller    : Poller             = stub.Context.Default.Poller,
      scheduler : Scheduler          = stub.Context.Default.Scheduler,
      econtext  : ExecutionContext   = stub.Context.Default.ExecutionContext
    ) = {
      val scontext = stub.Context.fromUrls(
        jsonRpcUrls         = jsonRpcUrls,
        chainId             = chainId,
        gasPriceTweak       = gasPriceTweak,
        gasLimitTweak       = gasLimitTweak,
        pollPeriod          = pollPeriod,
        pollTimeout         = pollTimeout,
        httpTimeout         = httpTimeout,
        transactionApprover = transactionApprover,
        transactionLogger   = transactionLogger,
        eventConfirmations  = eventConfirmations
      )( implicitly[URLSource[U]], efactory, poller, scheduler, econtext )
      new AsyncClient( nameServiceAddress )( scontext )
    }
  }
}
class AsyncClient(
  val nameServiceAddress : EthAddress = StandardNameServiceAddress
)( implicit scontext : stub.Context ) {

  implicit val econtext = scontext.icontext.econtext

  private lazy val nameService = AsyncENS( nameServiceAddress )

  private def stubnamehash( name : String ) : sol.Bytes32 = sol.Bytes32( namehash( name ).bytes )

  private def stublabelhash( str : String ) = sol.Bytes32( labelhash( str ).bytes )

  private def stublabelhash_uint( str : String ) = sol.UInt256( BigInt( 1, labelhash( str ).toByteArray ) )

  private def ethsigner[S : EthSigner.Source]( source : S ) : EthSigner  = implicitly[EthSigner.Source[S]].toEthSigner(source)

  private def ethsender[S : EthSigner.Source]( source : S ) : stub.Sender.Signing = Sender.Basic( ethsigner(source) )

  private def ethaddress[T : EthAddress.Source]( source : T ) : EthAddress = implicitly[EthAddress.Source[T]].toEthAddress(source)

  private def toStubNonce( forceNonce : Option[BigInt] ) = {
    forceNonce match {
      case Some( value ) => stub.Nonce.withValue( sol.UInt256( value ) )
      case None          => stub.Nonce.Auto
    }
  }

  private lazy val freverseRegistrar : Future[AsyncReverseRegistrar] = {
    owner( StandardNameServiceReverseTld ) map { mbReverseRegistrarAddress =>
      new AsyncReverseRegistrar( mbReverseRegistrarAddress.get ) // we assert that it exists
    }
  }

  def owner( path : String ) : Future[Option[EthAddress]] = {
    val fAddr = nameService.view.owner( stubnamehash( path ) )( Sender.Default )
    fAddr.map( addr => if ( addr == EthAddress.Zero ) None else Some( addr ) )
  }

  private def onlyOwner[S : EthSigner.Source]( signer : S, path : String )( op : => Future[TransactionInfo.Async] ) : Future[TransactionInfo.Async] = {
    owner( path ).flatMap { 
      case Some( owner ) => {
        val es = ethsigner(signer)
        if ( owner == es.address ) {
          op
        }
        else {
          Future.failed( new OnlyOwnerException( path, es.address, owner ) )
        }
      }
      case None => {
        Future.failed( new MustBeOwnedException( path ) )
      }
    }
  }

  def setOwner[S : EthSigner.Source, T : EthAddress.Source]( signer : S, path : String, address : T, forceNonce : Option[BigInt] = None ) : Future[TransactionInfo.Async] = onlyOwner( signer, path ) {
    nameService.txn.setOwner( stubnamehash( path ), ethaddress(address), nonce = toStubNonce( forceNonce ) )( ethsender(signer) )
  }

  def setSubnodeOwner[S : EthSigner.Source, T : EthAddress.Source]( signer : S, parentPath : String, subnodeLabel : String, address : T, forceNonce : Option[BigInt] = None ) : Future[TransactionInfo.Async] = {
    onlyOwner( signer, parentPath ) {
      nameService.txn.setSubnodeOwner( stubnamehash( parentPath ), stublabelhash( subnodeLabel ), ethaddress(address), nonce = toStubNonce( forceNonce ) )( ethsender(signer) )
    }
  }

  def ttl( path : String ) : Future[JDuration] = {
    val fSeconds = nameService.view.ttl( stubnamehash( path ) )( Sender.Default )
    fSeconds.map( seconds => JDuration.ofSeconds( seconds.widen.toValidLong ) )
  }

  def setTTL[S : EthSigner.Source]( signer : S, name : String, ttl : Long, forceNonce : Option[BigInt] = None ) : Future[TransactionInfo.Async] = onlyOwner( signer, name ) {
    nameService.txn.setTTL( stubnamehash( name ), sol.UInt64( ttl ), nonce = toStubNonce( forceNonce ) )( ethsender(signer) )
  }

  private def _resolver( stubnodehash : sol.Bytes32 ) : Future[Option[EthAddress]] = {
    val fAddr = nameService.view.resolver( stubnodehash )( Sender.Default )
    fAddr.map( addr => if ( addr == EthAddress.Zero ) None else Some( addr ) )
  }

  def resolver( path : String ) : Future[Option[EthAddress]] = _resolver( stubnamehash( path ) )

  def setResolver[S : EthSigner.Source, T : EthAddress.Source]( signer : S, path : String, resolver : T, forceNonce : Option[BigInt] = None ) : Future[TransactionInfo.Async] = onlyOwner( signer, path ) {
    nameService.txn.setResolver( stubnamehash( path ), ethaddress( resolver ), nonce = toStubNonce( forceNonce ) )( ethsender(signer) )
  }
  
  private def withResolver[T]( path : String )( op : ( sol.Bytes32, AsyncResolver ) => Future[T] ) : Future[T] = {
    val stubnodehash = stubnamehash( path )
    val fmbResolver : Future[Option[EthAddress]] = _resolver( stubnodehash )
    fmbResolver flatMap { mbResolver =>
      val mbFut = mbResolver map { resolverAddr =>
        val resolver = new AsyncResolver( resolverAddr )
        op( stubnodehash, resolver )
      }
      mbFut match {
        case None        => Future.failed( new NoResolverSetException( path ) )
        case Some( fut ) => fut
      }
    }
  }

  def address( path : String ) : Future[Option[EthAddress]] = {
    withResolver( path ){ ( stubnodehash, resolver ) =>
      val fAddr = resolver.view.addr( stubnodehash )( Sender.Default )
      fAddr.map( addr => if ( addr == EthAddress.Zero ) None else Some( addr ) )
    } recover { case e : NoResolverSetException => None }
  }

  def setAddress[S : EthSigner.Source, T : EthAddress.Source]( signer : S, path : String, address : T, forceNonce : Option[BigInt] = None ) : Future[TransactionInfo.Async] = {
    val _address = ethaddress( address )
    withResolver( path ){ ( stubnodehash, resolver ) =>
      resolver.txn.setAddr( stubnodehash, _address, nonce = toStubNonce( forceNonce ) )( ethsender(signer) )
    }
  }

  final object forTopLevelDomain {
    // MT: access controlled by this' lock
    private val tldToController : mutable.Map[String,RegistrarManagedDomain] = mutable.Map.empty

    def apply( tld : String ) : RegistrarManagedDomain = this.synchronized {
      tldToController.getOrElseUpdate( tld, RegistrarManagedDomain(tld) )
    }
  }

  final object RegistrarManagedDomain {
    def apply( domain : String ) : RegistrarManagedDomain =  new RegistrarManagedDomain( domain )
  }
  final class RegistrarManagedDomain( val domain : String ) {
    private lazy val _maybeDomainRegistrarAddress : Future[Option[EthAddress]] = owner( domain )

    private lazy val assertedDomainRegistrarAddress : Future[EthAddress] = {
      _maybeDomainRegistrarAddress map {
        case Some( address ) => address
        case None            => throw new NoResolverSetException( domain )
      }
    }

    private lazy val domainRegistrar : Future[AsyncRegistrar] = {
      for {
        address <- assertedDomainRegistrarAddress
        registrar = new AsyncRegistrar( address )
        supports <- registrar.view.supportsInterface( InterfaceId.NftRegistrar )( Sender.Default )
      }
      yield {
        if ( supports ) {
          registrar
        }
        else {
          throw new BadRegistrarException( s"Putative registrar at 0xs{address.hex} does not support the expected NFT / ENS Registrar interface ( 0x${InterfaceId.NftRegistrar.widen.hex} )." )
        }
      }
    }

    private lazy val domainResolver : Future[AsyncResolver] = {
      resolver( domain ).map( _.getOrElse( throw new NoResolverSetException( domain ) ) ).map { address =>
        new AsyncResolver( address )
      }
    }

    private lazy val domainController : Future[AsyncController] = {
      for {
        topLevelResolver          <- domainResolver
        topLevelControllerAddress <- topLevelResolver.view.interfaceImplementer( stubnamehash( domain ), InterfaceId.Controller )( Sender.Default )
      }
      yield {
        if ( topLevelControllerAddress != EthAddress.Zero ) {
          new AsyncController( topLevelControllerAddress )
        }
        else {
          throw new NoControllerSetException( domain )
        }
      }
    }

    lazy val maybeDomainRegistrarAddress : Future[Option[EthAddress]] = domainRegistrar.map( registrar => Some( registrar.address ) ).recover { case _ => None }

    lazy val hasValidRegistrar : Future[Boolean] = maybeDomainRegistrarAddress.map( _.fold( false )( addr => true ) )

    def minCommitmentAgeInSeconds : Future[BigInt] = {
      for {
        topLevelController        <- domainController
        minCommitmentAgeInSeconds <- topLevelController.view.minCommitmentAge()( Sender.Default )
      }
      yield {
        minCommitmentAgeInSeconds.widen
      }
    }

    def maxCommitmentAgeInSeconds : Future[BigInt] = {
      for {
        topLevelController        <- domainController
        maxCommitmentAgeInSeconds <- topLevelController.view.maxCommitmentAge()( Sender.Default )
      }
      yield {
        maxCommitmentAgeInSeconds.widen
      }
    }

    def minRegistrationDurationInSeconds : Future[BigInt] = {
      for {
        topLevelController   <- domainController
        minDurationInSeconds <- topLevelController.view.MIN_REGISTRATION_DURATION()( Sender.Default )
      }
      yield {
        minDurationInSeconds.widen
      }
    }

    def rentPriceInWei( name : String, durationInSeconds : BigInt ) : Future[BigInt] = {
      requireSimpleName( name )
      for {
        topLevelController <- domainController
        stubWei            <- topLevelController.view.rentPrice( name, sol.UInt( durationInSeconds ) )( Sender.Default )
      }
      yield {
        stubWei.widen
      }
    }

    def isValid( name : String ) : Future[Boolean] = {
      requireSimpleName( name )
      for {
        topLevelController <- domainController
        stubBool           <- topLevelController.view.valid( name )( Sender.Default )
      }
      yield {
        stubBool
      }
    }

    def isAvailable( name : String ) : Future[Boolean] = {
      requireSimpleName( name )
      for {
        topLevelController <- domainController
        stubBool           <- topLevelController.view.available( name )( Sender.Default )
      }
      yield {
        stubBool
      }
    }

    def nameExpires( name : String ) : Future[Instant] = {
      for {
        registrar <- domainRegistrar
        unixtime  <- registrar.view.nameExpires( stublabelhash_uint( name ) )( Sender.Default )
      }
      yield {
        Instant.ofEpochSecond( unixtime.widen.toLong )
      }
    }

    def makeCommitment[T : EthAddress.Source]( name : String, owner : T ) : Future[Commitment] = {
      requireSimpleName( name )

      val secret = Commitment.newSecret()
      for {
        topLevelController <- domainController
        hash               <- topLevelController.view.makeCommitment( name, ethaddress(owner), secret )( Sender.Default )
      }
      yield {
        Commitment( EthHash.withBytes(hash.widen), secret )
      }
    }

    def commit[S : EthSigner.Source]( signer : S, commitment : Commitment ) : Future[TransactionInfo.Async] = {
      for {
        topLevelController <- domainController
        txnInfo            <- topLevelController.txn.commit( sol.Bytes32(commitment.hash.bytes) )( ethsender( signer ) )
      }
      yield {
        txnInfo
      }
    }

    def register[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, owner : T, durationInSeconds : BigInt, commitment : Commitment, paymentInWei : BigInt ) : Future[TransactionInfo.Async] = {
      requireSimpleName( name )
      for {
        topLevelController <- domainController
        txnInfo            <- topLevelController.txn.register( name, ethaddress(owner), sol.UInt256(durationInSeconds), commitment.secret, payment = stub.Payment.ofWei( sol.UInt256( paymentInWei ) ) )( ethsender( signer ) )
      }
      yield {
        txnInfo
      }
    }

    def register[S : EthSigner.Source, T : EthAddress.Source]( signer : S, name : String, owner : T, durationInSeconds : BigInt, secret : Seq[Byte], paymentInWei : BigInt ) : Future[TransactionInfo.Async] = {
      requireSimpleName( name )
      for {
        topLevelController <- domainController
        txnInfo            <- topLevelController.txn.register( name, ethaddress(owner), sol.UInt256(durationInSeconds), sol.Bytes32(secret), payment = stub.Payment.ofWei( sol.UInt256( paymentInWei ) ) )( ethsender( signer ) )
      }
      yield {
        txnInfo
      }
    }

    def renew[S : EthSigner.Source]( signer : S, name : String, durationInSeconds : BigInt ) : Future[TransactionInfo.Async] = {
      requireSimpleName( name )
      for {
        topLevelController <- domainController
        txnInfo            <- topLevelController.txn.renew( name, sol.UInt256(durationInSeconds) )( ethsender(signer) )
      }
      yield {
        txnInfo
      }
    }

    private def requireSimpleName( name : String ) = {
      require( name.indexOf('.') < 0, s"A simple name (not a path, no internal periods) is required, but found '${name}'." )
    }
  }
}

