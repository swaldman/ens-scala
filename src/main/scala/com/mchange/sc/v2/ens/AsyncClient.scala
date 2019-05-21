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
import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthHash,EthPrivateKey,EthSigner,wallet}
import com.mchange.sc.v1.consuela.ethereum.jsonrpc

import com.mchange.sc.v1.consuela.ethereum.stub
import stub.{sol, Sender, TransactionInfo}

import com.mchange.sc.v2.jsonrpc.Exchanger
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
      gasPriceTweak       = gasPriceTweak,
      gasLimitTweak       = gasLimitTweak,
      pollPeriod          = pollPeriod,
      pollTimeout         = pollTimeout,        
      httpTimeout         = httpTimeout,        
      transactionApprover = transactionApprover,        
      transactionLogger   = transactionLogger,
      eventConfirmations  = eventConfirmations
    )( implicitly[URLSource[U]], efactory, poller, scheduler, econtext )
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
        gasPriceTweak       = gasPriceTweak,
        gasLimitTweak       = gasLimitTweak,
        pollPeriod          = pollPeriod,
        pollTimeout         = pollTimeout,
        httpTimeout         = httpTimeout,
        transactionApprover = transactionApprover,
        transactionLogger   = transactionLogger,
        eventConfirmations  = eventConfirmations
      )( implicitly[URLSource[U]], efactory, poller, scheduler, econtext )
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

  private def toStubNonce( forceNonce : Option[BigInt] ) = {
    forceNonce match {
      case Some( value ) => stub.Nonce.withValue( sol.UInt256( value ) )
      case None          => stub.Nonce.Auto
    }
  }

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

  private lazy val ftopLevelResolver : Future[AsyncResolver] = {
    resolver( tld ).map( _.getOrElse( throw new NoResolverSetException( tld ) ) ).map { address =>
      new AsyncResolver( address )
    }
  }

  private lazy val ftopLevelController : Future[AsyncController] = {
    for {
      topLevelResolver          <- ftopLevelResolver
      topLevelControllerAddress <- topLevelResolver.view.interfaceImplementer( stubnamehash( tld ), ControllerInterfaceId )( Sender.Default )
    }
    yield {
      new AsyncController( topLevelControllerAddress )
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
    nameService.txn.setOwner( stubnamehash( path ), ethaddress(address), nonce = toStubNonce( forceNonce ) )( Sender.Basic( ethsigner(signer) ) )
  }

  def setSubnodeOwner[S : EthSigner.Source, T : EthAddress.Source]( signer : S, parentPath : String, subnodeLabel : String, address : T, forceNonce : Option[BigInt] = None ) : Future[TransactionInfo.Async] = {
    onlyOwner( signer, parentPath ) {
      nameService.txn.setSubnodeOwner( stubnamehash( parentPath ), stubsimplehash( subnodeLabel ), ethaddress(address), nonce = toStubNonce( forceNonce ) )( Sender.Basic( ethsigner(signer) ) )
    }
  }

  def ttl( path : String ) : Future[JDuration] = {
    val fSeconds = nameService.view.ttl( stubnamehash( path ) )( Sender.Default )
    fSeconds.map( seconds => JDuration.ofSeconds( seconds.widen.toValidLong ) )
  }

  def setTTL[S : EthSigner.Source]( signer : S, name : String, ttl : Long, forceNonce : Option[BigInt] = None ) : Future[TransactionInfo.Async] = onlyOwner( signer, name ) {
    nameService.txn.setTTL( stubnamehash( name ), sol.UInt64( ttl ), nonce = toStubNonce( forceNonce ) )( Sender.Basic( ethsigner(signer) ) )
  }

  private def _resolver( stubnodehash : sol.Bytes32 ) : Future[Option[EthAddress]] = {
    val fAddr = nameService.view.resolver( stubnodehash )( Sender.Default )
    fAddr.map( addr => if ( addr == EthAddress.Zero ) None else Some( addr ) )
  }

  def resolver( path : String ) : Future[Option[EthAddress]] = _resolver( stubnamehash( path ) )

  def setResolver[S : EthSigner.Source, T : EthAddress.Source]( signer : S, path : String, resolver : T, forceNonce : Option[BigInt] = None ) : Future[TransactionInfo.Async] = onlyOwner( signer, path ) {
    nameService.txn.setResolver( stubnamehash( path ), ethaddress( resolver ), nonce = toStubNonce( forceNonce ) )( Sender.Basic( ethsigner(signer) ) )
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
      resolver.txn.setAddr( stubnodehash, _address, nonce = toStubNonce( forceNonce ) )( Sender.Basic( ethsigner(signer) ) )
    }
  }

  def rentPriceInWei( name : String, durationInSeconds : BigInt ) : Future[BigInt] = {
    requireSimpleName( name )
    for {
      topLevelController <- ftopLevelController
      stubWei            <- topLevelController.view.rentPrice( name, sol.UInt( durationInSeconds ) )( Sender.Default )
    }
    yield {
      stubWei.widen
    }
  }

  def isValid( name : String ) : Future[Boolean] = {
    requireSimpleName( name )
    for {
      topLevelController <- ftopLevelController
      stubBool           <- topLevelController.view.valid( name )( Sender.Default )
    }
    yield {
      stubBool
    }
  }

  def isAvailable( name : String ) : Future[Boolean] = {
    requireSimpleName( name )
    for {
      topLevelController <- ftopLevelController
      stubBool           <- topLevelController.view.available( name )( Sender.Default )
    }
    yield {
      stubBool
    }
  }

  private def requireSimpleName( name : String ) = {
    require( name.indexOf('.') < 0, s"A simple name (not a path, no internal periods) is required, but found '${name}'." )
  }
}

