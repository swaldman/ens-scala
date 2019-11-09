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
import scala.concurrent.{Await,ExecutionContext,Future}
import scala.concurrent.duration._

import com.mchange.sc.v1.consuela._
import com.mchange.sc.v1.consuela.ethereum.{EthAddress,EthChainId,EthHash,EthPrivateKey,EthSigner,wallet}
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
    chainId             : Option[EthChainId]       = stub.Context.Default.ChainId,
    nameServiceAddress  : EthAddress               = StandardNameServiceAddress,
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
    new Client( nameServiceAddress, executionTimeout )( scontext )
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
      eventConfirmations  : Int                      = stub.Context.Default.EventConfirmations,
      executionTimeout    : Duration                 = Client.DefaultExecutionTimeout
    )( implicit
      efactory  : Exchanger.Factory       = stub.Context.Default.ExchangerFactory,
      poller    : Poller                  = stub.Context.Default.Poller,
      scheduler : Scheduler               = stub.Context.Default.Scheduler,
      econtext  : ExecutionContext        = stub.Context.Default.ExecutionContext
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
      new Client( nameServiceAddress, executionTimeout )( scontext )
    }
  }
}
class Client(
  val nameServiceAddress : EthAddress = StandardNameServiceAddress,
  val executionTimeout   : Duration   = Client.DefaultExecutionTimeout
)( implicit scontext : stub.Context ) {

  private val inner : AsyncClient = new AsyncClient( nameServiceAddress )( scontext )

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

  def multicoinAddress( name : String, slip44CoinIndex : Int ) : Option[immutable.Seq[Byte]] = await( inner.multicoinAddress( name, slip44CoinIndex ) )

  def setMulticoinAddress[S : EthSigner.Source]( signer : S, name : String, slip44CoinIndex : Int, address : immutable.Seq[Byte], forceNonce : Option[BigInt] = None ) : TransactionInfo = {
    awaitTransactionInfo( inner.setMulticoinAddress( signer, name, slip44CoinIndex, address, forceNonce = forceNonce ) )
  }

  

  final object forTopLevelDomain {
    // MT: access controlled by this' lock
    private val tldToController : mutable.Map[String,RegistrarManagedDomain] = mutable.Map.empty

    def apply( tld : String ) : RegistrarManagedDomain = this.synchronized {
      tldToController.getOrElseUpdate( tld, RegistrarManagedDomain(tld) )
    }
  }

  final object RegistrarManagedDomain {
    def apply( domain : String ) : RegistrarManagedDomain =  new RegistrarManagedDomain( inner.RegistrarManagedDomain( domain ) )
  }
  final class RegistrarManagedDomain( _inner : inner.RegistrarManagedDomain ) {

    def domain : String = _inner.domain

    def maybeDomainRegistrarAddress : Either[Throwable,EthAddress] = await( _inner.maybeDomainRegistrarAddress )

    def hasValidRegistrar : Boolean = await( _inner.hasValidRegistrar )

    def minCommitmentAgeInSeconds : BigInt = await( _inner.minCommitmentAgeInSeconds )

    def maxCommitmentAgeInSeconds : BigInt = await( _inner.maxCommitmentAgeInSeconds )

    def minRegistrationDurationInSeconds : BigInt = await( _inner.minRegistrationDurationInSeconds )

    def rentPriceInWei( name : String, durationInSeconds : BigInt ) : BigInt = await( _inner.rentPriceInWei( name, durationInSeconds ) )

    def isValid( name : String ) : Boolean = await( _inner.isValid( name ) )

    def isAvailable( name : String ) : Boolean = await( _inner.isAvailable( name ) )

    def nameExpires( name : String ) : Option[Instant] = await( _inner.nameExpires( name ) )

    def makeCommitment[T : EthAddress.Source]( name : String, owner : T ) : Commitment = await( _inner.makeCommitment( name, owner ) )

    def commit[S : EthSigner.Source]( signer : S, commitment : Commitment, forceNonce : Option[BigInt] = None ) : TransactionInfo = awaitTransactionInfo( _inner.commit( signer, commitment, forceNonce ) )

    def register[S : EthSigner.Source, T : EthAddress.Source](
      signer : S,
      name : String,
      owner : T,
      durationInSeconds : BigInt,
      commitment : Commitment,
      paymentInWei : BigInt,
      forceNonce : Option[BigInt]
    ) : TransactionInfo = {
      awaitTransactionInfo( _inner.register( signer, name, owner, durationInSeconds, commitment, paymentInWei, forceNonce ) )
    }

    def register[S : EthSigner.Source, T : EthAddress.Source](
      signer : S,
      name : String,
      owner : T,
      durationInSeconds : BigInt,
      commitment : Commitment,
      paymentInWei : BigInt
    ) : TransactionInfo = {
      awaitTransactionInfo( _inner.register( signer, name, owner, durationInSeconds, commitment, paymentInWei ) )
    }

    def register[S : EthSigner.Source, T : EthAddress.Source](
      signer : S,
      name : String,
      owner : T,
      durationInSeconds : BigInt,
      secret : Seq[Byte],
      paymentInWei : BigInt,
      forceNonce : Option[BigInt]
    ) : TransactionInfo = {
      awaitTransactionInfo( _inner.register( signer, name, owner, durationInSeconds, secret, paymentInWei, forceNonce ) )
    }

    def register[S : EthSigner.Source, T : EthAddress.Source](
      signer : S,
      name : String,
      owner : T,
      durationInSeconds : BigInt,
      secret : Seq[Byte],
      paymentInWei : BigInt
    ) : TransactionInfo = {
      awaitTransactionInfo( _inner.register( signer, name, owner, durationInSeconds, secret, paymentInWei ) )
    }

    def renew[S : EthSigner.Source]( signer : S, name : String, durationInSeconds : BigInt, paymentInWei : BigInt, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
      awaitTransactionInfo( _inner.renew( signer, name, durationInSeconds, paymentInWei, forceNonce ) )
    }

    def migrateFromPredecessor[S : EthSigner.Source]( signer : S, name : String, forceNonce : Option[BigInt] = None ) : TransactionInfo = {
      awaitTransactionInfo( _inner.migrateFromPredecessor( signer, name, forceNonce ) )
    }
  }
}



