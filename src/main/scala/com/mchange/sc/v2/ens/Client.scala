package com.mchange.sc.v2.ens

import contract._

import scala.concurrent.ExecutionContext
import com.mchange.sc.v1.consuela.ethereum.EthAddress
import com.mchange.sc.v1.consuela.ethereum.jsonrpc20.Invoker

import com.mchange.sc.v1.consuela.ethereum.ethabi.stub
import stub.sol
import stub.Sender

object Client {
  def apply( ethJsonRpcUrl : String )( implicit econtext : ExecutionContext ) = new Client()( Invoker.Context( ethJsonRpcUrl ), econtext )
}
class Client( nameServiceAddress : EthAddress = StandardNameServiceAddress )( implicit icontext : Invoker.Context, econtext : ExecutionContext ) extends stub.Utilities {
  lazy val nameService = ENS( nameServiceAddress )

  private def stubnamehash( name : String ) : sol.Bytes32 = sol.Bytes32( namehash( name ).bytes )

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

  lazy val registrar : Registrar = new Registrar( owner( "eth" ).get ) // we assert that it exists

  lazy val reverseRegistrar : ReverseRegistrar = new ReverseRegistrar( owner( "addr.reverse" ).get ) // we assert that it exists
}
